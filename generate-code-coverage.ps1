#!/usr/bin/env bash

cabal clean

# TODO: this is a workaround for an issue that appeared when we switched from
#       build-type: Simple to build-type: Custom in various packages. Addresses error:
#       Error: [Cabal-5678]
#       Could not find test program "<repo-root>\dist-newstyle\build\<arch>\ghc-<version>\
#         <package>\opt\build\<package>-tests\<package>-tests.exe".
#         Did you build the package first?
cabal build all

echo "<html><head><title>haskell-web-api Coverage Reports</title><style>" > hpc_index.html
echo "  iframe { width: 100%; border: none; }" >> hpc_index.html
echo "</style></head><body>" >> hpc_index.html
echo "<h1>haskell-web-api Coverage Reports</h1>" >> hpc_index.html
echo "<script>" >> hpc_index.html
echo "  window.addEventListener('message', function (event) {" >> hpc_index.html
echo "    var data = event && event.data;" >> hpc_index.html
echo "    if (!data) return;" >> hpc_index.html
echo "    // Ensure the message came from one of our iframes." >> hpc_index.html
echo "    var iframes = document.querySelectorAll('iframe');" >> hpc_index.html
echo "    var fromKnownIframe = false;" >> hpc_index.html
echo "    for (var i = 0; i < iframes.length; i++) {" >> hpc_index.html
echo "      if (event.source === iframes[i].contentWindow) {" >> hpc_index.html
echo "        fromKnownIframe = true;" >> hpc_index.html
echo "        break;" >> hpc_index.html
echo "      }" >> hpc_index.html
echo "    }" >> hpc_index.html
echo "    if (!fromKnownIframe) return;" >> hpc_index.html
echo "    if (data.type === 'hpc-nav' && typeof data.href === 'string') {" >> hpc_index.html
echo "      // Basic href sanity check then navigate." >> hpc_index.html
echo "      if (!/^file:/i.test(data.href)) return;" >> hpc_index.html
echo "      window.location.assign(data.href);" >> hpc_index.html
echo "    } else if (data.type === 'hpc-height' && typeof data.height === 'number') {" >> hpc_index.html
echo "      // Adjust iframe height." >> hpc_index.html
echo "      for (var j = 0; j < iframes.length; j++) {" >> hpc_index.html
echo "        var iframe = iframes[j];" >> hpc_index.html
echo "        if (event.source === iframe.contentWindow) {" >> hpc_index.html
echo "          iframe.style.height = (Math.ceil(data.height) + 32) + 'px';" >> hpc_index.html
echo "          break;" >> hpc_index.html
echo "        }" >> hpc_index.html
echo "      }" >> hpc_index.html
echo "    }" >> hpc_index.html
echo "  });" >> hpc_index.html
echo "</script>" >> hpc_index.html

# The line below is seen as a comment by Bash but starts a multi-line comment for PowerShell
echo `# <#` >/dev/null

# --- BASH SECTION ---
set -euo pipefail

array_contains() {
  local needle="$1"
  shift
  local element
  for element in "$@"; do
    if [ "$element" = "$needle" ]; then
      return 0
    fi
  done
  return 1
}

# Remove stale HPC coverage artifacts that can confuse new runs.
find . -name "*.tix" -type f -delete
find . -maxdepth 1 -name "cabal.project.local.backup.*" -type f -delete

# Parse cabal.project to get packages and run tests for each separately.
# This works around Cabal issue where "cabal test all --enable-coverage" only
# generates coverage for the last package. See: https://github.com/haskell/cabal/issues/7200

# Extract package directories from cabal.project, then read actual package names from .cabal files
all_packages=""
while IFS= read -r pkgdir; do
  pkgdir="${pkgdir//$'\r'/}"                    # strip Windows CR
  pkgdir="${pkgdir#"${pkgdir%%[![:space:]]*}"}" # trim leading whitespace
  pkgdir="${pkgdir%/}"                          # trim trailing slash
  cabal_file=$(find "$pkgdir" -maxdepth 1 -name "*.cabal" -type f 2>/dev/null | head -n1)
  if [ -n "$cabal_file" ]; then
    pkg_name=$(grep -m1 '^name:' "$cabal_file" | sed 's/^name:[[:space:]]*//;s/\r$//')
    all_packages="$all_packages $pkg_name"
  fi
done < <(grep -E '^\s+\S+/' cabal.project | tr -d '\r')

# Find packages with coverage: False (look for "package <name>" followed by "coverage: False")
excluded_packages=$(awk '
  function trim_cr(val) { sub(/\r$/, "", val); return val }
  /^package[[:space:]]+/ {
    pkg = trim_cr($2)
    next
  }
  /coverage:[[:space:]]*False/ && pkg {
    print pkg
    pkg = ""
  }
' cabal.project)

temp_root="$(mktemp -d 2>/dev/null || mktemp -d -t hpc.XXXXXX || printf '%s/.hpc' "$(pwd)")"
if [ ! -d "$temp_root" ]; then
  rm -rf "$temp_root"
  mkdir -p "$temp_root"
fi
coverage_staging_dir="$temp_root/hpc"
mix_cache_dir="$temp_root/mix"
hpc_work_dir="$temp_root/hpc-work"
mkdir -p "$coverage_staging_dir" "$mix_cache_dir" "$hpc_work_dir"

project_local_path="cabal.project.local"
project_local_backup=""
if [ -f "$project_local_path" ]; then
  project_local_backup="$(mktemp "$temp_root/cabal.project.local.backup.XXXXXX")"
  cp "$project_local_path" "$project_local_backup"
  rm -f "$project_local_path"
fi

# Run tests for each package not in the excluded list
for pkg in $all_packages; do
  if ! printf '%s\n' "$excluded_packages" | grep -qxF "$pkg"; then
    rm -f "$project_local_path"
    if [ -n "$project_local_backup" ]; then
      cp "$project_local_backup" "$project_local_path"
    else
      : > "$project_local_path"
    fi
    if [ -s "$project_local_path" ]; then
      printf '\n' >> "$project_local_path"
    fi
    for candidate in $all_packages; do
      coverage_value="False"
      if [ "$candidate" = "$pkg" ]; then
        coverage_value="True"
      fi
      if printf '%s\n' "$excluded_packages" | grep -qxF "$candidate"; then
        coverage_value="False"
      fi
      cat <<EOF >> "$project_local_path"
package $candidate
  coverage: $coverage_value

EOF
    done

    # Use -O0 to disable optimization for accurate coverage (prevents inlining)
    cabal configure --disable-backup --ghc-options=-O0

    # Clean build artifacts to avoid stale tix data that can bleed between runs.
    find dist-newstyle -name "*.tix" -type f -print0 | xargs -0 rm -f --

    printf '\n\033[36mRunning tests with coverage for: %s\033[0m\n' "$pkg"
    cabal test "$pkg" --enable-coverage --test-show-details=direct --test-options="+RTS --read-tix-file=no -RTS --match Unit"

    pkg_hpc_dir=$(find dist-newstyle -path "*/$pkg-*/opt/hpc/vanilla" -type d -print | head -n1)
    if [ -n "$pkg_hpc_dir" ]; then
      dest="$coverage_staging_dir/$pkg"
      rm -rf "$dest"
      mkdir -p "$dest"
      cp -r "$pkg_hpc_dir"/. "$dest"/
    fi
  else
    printf '\n\033[33mSkipping coverage for: %s (coverage: False in cabal.project)\033[0m\n' "$pkg"
  fi
done

while IFS= read -r -d '' staged_pkg; do
  package_name="$(basename "$staged_pkg")"
  target_dir=$(find dist-newstyle -path "*/$package_name-*/opt/hpc/vanilla" -type d -print | head -n1)
  if [ -n "$target_dir" ]; then
    rm -rf "$target_dir"
    mkdir -p "$target_dir"
    cp -r "$staged_pkg"/. "$target_dir"/
  fi
done < <(find "$coverage_staging_dir" -mindepth 1 -maxdepth 1 -type d -print0)
repoRoot=$(pwd)
repo_hpc_dir="$repoRoot/.hpc"
if [ -d "$repo_hpc_dir" ]; then
  rm -rf "$repo_hpc_dir"
fi
missing_coverage=false
aggregate_issue=false
copied_mix=false
declare -a hpc_search_dirs=()
declare -a per_project_findings=()
declare -a aggregate_findings=()
declare -a aggregate_tix_paths=()
while IFS= read -r -d '' mixdir; do
  \cp -Rf "$mixdir"/. "$mix_cache_dir"/
  if ! array_contains "$mixdir" "${hpc_search_dirs[@]}"; then
    hpc_search_dirs+=("$mixdir")
  fi
  copied_mix=true
done < <(find dist-newstyle -type d -name mix -print0)
if [ -d "$hpc_work_dir" ] && find "$hpc_work_dir" -mindepth 1 -print -quit >/dev/null 2>&1; then
  \cp -Rf "$hpc_work_dir"/. "$mix_cache_dir"/
  while IFS= read -r -d '' extra_mix; do
    if ! array_contains "$extra_mix" "${hpc_search_dirs[@]}"; then
      hpc_search_dirs+=("$extra_mix")
    fi
    copied_mix=true
  done < <(find "$hpc_work_dir" -type d -name mix -print0)
fi
if $copied_mix; then
  mix_dir_count=${#hpc_search_dirs[@]}
  echo "Collected HPC mix files into $mix_cache_dir ($mix_dir_count directories)."
fi
while IFS= read -r -d '' report; do
  echo "<iframe src='${report#$repoRoot/}'></iframe><br/>" >> hpc_index.html
  snippet=$(
    sed -e ':a' -e 'N' -e '$!ba' \
      -e 's/[\/&\\]/\\&/g' \
      -e 's/"/\\"/g' \
      -e 's/\n/\\n/g' <<'SCRIPT'
<script>
  function scheduleHeight() {
    setTimeout(function () {
      var table = document.querySelector('body > *');
      var height = table
        ? table.scrollHeight
        : (document.documentElement.scrollHeight || document.body.scrollHeight);
      if (window.parent && window.parent !== window) {
        window.parent.postMessage({ type: 'hpc-height', height: height }, '*');
      }
    }, 0);
  }
  if (document.readyState === 'complete' || document.readyState === 'interactive') {
    scheduleHeight();
  } else {
    document.addEventListener('DOMContentLoaded', scheduleHeight);
  }
  window.addEventListener('load', scheduleHeight);
  window.addEventListener('resize', scheduleHeight);
  document.addEventListener('click', function (e) {
  var el = e.target;
  var a = el && el.closest ? el.closest('a[href]') : null;
  if (!a) return;

  var href = a.getAttribute('href');
  if (!href || href[0] === '#' || /^\s*javascript:/i.test(href)) return;

  e.preventDefault();
  e.stopPropagation();

  var resolved = new URL(href, window.location.href).href;
  if (window.parent && window.parent !== window) {
    window.parent.postMessage({ type: 'hpc-nav', href: resolved }, '*');
  } else {
    window.location.assign(resolved);
  }
}, true);
</script>
</body>
SCRIPT
  )
  sed -i "1,/<\/body>/s@</body>@$snippet@" "$report"
  tix_root="$(dirname "$(dirname "$report")")/tix"
  pkg_version_dir="$(basename "$(dirname "$(dirname "$(dirname "$(dirname "$(dirname "$report")")")")")")"
  package_name="${pkg_version_dir%%-[0-9]*}"
  if [ -d "$tix_root" ]; then
    while IFS= read -r -d '' tixfile; do
      if ! array_contains "$tixfile" "${aggregate_tix_paths[@]}"; then
        aggregate_tix_paths+=("$tixfile")
      fi
    done < <(find "$tix_root" -name "*.tix" -type f -print0)
  fi

  fractions=()
  while IFS= read -r fraction_line; do
    fractions+=("$fraction_line")
  done < <(
    awk 'BEGIN{IGNORECASE=1}
      /Program Coverage Total/ { capture=1; next }
      capture {
        while (match($0, /([0-9]+)[[:space:]]*\/[[:space:]]*([0-9]+)/, arr)) {
          printf "%s/%s\n", arr[1], arr[2]
          $0 = substr($0, RSTART + RLENGTH)
        }
        if (index($0, "</tr>") > 0) {
          exit
        }
      }
    ' "$report"
  )

  categories=(
    "Top Level Definitions"
    "Alternatives"
    "Expressions"
  )
  for idx in "${!categories[@]}"; do
    fraction="${fractions[$idx]:-}"
    if [ -z "$fraction" ]; then
      continue
    fi
    cleaned_fraction="${fraction//[[:space:]]/}"
    IFS='/' read -r covered total <<<"$cleaned_fraction"
    if [ -z "$covered" ] || [ -z "$total" ]; then
      continue
    fi
    if [ "$total" != "0" ] && [ "$covered" != "$total" ]; then
      per_project_findings+=("${categories[$idx]} coverage for $package_name ($covered/$total).")
      missing_coverage=true
    fi
  done
done < <(find dist-newstyle -name hpc_index.html -type f -print0 | sort -z)
aggregate_report_output=""
aggregate_tix_to_report=""
if [ "${#aggregate_tix_paths[@]}" -gt 0 ]; then
  if [ "${#aggregate_tix_paths[@]}" -gt 1 ]; then
    aggregate_tix_to_report="$temp_root/all-packages.tix"
    rm -f "$aggregate_tix_to_report"
    hpc sum --union --output="$aggregate_tix_to_report" "${aggregate_tix_paths[@]}"
  else
    aggregate_tix_to_report="${aggregate_tix_paths[0]}"
  fi

  report_args=()
  for search_dir in "${hpc_search_dirs[@]}"; do
    report_args+=("--hpcdir" "$search_dir")
  done

  echo -e "\n\033[90mFull coverage report (all packages):\033[0m"
  if aggregate_report_output=$(hpc report "${report_args[@]}" "$aggregate_tix_to_report" 2>&1); then
    printf '%s\n' "$aggregate_report_output"
    while IFS= read -r line; do
      if awk 'match($0, /^[[:space:]]*([0-9]+(\.[0-9]+)?)%/, m) { if ((m[1] + 0) < 100) { exit 0 } else { exit 1 } } { exit 1 }' <<<"$line"; then
        trimmed_line=$(printf '%s\n' "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        aggregate_findings+=("$trimmed_line")
        aggregate_issue=true
        missing_coverage=true
      fi
    done < <(printf '%s\n' "$aggregate_report_output")
  else
    printf '\033[31mFailed to generate aggregate coverage report.\033[0m\n' >&2
    printf '%s\n' "$aggregate_report_output" >&2
    missing_coverage=true
  fi

  if [ "${#aggregate_tix_paths[@]}" -gt 1 ] && [ -n "$aggregate_tix_to_report" ]; then
    rm -f "$aggregate_tix_to_report"
  fi
fi
rm -rf "$temp_root"
echo "</body></html>" >> hpc_index.html
printf '\n\e[32mMulti-package coverage report generated at %s/hpc_index.html\e[0m\n' "$(pwd)"

if [ "${#per_project_findings[@]}" -gt 0 ]; then
  echo
  printf '\033[31mPer-project reports found with <100%% coverage, exiting with error:\033[0m\n'
  for finding in "${per_project_findings[@]}"; do
    printf '\033[31m- %s\033[0m\n' "$finding"
  done
elif $aggregate_issue; then
  echo
  printf '\033[31mCoverage report contains less than 100%% coverage, exiting with error.\033[0m\n'
  for line in "${aggregate_findings[@]}"; do
    printf '\033[31m- %s\033[0m\n' "$line"
  done
fi

if $missing_coverage; then
  exit 1
fi
exit
# --- END BASH SECTION ---

# The line below ends the PowerShell multi-line comment and suppresses output
#> > $null

# --- POWERSHELL SECTION ---
$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest
$PSNativeCommandUseErrorActionPreference = $true
$script:MissingCoverage = $false
$PerProjectCoverageFindings = [System.Collections.Generic.List[string]]::new()
$AggregateCoverageFindings = [System.Collections.Generic.List[string]]::new()
$AggregateCoverageIssue = $false
ls -r **\*.tix | rm -Force
Get-ChildItem -Filter "cabal.project.local.backup.*" -File -ErrorAction SilentlyContinue |
  Remove-Item -Force

$RepoHpcDir = Join-Path (pwd) ".hpc"
if (Test-Path $RepoHpcDir) {
  rm -LiteralPath $RepoHpcDir -Recurse -Force
}

$DistRoot = Join-Path (pwd) "dist-newstyle"

# Parse cabal.project to get packages and run tests for each separately.
# This works around Cabal issue where "cabal test all --enable-coverage" only
# generates coverage for the last package. See: https://github.com/haskell/cabal/issues/7200

$CabalProjectContent = gc cabal.project
$CabalProjectRaw = gc -Raw cabal.project

# Extract package directories from cabal.project, then read actual package names from .cabal files
$AllPackages = @()
foreach ($line in $CabalProjectContent) {
  if ($line -match "^\s+(\S+)/?$") {
    $pkgDir = $Matches[1].TrimEnd("/")
    $cabalFile = ls "$pkgDir/*.cabal" -EA 0 | select -f 1
    if ($cabalFile) {
      $cabalContent = gc $cabalFile.FullName
      foreach ($line in $cabalContent) {
        if ($line -match "^name:\s*(\S+)") { $AllPackages += $Matches[1] }
      }
    }
  }
}

# Find packages with coverage: False (look for "package <name>" followed by "coverage: False")
$ExcludedPackages = [regex]::Matches(
  $CabalProjectRaw,
  "(?ms)^package\s+(\S+)\s*$.*?coverage:\s*False") |
  % { $_.Groups[1].Value }


$TempRoot = Join-Path ([System.IO.Path]::GetTempPath()) ([System.IO.Path]::GetRandomFileName())
ni -it D $TempRoot -f > $null
$CoverageRoot = Join-Path $TempRoot "hpc"
ni -it D $CoverageRoot -f > $null
$TempMixDir = Join-Path $TempRoot "mix"
ni -it D $TempMixDir -f > $null
$TempWorkDir = Join-Path $TempRoot "hpc-work"
ni -it D $TempWorkDir -f > $null
$HpcSearchDirs = @()

$ProjectLocalPath = Join-Path (pwd) "cabal.project.local"
$ExistingProjectLocal = if (Test-Path $ProjectLocalPath) {
  gc -Raw -LiteralPath $ProjectLocalPath
  rm -LiteralPath $ProjectLocalPath -Force
} else {
  $null
}
$BaseProjectLocal = $ExistingProjectLocal
if ($BaseProjectLocal -and -not $BaseProjectLocal.EndsWith("`n")) {
  $BaseProjectLocal += "`n"
}

# Run tests for each package not in the excluded list
foreach ($pkg in $AllPackages) {
  if ($pkg -notin $ExcludedPackages) {
    if (Test-Path $ProjectLocalPath) {
      rm -LiteralPath $ProjectLocalPath -Force
    }
    if ($null -ne $BaseProjectLocal) {
      [System.IO.File]::WriteAllText($ProjectLocalPath, $BaseProjectLocal)
    } else {
      ni -it F $ProjectLocalPath -f > $null
      clc -LiteralPath $ProjectLocalPath
    }
    $CoverageEntries = foreach ($candidate in $AllPackages) {
      $coverageValue = "False"
      if ($candidate -eq $pkg) {
        if ($ExcludedPackages -notcontains $candidate) {
          $coverageValue = "True"
        }
      }
      "package $candidate`n  coverage: $coverageValue`n"
    }
    ac -LiteralPath $ProjectLocalPath $CoverageEntries

    # Use -O0 to disable optimization for accurate coverage (prevents inlining)
    cabal configure --disable-backup --ghc-options=-O0

    # Clean build artifacts to avoid coverage data for dependent packages that will be
    # tested by their own iterations of this loop.
    ls -r dist-newstyle\**\*.tix | rm -Force
    ls -r dist-newstyle\**\mix\* | rm -Force

    Write-Host "`nRunning tests with coverage for: $pkg" -F Blue
    cabal test $pkg --enable-coverage --test-show-details=direct --test-options="+RTS --read-tix-file=no -RTS --match Unit"

    $PackageHpcDir = ls $DistRoot -Directory -r |
      ? { $_.FullName -match "\\$pkg-[^\\]+\\opt\\hpc\\vanilla$" } |
      select -f 1
    if ($PackageHpcDir) {
      $Destination = Join-Path $CoverageRoot $pkg
      if (Test-Path $Destination) {
        rm -LiteralPath $Destination -r -Force
      }
      ni -it D $Destination -f > $null
      cp (Join-Path $PackageHpcDir.FullName "*") $Destination -r -Force
    }
  } else {
    Write-Host "`nSkipping coverage for: $pkg (coverage: False in cabal.project)" -F Yellow
  }
}

if ($null -ne $ExistingProjectLocal) {
  [System.IO.File]::WriteAllText($ProjectLocalPath, $ExistingProjectLocal)
} elseif (Test-Path $ProjectLocalPath) {
  rm -LiteralPath $ProjectLocalPath -Force
}

# Restore staged coverage artifacts into dist-newstyle before aggregation
ls $CoverageRoot -Directory | % {
  $PackageName = $_.Name
  $TargetDir = ls $DistRoot -Directory -r |
    ? { $_.FullName -match "\\$PackageName-[^\\]+\\opt\\hpc\\vanilla$" } |
    select -f 1
  if ($TargetDir) {
    cp (Join-Path $_.FullName "*") $TargetDir.FullName -r -Force
  }
}

$CurrentPath = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath(".\")
$MixDirs = ls $DistRoot -Directory -r | ? { $_.Name -eq "mix" }
if ($MixDirs) {
  foreach ($MixDir in $MixDirs) {
    $SourcePattern = Join-Path $MixDir.FullName '*'
    if (Test-Path $SourcePattern) {
      cp $SourcePattern $TempMixDir -Recurse -Force
    }
    if ($HpcSearchDirs -notcontains $MixDir.FullName) {
      $HpcSearchDirs += $MixDir.FullName
    }
  }
}
$WorkMixDirs = ls $TempWorkDir -Directory -r | ? { $_.Name -eq "mix" }
foreach ($WorkMixDir in $WorkMixDirs) {
  $SourcePattern = Join-Path $WorkMixDir.FullName '*'
  if (Test-Path $SourcePattern) {
    cp $SourcePattern $TempMixDir -Recurse -Force
  }
  if ($HpcSearchDirs -notcontains $WorkMixDir.FullName) {
    $HpcSearchDirs += $WorkMixDir.FullName
  }
}
if ($HpcSearchDirs.Count -gt 0) {
  Write-Host "Collected HPC mix files into $TempMixDir ($($HpcSearchDirs.Count) directories)."
}

$iframeInjection = @"
<script>
  function scheduleHeight() {
    setTimeout(function () {
      var table = document.querySelector('body > *');
      var height = table
        ? table.scrollHeight
        : (document.documentElement.scrollHeight || document.body.scrollHeight);
      if (window.parent && window.parent !== window) {
        window.parent.postMessage({ type: 'hpc-height', height: height }, '*');
      }
    }, 0);
  }
  if (document.readyState === 'complete' || document.readyState === 'interactive') {
    scheduleHeight();
  } else {
    document.addEventListener('DOMContentLoaded', scheduleHeight);
  }
  window.addEventListener('load', scheduleHeight);
  window.addEventListener('resize', scheduleHeight);
  document.addEventListener('click', function (e) {
    var el = e.target;
    var a = el && el.closest ? el.closest('a[href]') : null;
    if (!a) return;

    var href = a.getAttribute('href');
    if (!href || href[0] === '#' || /^\s*javascript:/i.test(href)) return;

    e.preventDefault();
    e.stopPropagation();

    var resolved = new URL(href, window.location.href).href;
    if (window.parent && window.parent !== window) {
      window.parent.postMessage({ type: 'hpc-nav', href: resolved }, '*');
    } else {
      window.location.assign(resolved);
    }
  }, true);
</script>
</body>
"@

$HtmlReports = ls -r $DistRoot -Filter hpc_index.html | sort FullName -d
$ProcessedPackages =
  [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase)
$AggregateTixPaths =
  [System.Collections.Generic.HashSet[string]]::new(
    [System.StringComparer]::OrdinalIgnoreCase)

foreach ($report in $HtmlReports) {
  $relativePath = [System.IO.Path]::GetRelativePath($CurrentPath, $report.FullName)
  echo "<iframe src='$relativePath'></iframe><br/>" >> hpc_index.html

  $htmlContent = Get-Content -Raw -LiteralPath $report.FullName
  $updatedContent = $htmlContent -replace '(?i)</body>', $iframeInjection
  Set-Content -LiteralPath $report.FullName -Value $updatedContent

  $PackageRoot = $report.Directory.Parent
  $TixRoot = Join-Path $PackageRoot "tix"
  $DiscoveredName = $report.Directory.Parent.Parent.Parent.Parent.Name
  $packageNameMatch = [regex]::Match($DiscoveredName, "^(.*?)(?:-[0-9].*)?$")
  $PackageName = if ($packageNameMatch.Success -and $packageNameMatch.Groups[1].Value) {
    $packageNameMatch.Groups[1].Value
  } else {
    $DiscoveredName
  }

  if (-not $ProcessedPackages.Add($PackageName)) {
    continue
  }

  $totalsIndex = $htmlContent.IndexOf(
    "Program Coverage Total",
    [System.StringComparison]::OrdinalIgnoreCase)

  if ($totalsIndex -ge 0) {
    $segmentLength = [Math]::Min(1000, $htmlContent.Length - $totalsIndex)
    $totalsSegment =
      $htmlContent.Substring($totalsIndex, $segmentLength)

    $Fractions = [regex]::Matches(
      $totalsSegment,
      "<td[^>]*>(\d+)\s*/\s*(\d+)</td>",
      [Text.RegularExpressions.RegexOptions]::IgnoreCase)
    $Categories = @("Top Level Definitions", "Alternatives", "Expressions")
    for ($i = 0; $i -lt [Math]::Min($Fractions.Count, $Categories.Count); $i++) {
      $covered = [int]$Fractions[$i].Groups[1].Value
      $total = [int]$Fractions[$i].Groups[2].Value
      if ($total -ne 0 -and $covered -ne $total) {
        [void]$PerProjectCoverageFindings.Add(
          "$($Categories[$i]) coverage for $PackageName ($covered/$total).")
        $script:MissingCoverage = $true
      }
    }
  }

  if (Test-Path $TixRoot) {
    foreach ($item in ls $TixRoot -Filter *.tix -File -Recurse -EA 0) {
      [void]$AggregateTixPaths.Add($item.FullName)
    }
  }
}

if ($AggregateTixPaths.Count -gt 0) {
  $AggregateTixList = [System.Linq.Enumerable]::ToArray($AggregateTixPaths)
  $TempCombinedTix = $null
  if ($AggregateTixList.Length -gt 1) {
    $TempCombinedTix =
      Join-Path ([System.IO.Path]::GetTempPath()) (([System.IO.Path]::GetRandomFileName()) + ".tix")
    $sumArgs = @("--union", "--output=$TempCombinedTix")
    $sumArgs += $AggregateTixList
    hpc sum @sumArgs
    $TixToReport = $TempCombinedTix
  } else {
    $TixToReport = $AggregateTixList[0]
  }

  Write-Host "`nFull coverage report (all packages):" -F DarkGray
  $HpcReport = @()
  try {
    $HpcArgs = @()
    foreach ($dir in $HpcSearchDirs) {
      $HpcArgs += "--hpcdir=$dir"
    }
    hpc report @HpcArgs $TixToReport | tee -v HpcReport
  } finally {
    if ($TempCombinedTix -and (Test-Path $TempCombinedTix)) {
      rm -LiteralPath $TempCombinedTix -Force
    }
  }

  $UnderCoveredLines = $HpcReport |
    Where-Object {
      $_ -match '^\s*(\d+(?:\.\d+)?)%' -and [double]$Matches[1] -lt 100
    }

  if ($UnderCoveredLines) {
    $AggregateCoverageIssue = $true
    foreach ($line in $UnderCoveredLines) {
      [void]$AggregateCoverageFindings.Add($line.Trim())
    }
    $script:MissingCoverage = $true
  }
}
if (Test-Path $TempRoot) {
  rm -LiteralPath $TempRoot -Recurse -Force
}
echo "</body></html>" >> hpc_index.html
$CombinedPath = Join-Path $CurrentPath "hpc_index.html"
Write-Host "`nMulti-package coverage report generated at $CombinedPath" -F Green
start $(([System.Uri]$CombinedPath).AbsoluteUri)

if ($PerProjectCoverageFindings.Count -gt 0) {
  Write-Host "`nPer-project reports found with <100% coverage, exiting with error:" -F Red
  foreach ($finding in $PerProjectCoverageFindings) {
    Write-Host "- $finding" -F Red
  }
  if ($AggregateCoverageIssue -and $AggregateCoverageFindings.Count -gt 0) {
    Write-Host "`nAll-packages coverage report also below 100% coverage:" -F Red
    foreach ($line in $AggregateCoverageFindings) {
      Write-Host "- $line" -F Red
    }
  }
} elseif ($AggregateCoverageIssue) {
  Write-Host "`nCoverage report contains less than 100% coverage, exiting with error." -F Red
}

if ($script:MissingCoverage) {
  exit 1
}
# --- END POWERSHELL SECTION ---
