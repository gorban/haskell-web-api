#!/usr/bin/env bash

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
echo "    if (data.type === 'hpc-nav' && typeof data.href !== 'string') {" >> hpc_index.html
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
find . -name "*.tix" -type f -print0 | xargs -0 rm -f --
cabal clean
cabal configure --disable-backup
cabal test all --enable-coverage --test-show-details=direct --test-options=+RTS --test-options=--read-tix-file=no --test-options=-RTS --test-options=--match --test-options=Unit
repoRoot=$(pwd)
tempdir="$(mktemp -d)"
missing_coverage=false
while IFS= read -r -d '' report; do
  echo "Using temporary directory $tempdir for HPC mix files."
  mixdir="$(dirname "$(dirname "$(dirname "$(dirname "$(dirname "$(dirname "$(dirname "$report")")")")")")")"
  find "$mixdir" -name mix ! -path "$mixdir/opt/build/extra-compilation-artifacts/hpc/dyn/mix" -type d -exec cp -r {}/. "$tempdir/" \;
done < <(find dist-newstyle -name hpc_index.html -type f -print0)
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
      var height = table ? table.scrollHeight : (document.documentElement.scrollHeight || document.body.scrollHeight);
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
    window.parent.postMessage({ type: "hpc-nav", href: resolved }, "*");
  } else {
    window.location.assign(resolved);
  }
}, true);
</script>
</body>
SCRIPT
  )
  sed -i "1,/<\/body>/s@</body>@$snippet@" "$report"
  while IFS= read -r -d '' tixfile; do
    echo -e "\n\e[34m$(basename "$tixfile") coverage report:\e[0m"
    hpc report --hpcdir "$tempdir" "$tixfile" | tee /dev/fd/2 | grep -v -qE '^\s*100%$' && {
      echo -e "\e[31mCoverage report for $(basename "$tixfile") contains less than 100% coverage.\e[0m" >&2
      missing_coverage=true
    }
  done < <(find "$(dirname "$(dirname "$report")")" -name "*.tix" -type f -print0)
done < <(find dist-newstyle -name hpc_index.html -type f -print0 | sort -z)
rm -rf "$tempdir"
echo "</body></html>" >> hpc_index.html
printf '\n\e[32mMulti-package coverage report generated at %s/hpc_index.html\e[0m\n' "$(pwd)"
if $missing_coverage; then
  echo -e "\n\e[31mCoverage report contains less than 100% coverage, exiting with error.\e[0m"
  exit 1
fi
exit
# --- END BASH SECTION ---

# The line below ends the PowerShell multi-line comment and suppresses output
#> | Out-Null

# --- POWERSHELL SECTION ---
$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest
$PSNativeCommandUseErrorActionPreference = $true
ls -r **\*.tix | rm -Force
cabal clean
cabal configure --disable-backup
cabal test all --enable-coverage --test-show-details=direct --test-options=+RTS --test-options=--read-tix-file=no --test-options=-RTS --test-options=--match --test-options=Unit
$CurrentPath = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath('.\')
$TempDir = Join-Path ([System.IO.Path]::GetTempPath()) ([System.IO.Path]::GetRandomFileName())
ls -r dist-newstyle\**\hpc_index.html | sort FullName -Descending | % {
  ls -r "$($_.Directory.Parent.Parent.Parent.Parent.Parent.Parent)\**\mix" | ls | % { cp -r $_ "$TempDir\$($_.Name)" }
}
ls -r dist-newstyle\**\hpc_index.html | sort FullName -Descending | % {
  echo "<iframe src='$([System.IO.Path]::GetRelativePath($CurrentPath, $_.FullName))'></iframe><br/>" >> hpc_index.html
  Set-Content $($(gc -Raw $_) -replace '(?i)</body>', @"
<script>
  function scheduleHeight() {
    setTimeout(function () {
      var table = document.querySelector('body > *');
      var height = table ? table.scrollHeight : (document.documentElement.scrollHeight || document.body.scrollHeight);
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
"@) -LiteralPath $_.FullName
  ls "$($_.Directory.Parent)\tix" | % {
    Write-Host "`n$($_.BaseName) coverage report:" -F DarkGray
    hpc report --hpcdir $TempDir $_.FullName | tee -V HpcReport
    # Write error if HpcReport contains any HpcReport lines do not start with optional whitespace followed by exactly 100%
    if ($HpcReport | ? { $_ -notmatch '^\s*100%$' }) {
      [Console]::ForegroundColor = 'Red'
      [Console]::Error.WriteLine("Coverage report for $($_.BaseName) contains less than 100% coverage.")
      [Console]::ResetColor()
      $script:MissingCoverage = $true
    }
  }
}
rm -r -Force $TempDir
echo "</body></html>" >> hpc_index.html
$CombinedPath = Join-Path $CurrentPath 'hpc_index.html'
Write-Host "`nMulti-package coverage report generated at $CombinedPath" -F Green
start $(([System.Uri]$CombinedPath).AbsoluteUri)
if ($script:MissingCoverage) {
  Write-Host "`nCoverage report contains less than 100% coverage, exiting with error." -F Red
  exit 1
}
# --- END POWERSHELL SECTION ---
