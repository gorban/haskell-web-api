#!/usr/bin/env bash

set -euo pipefail

cabal clean

# TODO: this is a workaround for an issue that appeared when we switched from
#       build-type: Simple to build-type: Custom in various packages. Addresses error:
#       Error: [Cabal-5678]
#       Could not find test program "<repo-root>\dist-newstyle\build\<arch>\ghc-<version>\
#         <package>\opt\build\<package>-tests\<package>-tests.exe".
#         Did you build the package first?
cabal build all --jobs=1

cat > hpc_index.html <<'EOF'
<html><head><title>haskell-web-api Coverage Reports</title><style>
  iframe { width: 100%; border: none; }
</style></head><body>
<h1>haskell-web-api Coverage Reports</h1>
<script>
  window.addEventListener('message', function (event) {
    var data = event && event.data;
    if (!data) return;
    // Ensure the message came from one of our iframes.
    var iframes = document.querySelectorAll('iframe');
    var fromKnownIframe = false;
    for (var i = 0; i < iframes.length; i++) {
      if (event.source === iframes[i].contentWindow) {
        fromKnownIframe = true;
        break;
      }
    }
    if (!fromKnownIframe) return;
    if (data.type === 'hpc-nav' && typeof data.href === 'string') {
      // Basic href sanity check then navigate.
      if (!/^file:/i.test(data.href)) return;
      window.location.assign(data.href);
    } else if (data.type === 'hpc-height' && typeof data.height === 'number') {
      // Adjust iframe height.
      for (var j = 0; j < iframes.length; j++) {
        var iframe = iframes[j];
        if (event.source === iframe.contentWindow) {
          iframe.style.height = (Math.ceil(data.height) + 32) + 'px';
          break;
        }
      }
    }
  });
</script>
EOF

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

open_generated_report() {
  local report_path="$1"

  if [ -n "${CONTAINER_ID:-}" ] && command -v distrobox-host-exec >/dev/null 2>&1; then
    distrobox-host-exec xdg-open "$report_path" >/dev/null 2>&1 &
    return 0
  fi

  if command -v xdg-open >/dev/null 2>&1; then
    xdg-open "$report_path" >/dev/null 2>&1 &
    return 0
  fi

  if command -v open >/dev/null 2>&1; then
    open "$report_path" >/dev/null 2>&1 &
    return 0
  fi

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
    cabal test "$pkg" --jobs=1 --enable-coverage --test-show-details=direct --test-options="+RTS --read-tix-file=no -RTS --match Unit"

    pkg_hpc_dir=$(find dist-newstyle -path "*/$pkg-*/opt/hpc/vanilla" -type d -print | head -n1)
    if [ -n "$pkg_hpc_dir" ]; then
      dest="$coverage_staging_dir/$pkg"
      rm -rf "$dest"
      mkdir -p "$dest"
      cp -r "$pkg_hpc_dir"/. "$dest"/

      # The TIX also records the test-suite entry point. Keep its MIX files
      # beside the package report so aggregate reporting never has to search
      # mixes left behind by a different package's coverage build.
      while IFS= read -r component_mix_dir; do
        [ -z "$component_mix_dir" ] && continue
        staged_component_mix="$dest/components/${component_mix_dir#dist-newstyle/}"
        mkdir -p "$(dirname "$staged_component_mix")"
        cp -r "$component_mix_dir" "$staged_component_mix"
      done < <(
        find dist-newstyle -type d -path '*/extra-compilation-artifacts/hpc/vanilla/mix' -print \
          | awk -v package="$pkg" 'index($0, "/" package "-")' \
          | sort
      )
    fi
  else
    printf '\n\033[33mSkipping coverage for: %s (coverage: False in cabal.project)\033[0m\n' "$pkg"
  fi
done

while IFS= read -r staged_pkg; do
  [ -z "$staged_pkg" ] && continue
  package_name="$(basename "$staged_pkg")"
  target_dir=$(find dist-newstyle -path "*/$package_name-*/opt/hpc/vanilla" -type d -print | head -n1)
  if [ -n "$target_dir" ]; then
    rm -rf "$target_dir"
    mkdir -p "$target_dir"
    cp -r "$staged_pkg"/. "$target_dir"/
  fi
done < <(find "$coverage_staging_dir" -mindepth 1 -maxdepth 1 -type d -print)
repoRoot="$(pwd)"
missing_coverage=false
copied_mix=false
declare -a hpc_search_dirs=()
declare -a per_project_findings=()
declare -a aggregate_findings=()
declare -a aggregate_tix_paths=()
while IFS= read -r tixfile; do
  [ -z "$tixfile" ] && continue
  aggregate_tix_paths+=("$tixfile")
done < <(find "$coverage_staging_dir" -path '*/tix/*.tix' -type f -print | sort)
# Each staged MIX directory is from the same coverage build as the staged TIX
# files. Do not search the live build tree: it contains incompatible mixes from
# the individual package runs.
while IFS= read -r mixdir; do
  [ -z "$mixdir" ] && continue
  \cp -Rf "$mixdir"/. "$mix_cache_dir"/
  if ! array_contains "$mixdir" ${hpc_search_dirs[@]+"${hpc_search_dirs[@]}"}; then
    hpc_search_dirs+=("$mixdir")
  fi
  copied_mix=true
done < <(find "$coverage_staging_dir" -type d -name mix -print | sort)
if [ -d "$hpc_work_dir" ] && find "$hpc_work_dir" -mindepth 1 -print -quit >/dev/null 2>&1; then
  \cp -Rf "$hpc_work_dir"/. "$mix_cache_dir"/
  while IFS= read -r extra_mix; do
    [ -z "$extra_mix" ] && continue
    if ! array_contains "$extra_mix" ${hpc_search_dirs[@]+"${hpc_search_dirs[@]}"}; then
      hpc_search_dirs+=("$extra_mix")
    fi
    copied_mix=true
  done < <(find "$hpc_work_dir" -type d -name mix -print)
fi
if $copied_mix; then
  mix_dir_count=${#hpc_search_dirs[@]}
  echo "Collected HPC mix files into $mix_cache_dir ($mix_dir_count directories)."
fi
while IFS= read -r report; do
  [ -z "$report" ] && continue
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
  report_sed_tmp="$(mktemp "$temp_root/report.sed.XXXXXX")"
  sed "1,/<\/body>/s@</body>@$snippet@" "$report" > "$report_sed_tmp"
  mv "$report_sed_tmp" "$report"
  pkg_version_dir="$(basename "$(dirname "$(dirname "$(dirname "$(dirname "$(dirname "$report")")")")")")"
  package_name="${pkg_version_dir%%-[0-9]*}"

  fractions=()
  while IFS= read -r fraction_line; do
    fractions+=("$fraction_line")
  done < <(
    awk 'BEGIN{IGNORECASE=1}
      {
        if (!capture) {
          if (match($0, /Program Coverage Total/)) {
            $0 = substr($0, RSTART + RLENGTH)
            capture = 1
          } else {
            next
          }
        }
        while (match($0, /[0-9]+[[:space:]]*\/[[:space:]]*[0-9]+/)) {
          s = substr($0, RSTART, RLENGTH)
          gsub(/[[:space:]]/, "", s)
          n = index(s, "/")
          if (n > 0) {
            printf "%s/%s\n", substr(s, 1, n-1), substr(s, n+1)
          }
          $0 = substr($0, 1, RSTART-1) substr($0, RSTART+RLENGTH)
        }
        if (index($0, "</tr>") > 0) {
          exit
        }
      }
    ' "$report"
  )

  categories=(
    "Top-level declarations"
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
done < <(find dist-newstyle -name hpc_index.html -type f -print | sort)
aggregate_report_output=""
aggregate_tix_to_report=""
if [ "${#aggregate_tix_paths[@]}" -gt 0 ]; then
  if [ "${#aggregate_tix_paths[@]}" -gt 1 ]; then
    aggregate_tix_to_report="$temp_root/all-packages.tix"
    rm -f "$aggregate_tix_to_report"
    hpc sum --union --output="$aggregate_tix_to_report" ${aggregate_tix_paths[@]+"${aggregate_tix_paths[@]}"}
  else
    aggregate_tix_to_report="${aggregate_tix_paths[0]}"
  fi

  report_args=()
  for search_dir in ${hpc_search_dirs[@]+"${hpc_search_dirs[@]}"}; do
    report_args+=("--hpcdir" "$search_dir")
  done
  while IFS= read -r spec_path; do
    [ -z "$spec_path" ] && continue
    spec_module="${spec_path#*/test/}"
    spec_module="${spec_module%.hs}"
    spec_module="${spec_module//\//.}"
    report_args+=("--exclude=$spec_module")
  done < <(find packages examples -path "*/test/*Spec.hs" -type f -print | sort)
  # Test source modules are excluded above. Each test suite also contributes a
  # generated Main module, which has no application coverage contract.
  while IFS= read -r test_main_mix; do
    [ -z "$test_main_mix" ] && continue
    test_component_package="$(basename "$(dirname "$test_main_mix")")"
    report_args+=("--exclude=$test_component_package:Main")
  done < <(find "$coverage_staging_dir" -path '*/components/*/mix/*-tests/Main.mix' -type f -print | sort)

  echo -e "\n\033[90mFull coverage report (all packages):\033[0m"
  if aggregate_report_output=$(hpc report ${report_args[@]+"${report_args[@]}"} "$aggregate_tix_to_report" 2>&1); then
    printf '%s\n' "$aggregate_report_output"
    while IFS= read -r line; do
      if [[ "$line" == *"expressions used"* || "$line" == *"boolean coverage"* || "$line" == *"alternatives used"* ]] && awk 'match($0, /[0-9]+(\.[0-9]+)?%/) { s=substr($0,RSTART,RLENGTH); gsub(/%/,"",s); if ((s+0)<100) exit 0; exit 1 } { exit 1 }' <<<"$line"; then
        trimmed_line=$(printf '%s\n' "$line" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
        aggregate_findings+=("$trimmed_line")
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
open_generated_report "$(pwd)/hpc_index.html" || true

if [ "${#per_project_findings[@]}" -gt 0 ]; then
  echo
  printf '\033[31mPer-project reports found with <100%% coverage, exiting with error:\033[0m\n'
  for finding in ${per_project_findings[@]+"${per_project_findings[@]}"}; do
    printf '\033[31m- %s\033[0m\n' "$finding"
  done
elif [ "${#aggregate_findings[@]}" -gt 0 ]; then
  echo
  printf '\033[31mAggregate coverage report found <100%% coverage, exiting with error:\033[0m\n'
  for line in ${aggregate_findings[@]+"${aggregate_findings[@]}"}; do
    printf '\033[31m- %s\033[0m\n' "$line"
  done
  missing_coverage=true
fi

if $missing_coverage; then
  exit 1
fi
