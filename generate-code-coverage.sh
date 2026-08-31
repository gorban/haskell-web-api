#!/usr/bin/env bash

set -euo pipefail

coverage_fraction_is_incomplete() {
  local fraction="$1"
  local covered
  local total

  IFS='/' read -r covered total <<<"$fraction"
  if ! [[ "$covered" =~ ^[0-9]+$ && "$total" =~ ^[0-9]+$ ]]; then
    return 0
  fi
  if [ "$total" = "0" ]; then
    [ "$covered" != "0" ]
    return
  fi
  [ "$covered" != "$total" ]
}

coverage_percentage_is_incomplete() {
  local percentage="$1"

  if ! [[ "$percentage" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
    return 0
  fi
  awk -v percentage="$percentage" 'BEGIN { exit !(percentage + 0 < 100) }'
}

categories=(
  "Top-level declarations"
  "Alternatives"
  "Expressions"
)

report_coverage_fractions() {
  local report="$1"

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
}

report_coverage_is_complete() {
  local report="$1"
  local -a fractions=()
  local fraction

  mapfile -t fractions < <(report_coverage_fractions "$report")
  if [ "${#fractions[@]}" -lt "${#categories[@]}" ]; then
    return 1
  fi
  for index in "${!categories[@]}"; do
    fraction="${fractions[$index]}"
    if coverage_fraction_is_incomplete "$fraction"; then
      return 1
    fi
  done
}

# GHC executes these implementation modules only while compiling quasiquotes.
# Their counters belong to the compiler process, not a test executable's TIX,
# so an ordinary runtime HPC run cannot observe them. 'AttributeLowering' and
# 'LoweringSupport' are private collaborators reachable only from that same
# compile-time lowering path. Keep this exact list deliberately small: every
# ordinary runtime module, including generated instances and error paths,
# remains in the 100% gate.
runtime_coverage_filter_args() {
  local package_version_dir="$1"
  local package_name="$2"

  runtime_coverage_args=("--include=${package_version_dir}-inplace:")
  case "$package_name" in
    harch-web)
      runtime_coverage_args+=(
        "--exclude=${package_version_dir}-inplace:HarchWeb.Markup.Quasi"
        "--exclude=${package_version_dir}-inplace:HarchWeb.Markup.Quasi.AttributeLowering"
        "--exclude=${package_version_dir}-inplace:HarchWeb.Markup.Quasi.Lowering"
        "--exclude=${package_version_dir}-inplace:HarchWeb.Markup.Quasi.LoweringSupport"
        "--exclude=${package_version_dir}-inplace:HarchWeb.Markup.Quasi.Parser"
      )
      ;;
  esac
}

# The test-suite HPC directory is nested below the package directory, e.g.
# @.../ghc-9.14.1/custom-api-0.1.0.0/t/custom-api-tests/opt/hpc/vanilla@.
# Do not mistake @custom-api-tests@ for the package namespace: its MIX files
# are named @custom-api-0.1.0.0-inplace@ and an include filter for the test
# component silently produces an empty (0/0) HTML report.
package_version_dir_from_hpc_dir() {
  local hpc_dir="$1"
  local package_name="$2"
  local candidate="$hpc_dir"
  local parent

  while [ "$candidate" != "/" ]; do
    parent="$(dirname "$candidate")"
    if [[ "$(basename "$parent")" == ghc-* && "$(basename "$candidate")" == "$package_name"-* ]]; then
      printf '%s\n' "$(basename "$candidate")"
      return 0
    fi
    candidate="$parent"
  done
  return 1
}

package_version_dir_from_path() {
  local path="$1"
  local candidate="$path"
  local parent

  while [ "$candidate" != "/" ]; do
    parent="$(dirname "$candidate")"
    if [[ "$(basename "$parent")" == ghc-* ]]; then
      printf '%s\n' "$(basename "$candidate")"
      return 0
    fi
    candidate="$parent"
  done
  return 1
}

consolidated_report_is_included() {
  local report="$1"

  # TestCore is test support, not a shippable library or tested example. Keep
  # its unit coverage run, but do not display or aggregate its empty report.
  case "$report" in
    */test-core-*/hpc/vanilla/html/hpc_index.html) return 1 ;;
    *) return 0 ;;
  esac
}

coverage_gate_fixture() {
  local project_fraction="$1"
  local aggregate_percentage="$2"
  local missing_coverage=false

  if coverage_fraction_is_incomplete "$project_fraction"; then
    missing_coverage=true
  fi
  if coverage_percentage_is_incomplete "$aggregate_percentage"; then
    missing_coverage=true
  fi
  if "$missing_coverage"; then
    return 1
  fi
  return 0
}

if [ "${1:-}" = "--coverage-gate-fixture" ]; then
  if [ "$#" != 3 ]; then
    printf 'usage: %s --coverage-gate-fixture <project-covered/total> <aggregate-percent>\n' "$0" >&2
    exit 2
  fi
  coverage_gate_fixture "$2" "$3"
  exit
fi

if [ "${1:-}" = "--coverage-report-fixture" ]; then
  if [ "$#" != 2 ]; then
    printf 'usage: %s --coverage-report-fixture <hpc-report>\n' "$0" >&2
    exit 2
  fi
  report_coverage_is_complete "$2"
  exit
fi

if [ "${1:-}" = "--package-version-dir-fixture" ]; then
  if [ "$#" != 3 ]; then
    printf 'usage: %s --package-version-dir-fixture <hpc-dir> <package-name>\n' "$0" >&2
    exit 2
  fi
  package_version_dir_from_hpc_dir "$2" "$3"
  exit
fi

if [ "${1:-}" = "--consolidated-report-fixture" ]; then
  if [ "$#" != 2 ]; then
    printf 'usage: %s --consolidated-report-fixture <hpc-report-path>\n' "$0" >&2
    exit 2
  fi
  consolidated_report_is_included "$2"
  exit
fi

if ! command -v ld.lld >/dev/null; then
  printf '%s\n' 'LLVM lld is required for the coverage build; install an ld.lld executable before running this check.' >&2
  exit 2
fi

cabal clean

# TODO: this is a workaround for an issue that appeared when we switched from
#       build-type: Simple to build-type: Custom in various packages. Addresses error:
#       Error: [Cabal-5678]
#       Could not find test program "<repo-root>\dist-newstyle\build\<arch>\ghc-<version>\
#         <package>\opt\build\<package>-tests\<package>-tests.exe".
#         Did you build the package first?
cabal build all --jobs=1 --ghc-options=-optl-fuse-ld=lld

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
mkdir -p "$coverage_staging_dir"
coverage_root="$(pwd)"

coverage_source_args=()
while IFS= read -r cabal_file; do
  package_root="${cabal_file%/*}"
  coverage_source_args+=("--srcdir=$coverage_root/${package_root#./}")
done < <(find . -path './dist-newstyle' -prune -o -name '*.cabal' -type f -print | sort)

project_local_path="cabal.project.local"
project_local_backup=""
if [ -f "$project_local_path" ]; then
  project_local_backup="$(mktemp "$temp_root/cabal.project.local.backup.XXXXXX")"
  cp "$project_local_path" "$project_local_backup"
  rm -f "$project_local_path"
fi

restore_project_local() {
  if [ -n "$project_local_backup" ]; then
    cp "$project_local_backup" "$project_local_path"
  else
    rm -f "$project_local_path"
  fi
  rm -rf "$temp_root"
}

trap restore_project_local EXIT

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
    cabal configure --disable-backup --ghc-options="-O0 -optl-fuse-ld=lld"

    # Clean build artifacts to avoid stale tix data that can bleed between runs.
    find dist-newstyle -name "*.tix" -type f -print0 | xargs -0 rm -f --

    printf '\n\033[36mRunning tests with coverage for: %s\033[0m\n' "$pkg"
    cabal test "$pkg" --jobs=1 --enable-coverage --test-show-details=direct --test-options="+RTS --read-tix-file=no -RTS --match Unit"

    pkg_hpc_dir=$(find dist-newstyle -path "*/$pkg-*/opt/hpc/vanilla" -type d -print | head -n1)
    if [ -n "$pkg_hpc_dir" ]; then
      if ! pkg_version_dir="$(package_version_dir_from_hpc_dir "$pkg_hpc_dir" "$pkg")"; then
        printf 'Could not derive the package-version directory for coverage package %s.\n' "$pkg" >&2
        exit 1
      fi
      coverage_hpc_args=()
      while IFS= read -r mix_dir; do
        coverage_hpc_args+=("--hpcdir=$coverage_root/$mix_dir")
      done < <(find dist-newstyle -type d -path '*/extra-compilation-artifacts/hpc/vanilla/mix' -print | sort -u)

      tix_file=$(find "$pkg_hpc_dir/tix" -type f -name '*.tix' -print | head -n1)
      if [ -z "$tix_file" ]; then
        printf 'No TIX file was generated for coverage package %s.\n' "$pkg" >&2
        exit 1
      fi

      runtime_coverage_filter_args "$pkg_version_dir" "$pkg"
      if ! package_coverage_report=$(hpc report "${runtime_coverage_args[@]}" "$tix_file" "${coverage_hpc_args[@]}"); then
        printf 'Could not resolve every HPC module for coverage package %s.\n' "$pkg" >&2
        exit 1
      fi

      dest="$coverage_staging_dir/$pkg"
      rm -rf "$dest"
      mkdir -p "$dest"
      cp -r "$pkg_hpc_dir"/. "$dest"/

      rm -rf "$dest/html"
      if ! hpc_markup_output=$(hpc markup --destdir="$dest/html" "${runtime_coverage_args[@]}" "${coverage_source_args[@]}" "$tix_file" "${coverage_hpc_args[@]}" 2>&1); then
        printf 'Could not generate an authoritative HPC report for coverage package %s.\n' "$pkg" >&2
        printf '%s\n' "$hpc_markup_output" >&2
        exit 1
      fi
      printf 'Generated authoritative HPC report for %s.\n' "$pkg"

      # Keep the report that the gate used even when this package is the
      # first failure. Cabal's own package HTML can omit production MIX files,
      # which would otherwise leave a misleading partial report beside it.
      rm -rf "$pkg_hpc_dir"
      mkdir -p "$pkg_hpc_dir"
      cp -r "$dest"/. "$pkg_hpc_dir"/

      if ! printf '%s\n' "$package_coverage_report" | grep -q '100% expressions used' \
        || ! printf '%s\n' "$package_coverage_report" | grep -q '100% alternatives used' \
        || ! printf '%s\n' "$package_coverage_report" | grep -q '100% top-level declarations used'; then
        printf 'Authoritative coverage report for %s is incomplete:\n%s\n' "$pkg" "$package_coverage_report" >&2
        exit 1
      fi

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
declare -a per_project_findings=()
declare -a aggregate_findings=()
aggregate_covered=(0 0 0)
aggregate_total=(0 0 0)
report_count=0
while IFS= read -r report; do
  [ -z "$report" ] && continue
  consolidated_report_is_included "$report" || continue
  report_count=$((report_count + 1))
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
  if ! pkg_version_dir="$(package_version_dir_from_path "$report")"; then
    printf 'Could not derive the package-version directory for report %s.\n' "$report" >&2
    exit 1
  fi
  package_name="${pkg_version_dir%%-[0-9]*}"
  mapfile -t fractions < <(report_coverage_fractions "$report")

  for idx in "${!categories[@]}"; do
    fraction="${fractions[$idx]:-}"
    if [ -z "$fraction" ]; then
      per_project_findings+=("${categories[$idx]} coverage for $package_name could not be parsed from its HPC report.")
      missing_coverage=true
      continue
    fi
    cleaned_fraction="${fraction//[[:space:]]/}"
    IFS='/' read -r covered total <<<"$cleaned_fraction"
    if [ -z "$covered" ] || [ -z "$total" ]; then
      per_project_findings+=("${categories[$idx]} coverage for $package_name could not be parsed from its HPC report.")
      missing_coverage=true
      continue
    fi
    aggregate_covered[$idx]=$((aggregate_covered[$idx] + covered))
    aggregate_total[$idx]=$((aggregate_total[$idx] + total))
    if coverage_fraction_is_incomplete "$cleaned_fraction"; then
      per_project_findings+=("${categories[$idx]} coverage for $package_name ($covered/$total).")
      missing_coverage=true
    fi
  done
done < <(find dist-newstyle -name hpc_index.html -type f -print | sort)
if [ "$report_count" = "0" ]; then
  per_project_findings+=("No per-project HPC reports were found.")
  missing_coverage=true
fi
# Each package report is produced by its own coverage build. Aggregate those
# authoritative production-report totals, rather than unioning raw TIX files
# that also contain test-suite and build-only instrumentation from other runs.
echo -e "\n\033[90mFull coverage report (all packages):\033[0m"
for idx in "${!categories[@]}"; do
  covered="${aggregate_covered[$idx]}"
  total="${aggregate_total[$idx]}"
  if [ "$total" = "0" ]; then
    continue
  fi
  percentage=$((covered * 100 / total))
  printf ' %d%% %s used (%d/%d)\n' "$percentage" "${categories[$idx],,}" "$covered" "$total"
  if coverage_fraction_is_incomplete "$covered/$total"; then
    aggregate_findings+=("${categories[$idx]} coverage ($covered/$total).")
  fi
done
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
