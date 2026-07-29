#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

required_commands=(argon hlint rg)
missing_commands=()

for command_name in "${required_commands[@]}"; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    missing_commands+=("$command_name")
  fi
done

if [ "${#missing_commands[@]}" -ne 0 ]; then
  printf 'Missing required quality tools: %s\n' "${missing_commands[*]}" >&2
  printf '%s\n' 'Run tools/install-haskell-quality-tools.sh and the formatting-tool installer documented in SETUP.md.' >&2
  exit 1
fi

production_paths=(
  packages/core/src
  packages/harch-web/src
  packages/test-core/src
  packages/web-api/src
  examples
)
test_paths=(
  packages/core/test
  packages/harch-web/test
  packages/test-core/test
  packages/web-api/test
)

health_paths=(
  packages/core/src
  packages/harch-web/src
  packages/test-core/src
  packages/web-api/src
  packages/core/test
  packages/harch-web/test
  packages/test-core/test
  packages/web-api/test
  examples
)

module_name_for() {
  awk '
    /^module[[:space:]]+/ {
      print $2
      exit
    }
  ' "$1"
}

module_imports_for() {
  awk '
    /^import[[:space:]]+/ {
      for (field_index = 2; field_index <= NF; field_index++) {
        if ($field_index != "qualified" && $field_index != "safe" && $field_index !~ /^\{-#/) {
          print $field_index
          break
        }
      }
    }
  ' "$1" | sort -u
}

module_declaration_count_for() {
  awk '
    function add(name) {
      sub(/\(.*/, "", name)
      if (name != "") declarations[name] = 1
    }
    /^(data|newtype|type|class)[[:space:]]+/ { add($2); next }
    /^instance[[:space:]]+/ { add("instance:" NR); next }
    /^[A-Za-z_][A-Za-z0-9_\047]*[[:space:]]*::/ { add($1); next }
    /^[A-Za-z_][A-Za-z0-9_\047]*([[:space:]]+[^=]+)?[[:space:]]*=/ { add($1) }
    END {
      for (declaration in declarations) count++
      print count + 0
    }
  ' "$1"
}

module_export_count_for() {
  awk '
    function count_entries(text, pieces, count, piece_index, entry) {
      sub(/--.*/, "", text)
      sub(/[[:space:]]*where.*/, "", text)
      count = split(text, pieces, ",")
      for (piece_index = 1; piece_index <= count; piece_index++) {
        entry = pieces[piece_index]
        gsub(/^[[:space:]()]*/, "", entry)
        gsub(/[[:space:]()]*$/, "", entry)
        if (entry != "") exports++
      }
    }
    /^module[[:space:]]+/ { module_started = 1 }
    module_started && !exports_started {
      if (index($0, "(") != 0) {
        exports_started = 1
        line = $0
        sub(/^[^(]*\(/, "", line)
        count_entries(line)
      }
      if ($0 ~ /where/) exit
      next
    }
    exports_started {
      count_entries($0)
      if ($0 ~ /where/) exit
    }
    END { print exports + 0 }
  ' "$1"
}

module_max_arity_for() {
  awk '
    /^[A-Za-z_][A-Za-z0-9_\047]*([[:space:]]+[^=]+)?[[:space:]]*=/ {
      left = $0
      sub(/[[:space:]]*=.*/, "", left)
      if (left ~ /::/) next
      count = split(left, pieces, /[[:space:]]+/) - 1
      if (count > maximum) maximum = count
    }
    END { print maximum + 0 }
  ' "$1"
}

print_module_health_reports() {
  declare -a health_keys=()
  declare -a cycle_stack=()
  declare -a import_cycles=()
  declare -A path_by_key=()
  declare -A scope_by_key=()
  declare -A module_by_key=()
  declare -A line_count_by_key=()
  declare -A declaration_count_by_key=()
  declare -A import_count_by_key=()
  declare -A export_count_by_key=()
  declare -A arity_by_key=()
  declare -A imports_by_key=()
  declare -A keys_by_module=()
  declare -A local_imports_by_key=()
  declare -A fanin_by_key=()
  declare -A visit_state=()
  declare -A cycle_seen=()

  while IFS= read -r path; do
    case "$path" in
      packages/hspec-expectations-match/*) continue ;;
      */test/*) scope='test' ;;
      *) scope='production' ;;
    esac

    module_name="$(module_name_for "$path")"
    if [ -z "$module_name" ]; then
      module_name="<no module declaration>"
    fi

    key="$scope:$path"
    imports="$(module_imports_for "$path")"
    health_keys+=("$key")
    path_by_key["$key"]="$path"
    scope_by_key["$key"]="$scope"
    module_by_key["$key"]="$module_name"
    line_count_by_key["$key"]="$(wc -l < "$path" | tr -d ' ')"
    declaration_count_by_key["$key"]="$(module_declaration_count_for "$path")"
    import_count_by_key["$key"]="$(printf '%s\n' "$imports" | sed '/^$/d' | wc -l | tr -d ' ')"
    export_count_by_key["$key"]="$(module_export_count_for "$path")"
    arity_by_key["$key"]="$(module_max_arity_for "$path")"
    imports_by_key["$key"]="$imports"
    keys_by_module["$module_name"]="${keys_by_module[$module_name]:-} $key"
  done < <(find "${health_paths[@]}" -type f -name '*.hs' -print | sort)

  for key in "${health_keys[@]}"; do
    unset seen_targets
    declare -A seen_targets=()
    local_targets=()
    while IFS= read -r imported_module; do
      [ -z "$imported_module" ] && continue
      for target_key in ${keys_by_module[$imported_module]:-}; do
        if [ -z "${seen_targets[$target_key]:-}" ]; then
          seen_targets["$target_key"]=1
          local_targets+=("$target_key")
          fanin_by_key["$target_key"]=$(( ${fanin_by_key[$target_key]:-0} + 1 ))
        fi
      done
    done <<<"${imports_by_key[$key]}"
    local_imports_by_key["$key"]="${local_targets[*]}"
  done

  record_cycle() {
    local target="$1"
    local cycle=''
    local found=false
    local member
    for member in "${cycle_stack[@]}"; do
      if [ "$member" = "$target" ]; then
        found=true
      fi
      if [ "$found" = true ]; then
        cycle+="${module_by_key[$member]} -> "
      fi
    done
    cycle+="${module_by_key[$target]}"
    if [ -z "${cycle_seen[$cycle]:-}" ]; then
      cycle_seen["$cycle"]=1
      import_cycles+=("$cycle")
    fi
  }

  visit_module() {
    local current="$1"
    local target
    visit_state["$current"]='visiting'
    cycle_stack+=("$current")
    for target in ${local_imports_by_key[$current]:-}; do
      case "${visit_state[$target]:-}" in
        visiting) record_cycle "$target" ;;
        '') visit_module "$target" ;;
      esac
    done
    unset "cycle_stack[$((${#cycle_stack[@]} - 1))]"
    visit_state["$current"]='done'
  }

  for key in "${health_keys[@]}"; do
    if [ -z "${visit_state[$key]:-}" ]; then
      visit_module "$key"
    fi
  done

  print_health_table() {
    local requested_scope="$1"
    printf '\nModule-health report: %s (advisory)\n\n' "$requested_scope"
    printf '%-42s %7s %7s %7s %7s %7s %8s %7s  %s\n' 'module' 'lines' 'decls' 'imports' 'exports' 'arity' 'fan-out' 'fan-in' 'path'
    for key in "${health_keys[@]}"; do
      [ "${scope_by_key[$key]}" = "$requested_scope" ] || continue
      local_fanout=0
      for ignored_target in ${local_imports_by_key[$key]:-}; do
        local_fanout=$((local_fanout + 1))
      done
      printf '%-42s %7s %7s %7s %7s %7s %8s %7s  %s\n' \
        "${module_by_key[$key]}" \
        "${line_count_by_key[$key]}" \
        "${declaration_count_by_key[$key]}" \
        "${import_count_by_key[$key]}" \
        "${export_count_by_key[$key]}" \
        "${arity_by_key[$key]}" \
        "$local_fanout" \
        "${fanin_by_key[$key]:-0}" \
        "${path_by_key[$key]}"
    done
  }

  print_health_table 'production'
  print_health_table 'test'

  printf '\nLocal import cycles (advisory)\n\n'
  if [ "${#import_cycles[@]}" -eq 0 ]; then
    printf 'None found.\n'
  else
    printf '%s\n' "${import_cycles[@]}" | sort | sed 's/^/- /'
  fi
}

print_argon_report() {
  local title="$1"
  local minimum="$2"
  shift 2

  printf '\n%s\n\n' "$title"
  argon --no-color --min "$minimum" "$@" \
    | rg -v '^\s+[0-9]+:[0-9]+ spec - ' \
    || true
}

print_argon_report 'Production priority hotspots (complexity > 10)' 11 "${production_paths[@]}"
print_argon_report 'Production review candidates (complexity >= 8)' 8 "${production_paths[@]}"
print_argon_report 'Test helper priority hotspots (top-level spec excluded)' 11 "${test_paths[@]}"
print_argon_report 'Test helper review candidates (top-level spec excluded)' 8 "${test_paths[@]}"

print_module_health_reports

printf '\nHLint advisory report\n\n'
hlint "${production_paths[@]}" "${test_paths[@]}" || true

all_paths=("${production_paths[@]}" "${test_paths[@]}")
pattern_config="tools/haskell-quality-monads.conf"

printf '\nConstructor-forwarding review candidates\n'
while read -r label failure_constructor success_constructor; do
  case "$label" in
    ''|'#'*) continue ;;
  esac

  printf '\n%s (%s/%s)\n' "$label" "$failure_constructor" "$success_constructor"
  rg -n --glob '*.hs' --glob '!packages/hspec-expectations-match/**' \
    --pcre2 \
    "(?:${failure_constructor}|${success_constructor})[^\\n]*->\\s*(?:pure\\s+|return\\s+)?\\(?\\s*(?:${failure_constructor}|${success_constructor})\\b" \
    "${all_paths[@]}" \
    || true
done < "$pattern_config"

printf '\nTransformer-result case review candidates\n\n'
rg -n -U --glob '*.hs' --glob '!packages/hspec-expectations-match/**' \
  --pcre2 '<-\s*(?:runExceptT|runMaybeT)\b[^\n]*\n\s*case\b' \
  "${all_paths[@]}" \
  || true

printf '\nManual effect-rail lifting review candidates\n\n'
rg -n --glob '*.hs' --glob '!packages/hspec-expectations-match/**' \
  --pcre2 'withExceptT\b[^\n]*\(ExceptT\b' \
  "${all_paths[@]}" \
  || true

printf '\nRepeated production string literals (3+ uses; advisory)\n\n'
rg --no-filename -o --glob '*.hs' --pcre2 '"(?:[^"\\]|\\.)*"' "${production_paths[@]}" \
  | sort \
  | uniq -c \
  | awk '$1 >= 3 { print }' \
  || true

printf '\nQuality report complete. Findings are advisory and require human review.\n'
