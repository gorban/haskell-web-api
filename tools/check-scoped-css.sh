#!/usr/bin/env bash

set -euo pipefail

# Verifies that every tracked stylesheet uses the CssScope/CssClass scoping
# convention from HarchWeb.StaticAssets: a properly scoped class always
# renders as `.harch-<scope>-<local>` (see `cssClassText`), so any selector
# containing the literal token `.harch-` is compliant, including a compound
# selector qualified under a scoped ancestor (e.g.
# `.harch-page-frame-root button`). A selector that is deliberately global
# (a reset rule, a design-token `:root`, a framework-wide `[data-*]` hook,
# and similar) must instead be marked with a `harch-global` comment, either
# on its own line directly above the rule or inline before it on the same
# line, e.g.:
#
#   /* harch-global */
#   body { margin: 0; }
#
#   /* harch-global */ a { color: blue; }
#
# Anything else — a bare element selector, an unscoped class, an id
# selector, an attribute selector, `:root`, `*`, `:where(...)`, and so on,
# with no `harch-global` marker — is a finding.
#
# This is a line/brace-oriented scan, not a full CSS grammar parser. It does
# handle: nested at-rule blocks (`@media`/`@supports`/...), whose own
# prelude is never checked as a selector while rules nested inside are
# checked normally; and comma-separated selector lists, split only on
# commas that are not nested inside `(...)` or `[...]` (so
# `:where(a, button, input, select)` and `[data-x="a,b"]` are each treated
# as one selector, not incorrectly split). It deliberately does not handle
# `@keyframes` percentage/`from`/`to` selectors (none exist in this
# repository's stylesheets today) and reports a multi-line selector's line
# number as the line of its opening `{`, not the selector's own first line.

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

mapfile -t stylesheets < <(git ls-files -- '*.css' | sort)

# Vendored third-party distributions are not authored CSS: their stylesheets
# ship byte-for-byte with a pinned upstream release, reviewed in the
# distribution's own README.md beside its LICENSE attribution and
# package.json pin. The scoping convention governs styles this repository
# authors, so the list below is a reviewed policy input — the same mechanism
# as check-build-diagnostics.sh's --allow-ghc-9-14-* flags — not a finding
# suppression: every other tracked stylesheet stays fully checked, and adding
# a vendored stylesheet requires editing this list in review.
# Reviewed exemption (2026-09-25): the pinned swagger-ui-dist 5.33.0
# distribution stylesheet.
vendored_stylesheets=(
  'packages/harch-web-openapi/assets/swagger-ui/swagger-ui.css'
)

if [ "${#stylesheets[@]}" -gt 0 ] && [ "${#vendored_stylesheets[@]}" -gt 0 ]; then
  filtered_stylesheets=()
  for stylesheet in "${stylesheets[@]}"; do
    keep=true
    for vendored in "${vendored_stylesheets[@]}"; do
      if [ "$stylesheet" = "$vendored" ]; then
        keep=false
        break
      fi
    done
    if [ "$keep" = true ]; then
      filtered_stylesheets+=("$stylesheet")
    fi
  done
  stylesheets=("${filtered_stylesheets[@]}")
fi

if [ "${#stylesheets[@]}" -eq 0 ]; then
  printf '%s\n' 'No tracked stylesheets were found.' >&2
  exit 1
fi

failed=0
for stylesheet in "${stylesheets[@]}"; do
  if ! awk '
    function is_at_rule(text,    trimmed) {
      trimmed = text
      gsub(/^[ \t\n]+/, "", trimmed)
      return substr(trimmed, 1, 1) == "@"
    }

    function has_marker(text) {
      return index(text, "harch-global") > 0
    }

    function strip_comments(text,    result, start, endc) {
      result = text
      while ((start = index(result, "/*")) > 0) {
        endc = index(substr(result, start), "*/")
        if (endc == 0) {
          result = substr(result, 1, start - 1)
          break
        }
        result = substr(result, 1, start - 1) substr(result, start + endc + 1)
      }
      return result
    }

    # Split `text` on commas that are not nested inside ( ) or [ ],
    # appending each trimmed fragment to global array `parts`, 1-indexed;
    # returns the fragment count.
    function split_top_level(text,    n, i, c, depth, current, count) {
      n = length(text)
      depth = 0
      current = ""
      count = 0
      for (i = 1; i <= n; i++) {
        c = substr(text, i, 1)
        if (c == "(" || c == "[") depth++
        else if (c == ")" || c == "]") depth--
        if (c == "," && depth == 0) {
          count++
          parts[count] = current
          current = ""
        } else {
          current = current c
        }
      }
      count++
      parts[count] = current
      return count
    }

    BEGIN { buf = "" }

    {
      line = $0
      lineNo = FNR
      n = length(line)
      for (i = 1; i <= n; i++) {
        c = substr(line, i, 1)
        if (c == "{") {
          rawBuf = buf
          selectorText = strip_comments(buf)
          gsub(/^[ \t\n]+/, "", selectorText)
          gsub(/[ \t\n]+$/, "", selectorText)
          if (selectorText != "" && !is_at_rule(selectorText) && !has_marker(rawBuf)) {
            count = split_top_level(selectorText)
            for (p = 1; p <= count; p++) {
              sel = parts[p]
              gsub(/^[ \t\n]+/, "", sel)
              gsub(/[ \t\n]+$/, "", sel)
              if (sel != "" && index(sel, ".harch-") == 0) {
                printf "%s:%d: unscoped selector (no .harch- class and no harch-global marker): %s\n", FILENAME, lineNo, sel > "/dev/stderr"
                failed = 1
              }
            }
          }
          buf = ""
        } else if (c == "}") {
          buf = ""
        } else {
          buf = buf c
        }
      }
      buf = buf "\n"
    }

    END { exit failed }
  ' "$stylesheet"; then
    failed=1
  fi
done

if [ "$failed" -ne 0 ]; then
  printf '%s\n' 'Found unscoped selectors in project-owned stylesheets.' >&2
  exit 1
fi

printf '%s\n' 'Every tracked stylesheet selector is scoped or explicitly marked harch-global.'
