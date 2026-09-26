#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
checker="$repo_root/tools/check-scoped-css.sh"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

run_checker_in() {
  local fixture_dir="$1"
  (cd "$fixture_dir" && git init -q && git add -A && "$checker")
}

expect_pass() {
  local description="$1"
  local fixture_dir="$2"

  if ! output="$(run_checker_in "$fixture_dir" 2>&1)"; then
    printf '%s\n' "Scoped-CSS check unexpectedly rejected $description:" >&2
    printf '%s\n' "$output" >&2
    exit 1
  fi
}

expect_rejection() {
  local description="$1"
  local fixture_dir="$2"
  local expected_selector="$3"

  if output="$(run_checker_in "$fixture_dir" 2>&1)"; then
    printf '%s\n' "Scoped-CSS check unexpectedly accepted $description." >&2
    exit 1
  fi
  if ! printf '%s\n' "$output" | grep -qF "$expected_selector"; then
    printf '%s\n' "Scoped-CSS check rejected $description, but did not name the selector '$expected_selector'. Output:" >&2
    printf '%s\n' "$output" >&2
    exit 1
  fi
}

# The real repository must already pass.
(cd "$repo_root" && "$checker")

# A properly scoped selector, a compound selector qualified under a scoped
# ancestor, and a harch-global-marked selector (own line and inline) all
# pass.
pass_fixture="$fixture_root/pass"
mkdir -p "$pass_fixture"
cat >"$pass_fixture/site.css" <<'EOF'
.harch-page-frame-root {
  padding: 1rem;
}

.harch-page-frame-root button {
  padding: 1rem;
}

/* harch-global */
body {
  margin: 0;
}

/* harch-global */ a {
  color: blue;
}
EOF
expect_pass 'a fixture with only scoped and marked-global selectors' "$pass_fixture"

# A bare element selector with no scope and no marker is rejected, and the
# rejection names the offending selector.
bare_element_fixture="$fixture_root/bare-element"
mkdir -p "$bare_element_fixture"
cat >"$bare_element_fixture/site.css" <<'EOF'
div {
  display: flex;
}
EOF
expect_rejection 'a bare element selector' "$bare_element_fixture" 'div'

# An unscoped class (no harch- prefix) with no marker is rejected.
unscoped_class_fixture="$fixture_root/unscoped-class"
mkdir -p "$unscoped_class_fixture"
cat >"$unscoped_class_fixture/site.css" <<'EOF'
.card {
  padding: 1rem;
}
EOF
expect_rejection 'an unscoped class selector' "$unscoped_class_fixture" '.card'

# An attribute selector with no marker is rejected, including one nested
# inside a @media block.
attribute_selector_fixture="$fixture_root/attribute-selector"
mkdir -p "$attribute_selector_fixture"
cat >"$attribute_selector_fixture/site.css" <<'EOF'
@media (max-width: 24rem) {
  [data-example] {
    display: none;
  }
}
EOF
expect_rejection 'an unmarked attribute selector nested in @media' "$attribute_selector_fixture" '[data-example]'

# A comma-separated selector list rejects only its unscoped member, and a
# selector with a nested comma inside parentheses is treated as one
# selector rather than incorrectly split.
mixed_list_fixture="$fixture_root/mixed-list"
mkdir -p "$mixed_list_fixture"
cat >"$mixed_list_fixture/site.css" <<'EOF'
.harch-example-one,
.unscoped-two {
  color: red;
}

:where(a, button):focus-visible {
  outline: none;
}
EOF
expect_rejection 'a comma-separated list with one unscoped member' "$mixed_list_fixture" '.unscoped-two'

printf '%s\n' 'Scoped-CSS check fixture checks passed.'

# The reviewed vendored-distribution exemption: the pinned third-party
# stylesheet is exempt by explicit path, while authored stylesheets beside it
# remain fully checked (proving the exemption is not a suppression).
vendored_fixture="$(mktemp -d)"
mkdir -p "$vendored_fixture/packages/harch-web-openapi/assets/swagger-ui"
cat >"$vendored_fixture/packages/harch-web-openapi/assets/swagger-ui/swagger-ui.css" <<'EOF'
html.dark-mode .swagger-ui .models { color: red }
EOF
cat >"$vendored_fixture/author.css" <<'EOF'
.harch-demo-root { display: grid }
EOF
(cd "$vendored_fixture" && git init -q && git add -A)
expect_pass 'a fixture whose only unscoped stylesheet is the reviewed vendored distribution stylesheet' "$vendored_fixture"

cat >"$vendored_fixture/other.css" <<'EOF'
.plain-card { color: red }
EOF
(cd "$vendored_fixture" && git add -A)
expect_rejection 'an authored stylesheet outside the reviewed vendored path' "$vendored_fixture" '.plain-card'
