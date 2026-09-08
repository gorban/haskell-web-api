# syntax=docker/dockerfile:1
# Multi-stage Dockerfile for Haskell web-api
# Build a runtime image without running tests with: docker build -t haskell-web-api .
#
# To extract coverage reports from the build stage:
#   docker build --target coverage-artifacts --output type=local,dest=./coverage-out .
#
# To build a runtime image after running coverage/tests:
#   docker build --target runtime-with-tests -t haskell-web-api .

# =============================================================================
# Stage 1: Build environment with GHC 9.14.1 and Cabal 3.16.1.0
# =============================================================================
FROM debian:bookworm-slim AS builder

SHELL ["/bin/bash", "-eo", "pipefail", "-c"]

# Install build dependencies, GHCup, GHC, and Cabal in one layer
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV PATH="/opt/.ghcup/bin:/root/.local/bin:/root/.cabal/bin:${PATH}"
# The project-owned spec preprocessor reads UTF-8 source. Debian's bare image
# otherwise inherits the ASCII C locale, which makes its coverage/test stage
# reject ordinary Unicode in Haskell comments and strings.
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8
RUN <<EOF
apt-get update
apt-get install -y --no-install-recommends \
    curl \
    gcc \
    g++ \
    git \
    libc6-dev \
    libffi-dev \
    libgmp-dev \
    libicu-dev \
    libpq-dev \
    libnuma-dev \
    libncurses-dev \
    make \
    lld \
    nodejs \
    npm \
    pkg-config \
    postgresql-client \
    xz-utils \
    zlib1g-dev \
    ca-certificates
rm -rf /var/lib/apt/lists/*

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh

ghcup install ghc 9.14.1 --set
ghcup install cabal 3.16.1.0 --set
ghc --version
cabal --version

# Install hspec-discover globally (before project context exists)
# GHC needs this executable during test compilation
# Use --install-method=copy to avoid symlink issues in Docker
cabal update
cabal install hspec-discover --install-method=copy --overwrite-policy=always
EOF

WORKDIR /app

# Copy cabal files first for better layer caching
COPY cabal.project ./
COPY examples/catalog-domain/catalog-domain.cabal examples/catalog-domain/
COPY examples/composed-domains/composed-domains.cabal examples/composed-domains/
COPY examples/custom-api/custom-api.cabal examples/custom-api/
COPY examples/custom-db-adapter/custom-db-adapter.cabal examples/custom-db-adapter/
COPY examples/localization/localization-example.cabal examples/localization/
COPY examples/multipart-upload/multipart-upload-example.cabal examples/multipart-upload/
COPY examples/orders-domain/orders-domain.cabal examples/orders-domain/
COPY examples/two-pages/two-pages-example.cabal examples/two-pages/
COPY examples/two-pages/Setup.hs examples/two-pages/SetupHooks.hs examples/two-pages/
COPY packages/core/core.cabal packages/core/
COPY packages/harch-web/harch-web.cabal packages/harch-web/
COPY packages/hspec-expectations-match/hspec-expectations-match.cabal packages/hspec-expectations-match/
COPY packages/postgres-database-changes/postgres-database-changes.cabal packages/postgres-database-changes/
COPY packages/test-core/test-core.cabal packages/test-core/
COPY packages/test-spec-preprocessor/test-spec-preprocessor.cabal packages/test-spec-preprocessor/
COPY packages/web-api/haskell-web-api.cabal packages/web-api/
COPY packages/web-api/Setup.hs packages/web-api/SetupHooks.hs packages/web-api/

# Download dependencies (cacheable layer)
RUN <<EOF
cabal update
cabal build all --only-dependencies
EOF

# Copy source code
COPY . .

# =============================================================================
# Stage 2: Build, test, and generate coverage
# =============================================================================
FROM builder AS build-and-test

# This hermetic target exercises the repository's unit-coverage boundary and
# selected browser behavior.  Setup prerequisites intentionally read the
# package-local override file, so provide only the no-daemon policy in this
# build stage rather than changing a developer's host environment. Cabal runs
# the web-api setup hook from that package directory.
RUN printf '%s\n' \
    'SETUP_AUTOSTART_DATABASE=false' \
    'SETUP_AUTOSTART_JAEGER=false' \
    > packages/web-api/.env.local

# Install the test-only browser in this stage; release images remain browser-free.
RUN <<EOF
cd packages/test-core/playwright-runner
npm ci
npx playwright install chromium --with-deps
EOF

# Run coverage script (builds with -O0 for accurate coverage) then rebuild with -O2 for release
RUN <<EOF
./generate-code-coverage.sh # Runs Unit tests and ensures 100% coverage
cabal test two-pages-example-tests --test-show-details=direct --test-options="--match real-browser"
cabal build all -O2
cp dist-newstyle/build/x86_64-linux/ghc-*/haskell-web-api-*/opt/build/haskell-web-api/haskell-web-api /app/haskell-web-api-bin
EOF

# =============================================================================
# Stage 3: Coverage artifacts preparation (only runs when targeting coverage-artifacts)
# =============================================================================
FROM build-and-test AS coverage-prep

# Prepare coverage output directory with all fixes applied
RUN <<EOF
mkdir -p /app/_coverage
cp hpc_index.html /app/_coverage/index.html

# Copy all HPC report directories
find dist-newstyle -type d -path '*/hpc/vanilla/html' | while read -r html_dir; do
  pkg_ver=$(echo "$html_dir" | sed -n 's|.*/ghc-[^/]*/\([^/]*\)/opt/hpc/vanilla/html|\1|p')
  if [ -n "$pkg_ver" ]; then
    mkdir -p "/app/_coverage/$pkg_ver"
    cp -r "$html_dir"/* "/app/_coverage/$pkg_ver/"
  fi
done

# Fix iframe src paths in the index
sed -i "s|dist-newstyle/build/[^/]*/ghc-[^/]*/\([^/]*\)/opt/hpc/vanilla/html/|\1/|g" /app/_coverage/index.html

# Fix the file: URL check to work with https:// (same-origin check)
sed -i 's@if (!/^file:/i\.test(data\.href)) return;@if (new URL(data.href, window.location.href).origin !== window.location.origin) return;@g' /app/_coverage/index.html
EOF

# =============================================================================
# Stage 4: Coverage artifacts extraction stage
# Use with: docker build --target coverage-artifacts --output type=local,dest=./coverage-out .
# =============================================================================
FROM scratch AS coverage-artifacts

# Copy prepared coverage reports (with iframe paths and JS fixes already applied)
COPY --from=coverage-prep /app/_coverage/ /

# =============================================================================
# Stage 5: Release build without running coverage/tests
# =============================================================================
FROM builder AS release-build

RUN <<EOF
cabal build all -O2
cp dist-newstyle/build/x86_64-linux/ghc-*/haskell-web-api-*/opt/build/haskell-web-api/haskell-web-api /app/haskell-web-api-bin
EOF

# =============================================================================
# Stage 6: Minimal runtime image after the coverage-tested build
# =============================================================================
FROM debian:bookworm-slim AS runtime-with-tests

# Match the builder's Debian ABI.  The executable links ICU through the C++
# bridge, libpq and their transitive system libraries; Alpine's musl/gcompat
# layer is not a supported substitute for that loader/library closure.
RUN <<EOF
set -e
apt-get update
apt-get install -y --no-install-recommends \
    ca-certificates \
    certbot \
    libcap2-bin \
    libffi8 \
    libgmp10 \
    libicu72 \
    libncurses6 \
    libnuma1 \
    libpq5 \
    libstdc++6 \
    libtinfo6 \
    openssl \
    zlib1g
rm -rf /var/lib/apt/lists/*
addgroup --gid 1000 app
adduser --disabled-password --gecos '' --uid 1000 --ingroup app app
EOF

WORKDIR /app

# Copy the compiled binary from build stage
COPY --from=build-and-test --chown=app:app /app/haskell-web-api-bin /app/haskell-web-api

# Copy the app's bundled public assets so runtime images keep the same asset layout
# as the repository even before runtime config is expanded further.
COPY --from=build-and-test --chown=app:app /app/packages/web-api/public /app/public

# Grant only the runtime binary the capability to bind privileged ports such as 80/443,
# then keep the container itself running as the non-root app user.
RUN <<EOF
set -e
setcap cap_net_bind_service+ep /app/haskell-web-api
getcap /app/haskell-web-api
EOF

# Switch to non-root user
USER app

# Default command
ENTRYPOINT ["/app/haskell-web-api"]

# =============================================================================
# Stage 7: Minimal runtime image without running coverage/tests
# =============================================================================
FROM debian:bookworm-slim AS runtime

RUN <<EOF
set -e
apt-get update
apt-get install -y --no-install-recommends \
    ca-certificates \
    certbot \
    libcap2-bin \
    libffi8 \
    libgmp10 \
    libicu72 \
    libncurses6 \
    libnuma1 \
    libpq5 \
    libstdc++6 \
    libtinfo6 \
    openssl \
    zlib1g
rm -rf /var/lib/apt/lists/*
addgroup --gid 1000 app
adduser --disabled-password --gecos '' --uid 1000 --ingroup app app
EOF

WORKDIR /app

COPY --from=release-build --chown=app:app /app/haskell-web-api-bin /app/haskell-web-api
COPY --from=release-build --chown=app:app /app/packages/web-api/public /app/public

RUN <<EOF
set -e
setcap cap_net_bind_service+ep /app/haskell-web-api
getcap /app/haskell-web-api
EOF

USER app

ENTRYPOINT ["/app/haskell-web-api"]
