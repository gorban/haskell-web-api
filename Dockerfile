# syntax=docker/dockerfile:1
# Multi-stage Dockerfile for Haskell web-api
# Build with: docker build -t haskell-web-api .
#
# To extract coverage reports from the build stage:
#   docker build --target coverage-artifacts --output type=local,dest=./coverage-out .
#
# To build just the minimal runtime image:
#   docker build -t haskell-web-api .

# =============================================================================
# Stage 1: Build environment with GHC 9.12.2 and Cabal 3.16.1.0
# =============================================================================
FROM debian:bookworm-slim AS builder

SHELL ["/bin/bash", "-eo", "pipefail", "-c"]

# Install build dependencies, GHCup, GHC, and Cabal in one layer
ENV GHCUP_INSTALL_BASE_PREFIX=/opt
ENV PATH="/opt/.ghcup/bin:/root/.local/bin:/root/.cabal/bin:${PATH}"
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
    libnuma-dev \
    libncurses-dev \
    make \
    xz-utils \
    zlib1g-dev \
    ca-certificates
rm -rf /var/lib/apt/lists/*

curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    sh

ghcup install ghc 9.12.2 --set
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
COPY packages/core/core.cabal packages/core/
COPY packages/hspec-expectations-match/hspec-expectations-match.cabal packages/hspec-expectations-match/
COPY packages/test-core/test-core.cabal packages/test-core/
COPY packages/web-api/web-api.cabal packages/web-api/

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

# Run coverage script (builds with -O0 for accurate coverage) then rebuild with -O2 for release
RUN <<EOF
./generate-code-coverage.ps1 # Runs Unit tests and ensures 100% coverage
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
# Stage 5: Minimal runtime image (Alpine-based for security)
# =============================================================================
FROM alpine:3.20 AS runtime

# Install runtime dependencies and create non-root user
RUN <<EOF
set -e
apk add --no-cache \
    gmp \
    libffi \
    gcompat \
    ca-certificates
addgroup -g 1000 app
adduser -D -u 1000 -G app app
EOF

WORKDIR /app

# Copy the compiled binary from build stage
COPY --from=build-and-test --chown=app:app /app/haskell-web-api-bin /app/haskell-web-api

# Switch to non-root user
USER app

# Default command
ENTRYPOINT ["/app/haskell-web-api"]
