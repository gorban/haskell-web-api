#!/usr/bin/env bash

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tls_dir="$script_dir/tls"

ca_key="$tls_dir/local-root-ca.key"
ca_cert="$tls_dir/local-root-ca.pem"
leaf_key="$tls_dir/privkey.pem"
leaf_cert="$tls_dir/localhost.pem"
leaf_csr="$tls_dir/localhost.csr"
leaf_ext="$tls_dir/localhost.ext"
fullchain_cert="$tls_dir/fullchain.pem"
serial_file="$tls_dir/local-root-ca.srl"

mkdir -p "$tls_dir"

openssl genrsa -out "$ca_key" 2048
openssl req -x509 -new -nodes \
  -key "$ca_key" \
  -sha256 \
  -days 7 \
  -out "$ca_cert" \
  -subj "/CN=haskell-web-api local root CA"

openssl genrsa -out "$leaf_key" 2048
openssl req -new \
  -key "$leaf_key" \
  -out "$leaf_csr" \
  -subj "/CN=localhost"

cat > "$leaf_ext" <<'EOF'
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
keyUsage=digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth
subjectAltName=DNS:localhost,IP:127.0.0.1
EOF

openssl x509 -req \
  -in "$leaf_csr" \
  -CA "$ca_cert" \
  -CAkey "$ca_key" \
  -CAcreateserial \
  -out "$leaf_cert" \
  -days 7 \
  -sha256 \
  -extfile "$leaf_ext"

cat "$leaf_cert" "$ca_cert" > "$fullchain_cert"

rm -f "$leaf_csr" "$leaf_ext" "$serial_file"

printf '%s\n' \
  "Generated local reverse-proxy TLS files in $tls_dir" \
  "  CA certificate: $ca_cert" \
  "  Server certificate chain: $fullchain_cert" \
  "  Server private key: $leaf_key" \
  "" \
  "Verify with:" \
  "  curl --cacert $ca_cert https://127.0.0.1/api/status"
