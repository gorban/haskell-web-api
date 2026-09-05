# scripts/generate-dev-certs.sh

```bash
#!/usr/bin/env bash
set -euo pipefail

mkcert -install
mkdir -p ./tls
mkcert -cert-file ./tls/fullchain.pem -key-file ./tls/privkey.pem localhost 127.0.0.1
```

If `mkcert` is unavailable, the current repo's OpenSSL helper is still a valid fallback for local
testing.
