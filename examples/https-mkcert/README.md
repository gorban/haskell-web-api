# https-mkcert

**Status:** Workflow guide

Show the preferred local-development HTTPS story when the developer wants a browser-trusted
self-signed certificate.

Current repo alignment:

- the repo currently ships an OpenSSL helper under `examples/reverse-proxy/generate-local-tls.sh`,
- the desired polished example should mention mkcert directly because it fits local app authoring
  better than asking every developer to manage their own CA manually.

Suggested snippet:

- [scripts/generate-dev-certs.sh.md](scripts/generate-dev-certs.sh.md)
