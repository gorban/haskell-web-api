# https-provided-certificate

**Status:** Implemented guide

Show the simplest direct HTTPS startup path using a provided certificate and private key.

Current repo alignment:

- manual TLS is already supported,
- the repo already ships a local certificate generator for the reverse-proxy example,
- this example should stay focused on direct app-managed TLS rather than nginx offload.

Suggested snippet:

- [env/.env.local.md](env/.env.local.md)
