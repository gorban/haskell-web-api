# https-acme

**Status:** Implemented guide

Show the real-certificate flow for a deployed app.

Current repo alignment:

- ACME/certbot-backed startup already exists,
- shared-directory TLS consumers are already documented,
- this example should clearly state the operational requirements instead of hiding them.

Suggested snippet:

- [env/.env.local.md](env/.env.local.md)

Required operational notes:

1. real DNS must point at the machine that answers the challenge,
2. port 80 must be reachable for `http-01`,
3. use staging first during setup and only switch to production once the path works.
