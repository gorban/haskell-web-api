# custom-db-adapter

**Status:** Iterative

Document the desired seam for replacing the default PostgreSQL-backed adapter with another database
technology such as SQLite.

Current repo alignment:

- there is already a clear database seam in the example app,
- the repo is still more PostgreSQL-first than adapter-first,
- this example should be honest that the clean adapter story needs more framework cleanup.

Suggested snippet:

- [src/App/Effects/Database.hs.md](src/App/Effects/Database.hs.md)
