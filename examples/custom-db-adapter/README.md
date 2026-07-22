# custom-db-adapter

**Status:** Working

Document the desired seam for replacing the default PostgreSQL-backed adapter with another database
technology such as SQLite.

This package is now a buildable alternative interpreter over the framework-owned
`HarchWeb.Database` contract. It uses an in-memory store so the adapter shape is
testable without a second database server; a SQLite adapter would implement the
same `BlogDatabaseRequest` GADT and return the same typed `DatabaseResult`.

Run its focused tests from the repository root:

```bash
cabal test custom-db-adapter-tests --test-show-details=direct
```

Current repo alignment:

- there is already a clear database seam in the example app,
- PostgreSQL remains the production adapter, while this example proves that its
  operation algebra and routing code are not coupled to it.

The implementation is in [src/App/Effects/Database.hs](src/App/Effects/Database.hs).
