module WebApi.Postgres
  ( PostgresPool,
    buildPostgresPageRepository,
    buildRuntimePostgresPageRepository,
    closePostgresPool,
    newPostgresPool,
  )
where

import WebApi.Postgres.Pool (PostgresPool, closePostgresPool, newPostgresPool)
import WebApi.Postgres.Runtime
  ( buildPostgresPageRepository,
    buildRuntimePostgresPageRepository,
  )
