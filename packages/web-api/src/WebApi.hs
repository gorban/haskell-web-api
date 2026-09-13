module WebApi
  ( buildApp,
    run,
    runDatabaseSetupArgs,
  )
where

import WebApi.App (buildApp, run)
import WebApi.DatabaseSetup (runDatabaseSetupArgs)
