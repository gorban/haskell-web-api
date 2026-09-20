{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Bounded libpq execution for the composed application's durable adapters.
--
-- The root creates one runtime value for its lifetime and shares its one
-- connection serially.  This is deliberate for the reference deployment:
-- every statement is fixed application SQL with separately encoded libpq
-- parameters, while the single connection bounds database-connection growth
-- independently of request concurrency.  The application owns this resource
-- and closes it during server shutdown; neither Harch nor a domain module
-- receives a connection or a raw-query escape hatch.
module App.Composed.Postgres.Runtime
  ( ComposedDatabaseRuntime,
    closeComposedDatabaseRuntime,
    newComposedDatabaseRuntime,
    runComposedDatabaseQuery,
  )
where

import App.Composed.Postgres (ComposedDatabaseConnectionString (..))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, withMVar)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Database.PostgreSQL.LibPQ qualified as LibPQ

-- | An opaque, bounded database capability.  It intentionally has no 'Show'
-- instance because its live connection can carry deployment credentials.
newtype ComposedDatabaseRuntime = ComposedDatabaseRuntime (MVar (Maybe LibPQ.Connection))

newComposedDatabaseRuntime :: ComposedDatabaseConnectionString -> IO ComposedDatabaseRuntime
newComposedDatabaseRuntime (ComposedDatabaseConnectionString connectionString) =
  ComposedDatabaseRuntime <$> (newMVar . Just =<< LibPQ.connectdb connectionString)

-- | Finish the owned connection.  The MVar makes shutdown wait for any query
-- already in progress; calls after closure fail on the ordinary unavailable
-- rail rather than reconnecting unexpectedly with stale credentials.
closeComposedDatabaseRuntime :: ComposedDatabaseRuntime -> IO ()
closeComposedDatabaseRuntime (ComposedDatabaseRuntime runtime) =
  modifyMVar_ runtime $ \case
    Nothing -> pure Nothing
    Just connection -> LibPQ.finish connection >> pure Nothing

-- | Execute only application-owned SQL with libpq parameters.  Database and
-- protocol diagnostics are intentionally collapsed: the durable adapters map
-- this result to their typed unavailable/corrupt rails and must not expose a
-- server message, SQL text, or deployed connection details.
runComposedDatabaseQuery :: ComposedDatabaseRuntime -> Text -> [Text] -> IO (Either Text [[Text]])
runComposedDatabaseQuery (ComposedDatabaseRuntime runtime) sql parameters =
  withMVar runtime $ \case
    Nothing -> pure (Left "database unavailable")
    Just connection -> do
      maybeResult <-
        LibPQ.execParams
          connection
          (TextEncoding.encodeUtf8 sql)
          (fmap parameterValue parameters)
          LibPQ.Text
      case maybeResult of
        Nothing -> pure (Left "database unavailable")
        Just result -> do
          status <- LibPQ.resultStatus result
          if status == LibPQ.TuplesOk
            then readRows result
            else pure (Left "database unavailable")

parameterValue :: Text -> Maybe (LibPQ.Oid, ByteString.ByteString, LibPQ.Format)
parameterValue value = Just (LibPQ.Oid 0, TextEncoding.encodeUtf8 value, LibPQ.Text)

readRows :: LibPQ.Result -> IO (Either Text [[Text]])
readRows result = do
  rowCount <- LibPQ.ntuples result
  fieldCount <- LibPQ.nfields result
  rows <-
    traverse
      (\row -> traverse (readField result row) [0 .. fieldCount - 1])
      [0 .. rowCount - 1]
  pure (mapM sequence rows)

readField :: LibPQ.Result -> LibPQ.Row -> LibPQ.Column -> IO (Either Text Text)
readField result row column =
  fmap (maybe (Left "database result is malformed") (Right . TextEncoding.decodeUtf8With lenientDecode)) (LibPQ.getvalue result row column)
