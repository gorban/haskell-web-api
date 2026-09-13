module WebApi.Postgres.QueryRunner
  ( PageQueryRunner (..),
  )
where

import Data.Text (Text)

-- | The private, transport-neutral capability required to load page content.
data PageQueryRunner = PageQueryRunner
  { runRequiredTextQuery :: Text -> IO (Either Text Text),
    runTextRowsQuery :: Text -> IO (Either Text [Text])
  }
