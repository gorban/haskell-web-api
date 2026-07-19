{-# LANGUAGE RankNTypes #-}

module HarchWeb.Database
  ( DatabaseEffect (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    databaseFailure,
    databaseSuccess,
  )
where

import Data.Text (Text)
import Data.Word (Word64)

-- | An application-owned interpreter for a typed database operation algebra.
-- The operation determines its result type, so callers cannot accidentally
-- decode one query as another query's result.
newtype DatabaseEffect databaseError operation = DatabaseEffect
  { runDatabaseEffect :: forall result. operation result -> IO (DatabaseResult databaseError result)
  }

-- | Stable metadata for a database operation. Timings are deliberately
-- excluded from equality and display output so tests and logs stay stable.
data DatabaseOperation = DatabaseOperation
  { databaseOperationName :: Text,
    databaseQueryTemplate :: Text,
    databaseOperationStartedAtNanoseconds :: Maybe Word64,
    databaseOperationEndedAtNanoseconds :: Maybe Word64
  }

instance Eq DatabaseOperation where
  left == right =
    databaseOperationName left == databaseOperationName right
      && databaseQueryTemplate left == databaseQueryTemplate right

instance Show DatabaseOperation where
  showsPrec precedence databaseOperation =
    showParen (precedence > 10) $
      showString "DatabaseOperation {databaseOperationName = "
        . shows (databaseOperationName databaseOperation)
        . showString ", databaseQueryTemplate = "
        . shows (databaseQueryTemplate databaseOperation)
        . showString "}"

-- | A typed result together with the operations performed to obtain it.
-- Applications choose their own domain-specific error type.
data DatabaseResult databaseError result = DatabaseResult
  { databaseResultValue :: Either databaseError result,
    databaseResultOperations :: [DatabaseOperation]
  }
  deriving (Eq, Show)

databaseSuccess :: result -> DatabaseResult databaseError result
databaseSuccess result =
  DatabaseResult
    { databaseResultValue = Right result,
      databaseResultOperations = []
    }

databaseFailure :: databaseError -> DatabaseResult databaseError result
databaseFailure databaseError =
  DatabaseResult
    { databaseResultValue = Left databaseError,
      databaseResultOperations = []
    }
