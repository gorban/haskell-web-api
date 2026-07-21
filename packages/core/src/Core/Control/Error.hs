module Core.Control.Error
  ( fromMaybeError,
    guardError,
    handleError,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)

-- | Lift an optional value into an error-capable computation without losing
-- the reason that absence is invalid at this boundary.
fromMaybeError :: (MonadError error monad) => error -> Maybe value -> monad value
fromMaybeError errorValue = maybe (throwError errorValue) pure

-- | Stop an error-capable computation unless its required condition holds.
guardError :: (MonadError error monad) => error -> Bool -> monad ()
guardError errorValue condition = unless condition (throwError errorValue)

-- | Run an 'ExceptT' computation and handle the error case.
--
-- Similar to Rust's @unwrap_or_else@, this extracts the value from an 'ExceptT'
-- computation, calling the provided handler if the computation fails.
--
-- Example:
--
-- @
-- main :: IO ()
-- main = do
--   args <- getArgs
--   run args \`handleError\` die
-- @
handleError :: (Monad m) => ExceptT e m a -> (e -> m a) -> m a
handleError action handler = runExceptT action >>= either handler pure
