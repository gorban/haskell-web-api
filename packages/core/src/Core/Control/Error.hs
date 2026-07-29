module Core.Control.Error
  ( fromMaybeError,
    guardError,
    handleError,
    liftEitherWith,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (ExceptT), MonadError, runExceptT, throwError)
import Data.Bifunctor (first)

-- | Lift an optional value into an error-capable computation without losing
-- the reason that absence is invalid at this boundary.
fromMaybeError :: (MonadError error monad) => error -> Maybe value -> monad value
fromMaybeError errorValue = maybe (throwError errorValue) pure

-- | Stop an error-capable computation unless its required condition holds.
guardError :: (MonadError error monad) => error -> Bool -> monad ()
guardError errorValue condition = unless condition (throwError errorValue)

-- | Lift an effectful 'Either' while translating its error at the workflow
-- boundary. This keeps the source adapter's error distinct from the
-- workflow's domain error without spelling an 'ExceptT' constructor at every
-- use site.
liftEitherWith :: (Functor monad) => (sourceError -> error) -> monad (Either sourceError value) -> ExceptT error monad value
liftEitherWith mapError = ExceptT . fmap (first mapError)

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
