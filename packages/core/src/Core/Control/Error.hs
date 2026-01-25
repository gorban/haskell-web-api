module Core.Control.Error (handleError) where

import Control.Monad.Except (ExceptT, runExceptT)

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
handleError :: Monad m => ExceptT e m a -> (e -> m a) -> m a
handleError action handler = runExceptT action >>= either handler pure
