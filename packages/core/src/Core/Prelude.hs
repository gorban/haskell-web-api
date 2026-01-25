-- | Core.Prelude re-exports all core utilities.
--
-- This module provides a single import point for all shared utilities.
-- Names are chosen carefully to avoid conflicts with other packages.
module Core.Prelude (module X) where

import Core.Control.Error as X
import Core.System.Temp as X
