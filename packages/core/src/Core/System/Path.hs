module Core.System.Path (normalizePath) where

-- | Convert Windows backslashes to forward slashes for pragma-safe paths.
normalizePath :: String -> String
normalizePath = map replaceBackslash
  where
    replaceBackslash '\\' = '/'
    replaceBackslash c = c
