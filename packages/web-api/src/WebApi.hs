module WebApi (run) where

import System.IO (Handle, hPutStrLn)

run :: Handle -> IO ()
run handle = hPutStrLn handle "Web API Module"
