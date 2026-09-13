module Main (main) where

import App.App (newMultipartUploadApplication)
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = newMultipartUploadApplication >>= Warp.run 8080
