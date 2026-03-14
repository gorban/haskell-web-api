{-# SPEC #-}

import Control.Exception (evaluate)
import System.IO (BufferMode (LineBuffering), SeekMode (AbsoluteSeek), hFlush, hGetContents, hSeek, hSetBuffering)
import System.IO.Temp (withSystemTempFile)
import WebApi (run)

spec = describe "run" $ do
  it "writes the banner to a provided handle" $
    withSystemTempFile "web-api" $ \_ handle -> do
      hSetBuffering handle LineBuffering
      run handle
      hFlush handle
      hSeek handle AbsoluteSeek 0
      contents <- hGetContents handle
      _ <- evaluate (length contents)
      contents `shouldBe` "Web API Module\n"
