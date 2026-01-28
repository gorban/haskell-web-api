{-# SPEC #-}

import Control.Monad (forM_)
import Core.System.Path (normalizePath)

spec = describe "normalizePath" $ do
  [ '\\',
    '/'
    ]
    `forM_` \slashDirection ->
      it ("converts (" ++ [slashDirection] ++ ") to forward slashes") $ do
        normalizePath ("foo" ++ [slashDirection] ++ "bar" ++ [slashDirection] ++ "baz")
          `shouldBe` "foo/bar/baz"

  it "leaves strings without slashes unchanged" $ do
    normalizePath "filename.txt" `shouldBe` "filename.txt"
