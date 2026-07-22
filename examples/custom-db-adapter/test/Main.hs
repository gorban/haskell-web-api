{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Effects.Database
import HarchWeb.Database
import Test.Hspec

main :: IO ()
main = hspec $ describe "Unit.memory database adapter" $ do
  it "interprets typed reads and writes without PostgreSQL" $ do
    database <- newMemoryDatabase [Post "First"]
    let effect = buildMemoryDatabaseEffect database
    runDatabaseEffect effect ListPosts
      `shouldReturn` DatabaseResult (Right [Post "First"]) [DatabaseOperation "memory-list-posts" "memory://posts" Nothing Nothing]
    runDatabaseEffect effect (SavePost (Post "Second"))
      `shouldReturn` DatabaseResult (Right (Post "Second")) [DatabaseOperation "memory-save-post" "memory://posts" Nothing Nothing]
    fmap databaseResultValue (runDatabaseEffect effect ListPosts)
      `shouldReturn` Right [Post "First", Post "Second"]

  it "keeps adapter-specific failures typed" $ do
    database <- newMemoryDatabase [Post "First"]
    let effect = buildMemoryDatabaseEffect database
    fmap databaseResultValue (runDatabaseEffect effect (SavePost (Post "First")))
      `shouldReturn` Left (DuplicatePostTitle "First")
