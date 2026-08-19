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
    initialPosts <- runDatabaseEffect effect ListPosts
    initialPosts
      `shouldBe` DatabaseResult (Right [Post "First"]) [DatabaseOperation "memory" "memory-list-posts" "memory://posts" Nothing Nothing]
    case databaseResultOperations initialPosts of
      [initialOperation] -> do
        databaseOperationStartedAtNanoseconds initialOperation `shouldBe` Nothing
        databaseOperationEndedAtNanoseconds initialOperation `shouldBe` Nothing
      operations -> expectationFailure ("expected one database operation, got " <> show operations)
    runDatabaseEffect effect (SavePost (Post "Second"))
      `shouldReturn` DatabaseResult (Right (Post "Second")) [DatabaseOperation "memory" "memory-save-post" "memory://posts" Nothing Nothing]
    fmap databaseResultValue (runDatabaseEffect effect ListPosts)
      `shouldReturn` Right [Post "First", Post "Second"]

  it "keeps adapter-specific failures typed" $ do
    database <- newMemoryDatabase [Post "First"]
    let effect = buildMemoryDatabaseEffect database
    fmap databaseResultValue (runDatabaseEffect effect (SavePost (Post "First")))
      `shouldReturn` Left (DuplicatePostTitle "First")

  it "renders its closed adapter values" $ do
    let firstPost = Post "First"
    postTitle firstPost `shouldBe` "First"
    show firstPost `shouldBe` "Post {postTitle = \"First\"}"
    firstPost `shouldNotBe` Post "Second"
    showList [firstPost] "" `shouldBe` "[Post {postTitle = \"First\"}]"
    show (DuplicatePostTitle "First") `shouldBe` "DuplicatePostTitle \"First\""
    DuplicatePostTitle "First" `shouldNotBe` DuplicatePostTitle "Second"
    showList [DuplicatePostTitle "First"] "" `shouldBe` "[DuplicatePostTitle \"First\"]"
