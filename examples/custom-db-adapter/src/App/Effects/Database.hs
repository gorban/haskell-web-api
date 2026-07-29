{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Effects.Database
  ( BlogDatabaseError (..),
    BlogDatabaseRequest (..),
    Post (..),
    buildMemoryDatabaseEffect,
    newMemoryDatabase,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import HarchWeb.Database (DatabaseEffect (..), DatabaseOperation (..), DatabaseResult (..))

newtype Post = Post {postTitle :: Text} deriving (Eq, Show)

newtype BlogDatabaseError = DuplicatePostTitle Text deriving (Eq, Show)

data BlogDatabaseRequest result where
  ListPosts :: BlogDatabaseRequest [Post]
  SavePost :: Post -> BlogDatabaseRequest Post

newtype MemoryDatabase = MemoryDatabase (IORef [Post])

newMemoryDatabase :: [Post] -> IO MemoryDatabase
newMemoryDatabase = fmap MemoryDatabase . newIORef

buildMemoryDatabaseEffect :: MemoryDatabase -> DatabaseEffect BlogDatabaseError BlogDatabaseRequest
buildMemoryDatabaseEffect (MemoryDatabase postsReference) = DatabaseEffect $ \case
  ListPosts -> do
    posts <- readIORef postsReference
    pure (DatabaseResult (Right posts) [operation "memory-list-posts" "memory://posts"])
  SavePost post -> do
    saved <- atomicModifyIORef' postsReference $ \posts ->
      if any ((== postTitle post) . postTitle) posts
        then (posts, Left (DuplicatePostTitle (postTitle post)))
        else (posts <> [post], Right post)
    pure (DatabaseResult saved [operation "memory-save-post" "memory://posts"])

operation :: Text -> Text -> DatabaseOperation
operation name template = DatabaseOperation name template Nothing Nothing
