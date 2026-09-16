# src/App/Effects/Database.hs

```hs
module App.Effects.Database where

class Monad m => DatabaseEffect m where
  listPosts :: m [Post]
  savePost :: NewPost -> m Post

data SqliteDatabase = SqliteDatabase FilePath

runSqliteDatabase :: SqliteDatabase -> IO a -> IO a
runSqliteDatabase = undefined
```

The key design point is not SQLite specifically. It is that apps should be able to define a
compatible effect implementation without rewriting framework-owned routing/page code.
