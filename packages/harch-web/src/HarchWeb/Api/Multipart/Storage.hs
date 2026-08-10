-- | Storage ownership for a multipart file part. A backend creates a staged
-- upload, accepts bounded chunks, and either completes or discards it. The
-- parser owns a staged upload until it emits a completed part; applications
-- choose any durable promotion separately.
module HarchWeb.Api.Multipart.Storage
  ( MultipartStorage (..),
    MultipartStagedUpload (..),
    InMemoryUpload,
    inMemoryMultipartStorage,
    inMemoryUploadBytes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef qualified as IORef
import Data.Text (Text)

newtype MultipartStorage stored = MultipartStorage
  { beginMultipartUpload :: Text -> IO (MultipartStagedUpload stored)
  }

data MultipartStagedUpload stored = MultipartStagedUpload
  { appendMultipartUpload :: ByteString -> IO (),
    completeMultipartUpload :: IO stored,
    discardMultipartUpload :: IO ()
  }

newtype InMemoryUpload = InMemoryUpload ByteString
  deriving (Eq, Show)

-- | The small-upload backend supplied by the framework. The multipart
-- consumer checks its file byte budget before each append, so this backend
-- cannot retain more than the selected 'MultipartLimits' file maximum.
inMemoryMultipartStorage :: MultipartStorage InMemoryUpload
inMemoryMultipartStorage =
  MultipartStorage $ \_filenameHint -> do
    chunksReference <- IORef.newIORef []
    pure
      MultipartStagedUpload
        { appendMultipartUpload = \chunk -> IORef.modifyIORef' chunksReference (chunk :),
          completeMultipartUpload = InMemoryUpload . ByteString.concat . reverse <$> IORef.readIORef chunksReference,
          discardMultipartUpload = IORef.writeIORef chunksReference []
        }

inMemoryUploadBytes :: InMemoryUpload -> ByteString
inMemoryUploadBytes (InMemoryUpload uploadBytes) = uploadBytes
