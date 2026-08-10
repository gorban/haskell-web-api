-- | Internal operations for the multipart parser. Applications construct
-- adapters through the public storage module, but only the parser receives
-- this operational view of its request-scoped staged handles.
module HarchWeb.Api.Multipart.Storage.Internal
  ( MultipartStorage (..),
    MultipartStagedUpload (..),
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)

data MultipartStorage stored = MultipartStorage
  { beginMultipartUpload :: Text -> IO (MultipartStagedUpload stored),
    discardCompletedMultipartUpload :: stored -> IO ()
  }

data MultipartStagedUpload stored = MultipartStagedUpload
  { appendMultipartUpload :: ByteString -> IO (),
    completeMultipartUpload :: IO stored,
    discardMultipartUpload :: IO ()
  }
