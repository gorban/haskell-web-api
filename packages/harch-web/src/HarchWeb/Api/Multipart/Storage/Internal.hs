-- | Internal operations for the multipart parser. Applications construct
-- adapters through the public storage module, but only the parser receives
-- this operational view of its request-scoped staged handles.
module HarchWeb.Api.Multipart.Storage.Internal
  ( MultipartStorage (..),
    MultipartStagedUpload (..),
    UntrustedFilename,
    untrustedFilenameFromText,
    untrustedFilenameText,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)

-- | Filename metadata supplied by a multipart client.  This deliberately has
-- no validation constructor: a client filename is display metadata, not a
-- portable filesystem path or object key.  Only the parser may introduce the
-- value; adapters must make an explicit call to 'untrustedFilenameText'
-- before handing it to a storage-specific naming policy.
newtype UntrustedFilename = UntrustedFilename Text
  deriving (Eq)

untrustedFilenameFromText :: Text -> UntrustedFilename
untrustedFilenameFromText = UntrustedFilename

untrustedFilenameText :: UntrustedFilename -> Text
untrustedFilenameText (UntrustedFilename filename) = filename

data MultipartStorage stored = MultipartStorage
  { beginMultipartUpload :: UntrustedFilename -> IO (MultipartStagedUpload stored),
    discardCompletedMultipartUpload :: Maybe (stored -> IO ())
  }

data MultipartStagedUpload stored = MultipartStagedUpload
  { appendMultipartUpload :: ByteString -> IO (),
    completeMultipartUpload :: IO stored,
    discardMultipartUpload :: IO ()
  }
