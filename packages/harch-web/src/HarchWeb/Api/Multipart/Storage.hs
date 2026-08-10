-- | Storage ownership for a multipart file part. A backend creates a staged
-- upload, accepts bounded chunks, and either completes or discards it. The
-- parser owns a staged upload until it emits a completed part; applications
-- choose any durable promotion separately.
module HarchWeb.Api.Multipart.Storage
  ( MultipartStorage,
    MultipartStagedUpload,
    multipartStorage,
    multipartStagedUpload,
    InMemoryUpload,
    inMemoryMultipartStorage,
    inMemoryUploadBytes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef qualified as IORef
import Data.Text (Text)
import HarchWeb.Api.Multipart.Storage.Internal (MultipartStagedUpload, MultipartStorage)
import HarchWeb.Api.Multipart.Storage.Internal qualified as Internal

-- | Construct an application-selected storage adapter. The callback receives
-- untrusted filename metadata only as a naming hint; it must not use it as a
-- filesystem path or object key without application validation.
multipartStorage ::
  (Text -> IO (MultipartStagedUpload stored)) ->
  -- | Discard a completed upload that has not been deliberately adopted.
  -- 'Nothing' is valid only when completed values own no releasable resource,
  -- as with the built-in in-memory adapter.
  Maybe (stored -> IO ()) ->
  MultipartStorage stored
multipartStorage = Internal.MultipartStorage

-- | Construct a request-scoped staged upload for 'multipartStorage'. The
-- multipart parser, rather than the application callback, owns its append,
-- completion, and discard operations.
multipartStagedUpload :: (ByteString -> IO ()) -> IO stored -> IO () -> MultipartStagedUpload stored
multipartStagedUpload = Internal.MultipartStagedUpload

newtype InMemoryUpload = InMemoryUpload ByteString

-- | The small-upload backend supplied by the framework. The multipart
-- consumer checks its file byte budget before each append, so this backend
-- cannot retain more than the selected 'MultipartLimits' file maximum.
inMemoryMultipartStorage :: MultipartStorage InMemoryUpload
inMemoryMultipartStorage =
  multipartStorage
    ( \_filenameHint -> do
        chunksReference <- IORef.newIORef []
        pure $
          multipartStagedUpload
            (\chunk -> IORef.modifyIORef' chunksReference (chunk :))
            (InMemoryUpload . ByteString.concat . reverse <$> IORef.readIORef chunksReference)
            (IORef.modifyIORef' chunksReference (const []))
    )
    Nothing

inMemoryUploadBytes :: InMemoryUpload -> ByteString
inMemoryUploadBytes (InMemoryUpload uploadBytes) = uploadBytes
