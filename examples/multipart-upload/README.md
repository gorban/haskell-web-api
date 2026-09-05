# Multipart upload example

This focused example shows the configuration and lifecycle required for a
native multipart upload endpoint. It is deliberately separate from the
minimal two-pages example: uploads need an API middleware boundary, CSRF
policy, explicit storage ownership, and resource limits that are not
universal page features.

Run it with:

```sh
cabal run multipart-upload-example
```

Open <http://127.0.0.1:8080/native-upload>. The form is ordinary SSR HTML
with no `data-harch-action`, so browser JavaScript never reads file bytes and
the same form works with scripts disabled.

## Storage and cleanup

`App.MultipartUpload` uses `withMultipartRequestBodyWith`, which selects the
bounded in-memory adapter. Its maximum retained bytes come from
`defaultMultipartLimits`. File claims are opaque: an application deliberately promotes a
claim to retain it, or deliberately discards it. This example discards its
accepted upload because it has no durable ownership requirement.

To choose another backend, construct a `MultipartStorage` and pass it to the
lower-level `withMultipartBodyWith` after validating the request's media type
and boundary. For example, a local-filesystem adapter can be written as:

```haskell
import Data.ByteString qualified as ByteString
import HarchWeb.Api.Multipart
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)

localFileStorage :: FilePath -> MultipartStorage FilePath
localFileStorage directory =
  multipartStorage
    (\_untrustedFilename -> do
        (path, handle) <- openBinaryTempFile directory "multipart-upload"
        pure $
          multipartStagedUpload
            (ByteString.hPut handle)
            (hClose handle >> pure path)
            (hClose handle >> removeFile path)
    )
    (Just removeFile)

consumeUpload storage limits boundary readChunk onPart =
  withMultipartBodyWith storage limits boundary readChunk onPart
```

The filename is deliberately ignored when allocating a local file: it is
untrusted metadata, not a path. The completed path remains inaccessible until
`onPart` explicitly calls `withPromotedMultipartUpload` with the action that
adopts the completed value; otherwise the parser discards it after callback
completion, rejection, malformed input, or an exception. A WAI application can preserve the convenience boundary's
media-type, `Content-Length`, and boundary validation while selecting this
adapter:

```haskell
withMultipartRequestBodyWithStorage
  (localFileStorage uploadDirectory)
  limits
  request
  onPart
```

The CSRF field precedes the file field. The per-part callback validates it
before a later file is opened, so an invalid form is rejected without storing
that later file. Unclaimed staged and completed uploads are discarded on
malformed input, rejection, handler exceptions, request-body failures, and
scope exit.

The Unit suite additionally proves that a valid accepted file has already
been deliberately discarded if a later multipart part is malformed.

## Verification

```sh
cabal test multipart-upload-example-tests --test-show-details=direct --test-options='--skip E2E'
cabal test multipart-upload-example-tests --test-show-details=direct --test-options='--match multipart-upload'
```

The Unit suite covers the endpoint’s CSRF and malformed-body outcomes. The
real-browser tests prove the native form produces SSR success and explicitly
discards exactly one accepted in-memory upload with scripts enabled and
disabled.
