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
`defaultMultipartLimits`; a production application should supply its chosen
`MultipartStorage` adapter when it needs disk, object storage, or a scanning
quarantine. File claims are opaque: an application deliberately promotes a
claim to retain it, or deliberately discards it. This example discards its
accepted upload because it has no durable ownership requirement.

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
