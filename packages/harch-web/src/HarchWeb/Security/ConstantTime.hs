-- | Private constant-work comparison for secret-derived byte strings.
--
-- Decision record (DM): this extends the existing security implementation
-- boundary rather than adding a public comparison API. Password verification,
-- email-verification digests, synchronizer tokens, and the client-action CSRF
-- check keep their domain-specific types and outcomes, but delegate the one
-- byte-level primitive here so changes cannot drift between copies. Length is
-- public at each of those protocol boundaries; bytes up to the shorter length
-- are compared without an early mismatch exit.
module HarchWeb.Security.ConstantTime
  ( constantWorkEquals,
  )
where

import Data.Bits (xor, (.|.))
import Data.ByteString qualified as ByteString

constantWorkEquals :: ByteString.ByteString -> ByteString.ByteString -> Bool
constantWorkEquals expected actual =
  let byteDifference =
        foldl'
          (.|.)
          0
          (ByteString.zipWith (\left right -> fromIntegral (left `xor` right)) expected actual)
      lengthDifference = ByteString.length expected `xor` ByteString.length actual
   in (byteDifference .|. lengthDifference) == 0
