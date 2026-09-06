{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private signed-CSRF backend. It owns signed-token parsing, issuance, and
-- verification; the shared page/action lifecycle remains in
-- 'HarchWeb.Csrf.Lifecycle'.
module HarchWeb.Csrf.Signed
  ( SignedCsrfDependencies (..),
    signedCsrfProtection,
  )
where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random.Entropy (getEntropy)
import Data.Bits (shiftL, (.|.))
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as ByteStringBuilder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Csrf.Lifecycle
  ( CsrfBinding (..),
    CsrfBindingDigest (..),
    CsrfBindingResolution (..),
    CsrfCookieMaxAgeSeconds (..),
    CsrfIssuance (..),
    CsrfKeyId (..),
    CsrfProtection (..),
    CsrfSigningKey (..),
    CsrfToken (..),
    CsrfVerification (..),
    SignedCsrfKeyring (..),
    SignedCsrfPolicy (..),
    csrfBindingBytes,
    csrfBindingFromCanonicalBytes,
    csrfTokenText,
    mkCsrfKeyId,
  )
import HarchWeb.Security.ConstantTime (constantWorkEquals)
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)

-- | Stable collaborators for the signed backend. Request context and the
-- submitted token remain explicit inputs to the issue/verify lifecycle.
data SignedCsrfDependencies context = SignedCsrfDependencies
  { signedCsrfDependenciesKeyring :: SignedCsrfKeyring,
    signedCsrfDependenciesPolicy :: SignedCsrfPolicy,
    signedCsrfDependenciesCurrentTime :: IO UnixTimeNanoseconds,
    signedCsrfDependenciesResolveBinding :: context -> IO CsrfBindingResolution
  }

signedCsrfProtection :: SignedCsrfDependencies context -> CsrfProtection context
signedCsrfProtection dependencies =
  CsrfProtection
    { issueCsrfToken = issueSignedToken dependencies,
      verifyCsrfToken = verifySignedToken dependencies
    }

issueSignedToken :: SignedCsrfDependencies context -> context -> IO CsrfIssuance
issueSignedToken dependencies context = do
  bindingResolution <- signedCsrfDependenciesResolveBinding dependencies context
  now <- signedCsrfDependenciesCurrentTime dependencies
  case expiryForBinding (signedCsrfDependenciesPolicy dependencies) now bindingResolution of
    Nothing -> pure CsrfProtectionUnavailable
    Just (binding, expiresAt) -> do
      nonce <- getEntropy csrfNonceBytes
      let keyring = signedCsrfDependenciesKeyring dependencies
          activeKey = signedCsrfActiveKey keyring
          payload = renderSignedPayload activeKey now expiresAt nonce binding
          mac = hmacSha256 (signedCsrfActiveSigningKey keyring) payload
          tokenText = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (payload <> mac))
      pure (CsrfTokenIssued (CsrfToken tokenText) (cookieMaxAgeUntil now expiresAt))

verifySignedToken :: SignedCsrfDependencies context -> context -> CsrfToken -> IO CsrfVerification
verifySignedToken dependencies context csrfToken =
  case parseSignedToken csrfToken of
    Nothing -> pure CsrfRejected
    Just parsedToken ->
      case lookupVerificationKey (parsedCsrfKeyId parsedToken) (signedCsrfVerificationKeys keyring) of
        Nothing -> pure CsrfRejected
        Just verificationKey -> do
          let expectedMac = hmacSha256 verificationKey (parsedCsrfPayload parsedToken)
          if not (constantWorkEquals expectedMac (parsedCsrfMac parsedToken))
            then pure CsrfRejected
            else do
              bindingResolution <- signedCsrfDependenciesResolveBinding dependencies context
              now <- signedCsrfDependenciesCurrentTime dependencies
              case bindingResolution of
                CsrfBindingUnavailable -> pure CsrfVerificationUnavailable
                BoundCsrfBinding _ expiresAt
                  | expiresAt <= now -> pure CsrfRejected
                _ ->
                  case expiryForBinding policy now bindingResolution of
                    Nothing -> pure CsrfVerificationUnavailable
                    Just (expectedBinding, bindingExpiry) ->
                      pure (verifyParsedToken policy now bindingExpiry expectedBinding parsedToken)
  where
    keyring = signedCsrfDependenciesKeyring dependencies
    policy = signedCsrfDependenciesPolicy dependencies

verifyParsedToken :: SignedCsrfPolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> CsrfBinding -> ParsedCsrfToken -> CsrfVerification
verifyParsedToken policy now bindingExpiry expectedBinding parsedToken
  | parsedCsrfExpiresAt parsedToken > bindingExpiry = CsrfRejected
  | isTooFarInFuture policy now (parsedCsrfIssuedAt parsedToken) = CsrfRejected
  | isExpired policy now (parsedCsrfExpiresAt parsedToken) = CsrfRejected
  | constantWorkEquals (csrfBindingBytes expectedBinding) (csrfBindingBytes (parsedCsrfBinding parsedToken)) = CsrfVerified
  | otherwise = CsrfRejected

expiryForBinding :: SignedCsrfPolicy -> UnixTimeNanoseconds -> CsrfBindingResolution -> Maybe (CsrfBinding, UnixTimeNanoseconds)
expiryForBinding policy now = \case
  AnonymousCsrfBinding -> do
    expiresAt <- addUnixTimeNanoseconds now (signedCsrfAnonymousLifetimeNanoseconds policy)
    pure (anonymousCsrfBinding, expiresAt)
  BoundCsrfBinding binding expiresAt
    | expiresAt > now -> Just (binding, expiresAt)
    | otherwise -> Nothing
  CsrfBindingUnavailable -> Nothing

data ParsedCsrfToken = ParsedCsrfToken
  { parsedCsrfKeyId :: CsrfKeyId,
    parsedCsrfIssuedAt :: UnixTimeNanoseconds,
    parsedCsrfExpiresAt :: UnixTimeNanoseconds,
    parsedCsrfBinding :: CsrfBinding,
    parsedCsrfPayload :: ByteString.ByteString,
    parsedCsrfMac :: ByteString.ByteString
  }

parseSignedToken :: CsrfToken -> Maybe ParsedCsrfToken
parseSignedToken token
  | Text.length (csrfTokenText token) > maxSignedCsrfTokenCharacters = Nothing
  | otherwise = do
      tokenBytes <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 (csrfTokenText token)))
      if ByteString.length tokenBytes > maxSignedCsrfTokenBytes
        then Nothing
        else do
          let fixedPrefixBytes = ByteString.length signedCsrfMagic + 1
              keyLength = fromIntegral (ByteString.index tokenBytes (ByteString.length signedCsrfMagic))
              payloadLength = fixedPrefixBytes + keyLength + 8 + 8 + csrfNonceBytes + csrfBindingBytesLength
              totalLength = payloadLength + csrfMacBytes
          if keyLength == 0 || ByteString.length tokenBytes /= totalLength
            then Nothing
            else do
              let (payload, mac) = ByteString.splitAt payloadLength tokenBytes
                  keyStart = fixedPrefixBytes
                  keyBytes = ByteString.take keyLength (ByteString.drop keyStart payload)
                  issuedAtOffset = keyStart + keyLength
                  expiresAtOffset = issuedAtOffset + 8
                  bindingOffset = expiresAtOffset + 8 + csrfNonceBytes
              keyIdText <- either (const Nothing) Just (TextEncoding.decodeUtf8' keyBytes)
              keyId <- mkCsrfKeyId keyIdText
              if ByteString.take (ByteString.length signedCsrfMagic) payload /= signedCsrfMagic
                then Nothing
                else
                  Just
                    ParsedCsrfToken
                      { parsedCsrfKeyId = keyId,
                        parsedCsrfIssuedAt = unixTimeNanoseconds (word64At payload issuedAtOffset),
                        parsedCsrfExpiresAt = unixTimeNanoseconds (word64At payload expiresAtOffset),
                        parsedCsrfBinding = CsrfBinding (CsrfBindingDigest (ByteString.take csrfBindingBytesLength (ByteString.drop bindingOffset payload))),
                        parsedCsrfPayload = payload,
                        parsedCsrfMac = mac
                      }

renderSignedPayload :: CsrfKeyId -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> ByteString.ByteString -> CsrfBinding -> ByteString.ByteString
renderSignedPayload keyId issuedAt expiresAt nonce binding =
  LazyByteString.toStrict
    ( ByteStringBuilder.toLazyByteString
        (ByteStringBuilder.byteString signedCsrfMagic <> ByteStringBuilder.word8 (fromIntegral (ByteString.length keyIdBytes)) <> ByteStringBuilder.byteString keyIdBytes <> ByteStringBuilder.word64BE (unixTimeNanosecondsValue issuedAt) <> ByteStringBuilder.word64BE (unixTimeNanosecondsValue expiresAt) <> ByteStringBuilder.byteString nonce <> ByteStringBuilder.byteString (csrfBindingBytes binding))
    )
  where
    keyIdBytes = csrfKeyIdBytes keyId

lookupVerificationKey :: CsrfKeyId -> NonEmpty.NonEmpty (CsrfKeyId, CsrfSigningKey) -> Maybe CsrfSigningKey
lookupVerificationKey keyId = fmap snd . find ((== keyId) . fst) . NonEmpty.toList

anonymousCsrfBinding :: CsrfBinding
anonymousCsrfBinding = csrfBindingFromCanonicalBytes "harch-csrf-anonymous-v1"

csrfKeyIdBytes :: CsrfKeyId -> ByteString.ByteString
csrfKeyIdBytes (CsrfKeyId value) = TextEncoding.encodeUtf8 value

hmacSha256 :: CsrfSigningKey -> ByteString.ByteString -> ByteString.ByteString
hmacSha256 (CsrfSigningKey key) payload = convert (hmac key payload :: HMAC SHA256)

isTooFarInFuture :: SignedCsrfPolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> Bool
isTooFarInFuture policy now issuedAt = maybe False (issuedAt >) (addUnixTimeNanoseconds now (signedCsrfClockSkewNanoseconds policy))

isExpired :: SignedCsrfPolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> Bool
isExpired policy now expiresAt = maybe False (now >) (addUnixTimeNanoseconds expiresAt (signedCsrfClockSkewNanoseconds policy))

cookieMaxAgeUntil :: UnixTimeNanoseconds -> UnixTimeNanoseconds -> CsrfCookieMaxAgeSeconds
cookieMaxAgeUntil now expiresAt = CsrfCookieMaxAgeSeconds ((unixTimeNanosecondsValue expiresAt - unixTimeNanosecondsValue now) `div` nanosecondsPerSecond)

word64At :: ByteString.ByteString -> Int -> Word64
word64At bytes offset = foldl (\value index -> value `shiftL` 8 .|. fromIntegral (ByteString.index bytes (offset + index))) 0 [0 .. 7]

signedCsrfMagic :: ByteString.ByteString
signedCsrfMagic = "HCS1"

csrfNonceBytes, csrfBindingBytesLength, csrfMacBytes, maxCsrfKeyIdCharacters, maxSignedCsrfTokenBytes, maxSignedCsrfTokenCharacters :: Int
csrfNonceBytes = 32
csrfBindingBytesLength = 32
csrfMacBytes = 32
maxCsrfKeyIdCharacters = 32
maxSignedCsrfTokenBytes = ByteString.length signedCsrfMagic + 1 + maxCsrfKeyIdCharacters + 8 + 8 + csrfNonceBytes + csrfBindingBytesLength + csrfMacBytes
maxSignedCsrfTokenCharacters = 256

nanosecondsPerSecond :: Word64
nanosecondsPerSecond = 1000000000
