{-# LANGUAGE OverloadedStrings #-}

-- | Which peers a deployment trusts to supply proxy-forwarded request
-- context (@X-Forwarded-For@\/@-Proto@\/@-Prefix@, RFC 7239 @Forwarded@).
-- Split out of "HarchWeb.Security" as a genuinely disjoint concern (CIDR
-- parsing\/matching, not response headers or CORS) — see the DE decision
-- record in @docs/design-guidance.md@.
module HarchWeb.Security.ForwardedTrust
  ( ForwardedHeaderTrust (..),
    CidrBlock,
    parseCidrBlock,
    cidrBlockText,
    isTrustedForwardingPeer,
  )
where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word32, Word8)
import Network.Socket qualified as Socket
import Text.Read (readMaybe)

-- | Which peers, if any, a deployment trusts to supply proxy-forwarded
-- request context. 'NeverTrustForwarded' is the safe default: every
-- @X-Forwarded-*@\/@Forwarded@ header is ignored regardless of who sends it.
--
-- 'Eq' and 'Show' are hand-written rather than derived: a GHC 9.14.1 @-O2@
-- HPC instrumentation quirk leaves parts of a *derived* instance's generated
-- Core permanently unticked regardless of how thoroughly it is exercised
-- (confirmed non-deterministic across otherwise-identical rebuilds — adding
-- more covering tests moved which sub-expressions showed unticked rather
-- than closing the gap). Ordinary hand-written functions do not have this
-- problem and are covered the normal way below.
data ForwardedHeaderTrust
  = NeverTrustForwarded
  | TrustForwardedFrom (NonEmpty CidrBlock)

instance Eq ForwardedHeaderTrust where
  NeverTrustForwarded == NeverTrustForwarded = True
  TrustForwardedFrom leftBlocks == TrustForwardedFrom rightBlocks = leftBlocks == rightBlocks
  NeverTrustForwarded == TrustForwardedFrom _ = False
  TrustForwardedFrom _ == NeverTrustForwarded = False

instance Show ForwardedHeaderTrust where
  showsPrec _ NeverTrustForwarded = showString "NeverTrustForwarded"
  showsPrec precedence (TrustForwardedFrom cidrBlocks) =
    showParen (precedence > 10) $
      showString "TrustForwardedFrom " . showsPrec 11 cidrBlocks

-- | An IPv4 CIDR block (for example @10.0.0.0\/8@), stored with its host
-- bits already masked off so two blocks naming the same network compare
-- equal regardless of how their address was written. 'Eq' and 'Show' are
-- hand-written for the same HPC-instrumentation reason as
-- 'ForwardedHeaderTrust' above.
data CidrBlock = CidrBlock
  { cidrBlockNetworkAddress :: Word32,
    cidrBlockPrefixLength :: Word8
  }

instance Eq CidrBlock where
  leftBlock == rightBlock =
    cidrBlockNetworkAddress leftBlock == cidrBlockNetworkAddress rightBlock
      && cidrBlockPrefixLength leftBlock == cidrBlockPrefixLength rightBlock

instance Show CidrBlock where
  showsPrec precedence cidrBlock =
    showParen (precedence > 10) $
      showString "CidrBlock {cidrBlockNetworkAddress = "
        . shows (cidrBlockNetworkAddress cidrBlock)
        . showString ", cidrBlockPrefixLength = "
        . shows (cidrBlockPrefixLength cidrBlock)
        . showString "}"
  show cidrBlock = shows cidrBlock ""

-- | Parses @"a.b.c.d/prefixLength"@. Rejects a malformed dotted quad, an
-- out-of-range octet, and a prefix length outside @0-32@.
parseCidrBlock :: Text -> Maybe CidrBlock
parseCidrBlock cidrText =
  case Text.splitOn "/" cidrText of
    [addressText, prefixLengthText] -> do
      networkAddress <- parseDottedQuad addressText
      prefixLength <- parseBoundedNatural 32 prefixLengthText
      Just (CidrBlock (applyPrefixMask prefixLength networkAddress) prefixLength)
    _ -> Nothing

-- | Renders a 'CidrBlock' back to @"a.b.c.d/prefixLength"@ form, for
-- diagnostics and round-trip testing.
cidrBlockText :: CidrBlock -> Text
cidrBlockText cidrBlock =
  dottedQuadText (cidrBlockNetworkAddress cidrBlock) <> "/" <> Text.pack (show (cidrBlockPrefixLength cidrBlock))

parseDottedQuad :: Text -> Maybe Word32
parseDottedQuad addressText =
  case Text.splitOn "." addressText of
    [a, b, c, d] -> packOctets <$> parseOctet a <*> parseOctet b <*> parseOctet c <*> parseOctet d
    _ -> Nothing

parseOctet :: Text -> Maybe Word8
parseOctet octetText = fromIntegral <$> parseBoundedNatural 255 octetText

-- | Parses a decimal natural number no greater than the given bound,
-- rejecting it otherwise. Reading straight into a fixed-width type here
-- would silently wrap an out-of-range value (@Word8@'s 'Text.Read.readMaybe'
-- accepts @"256"@ as @0@ and even @"-1"@ as @255@) instead of rejecting it.
parseBoundedNatural :: Int -> Text -> Maybe Word8
parseBoundedNatural upperBound valueText = do
  value <- readMaybe (Text.unpack valueText) :: Maybe Int
  if value >= 0 && value <= upperBound
    then Just (fromIntegral value)
    else Nothing

packOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
packOctets a b c d =
  (fromIntegral a `shiftL` 24) + (fromIntegral b `shiftL` 16) + (fromIntegral c `shiftL` 8) + fromIntegral d

dottedQuadText :: Word32 -> Text
dottedQuadText address =
  Text.intercalate "." (map (Text.pack . show) [octetAt 24, octetAt 16, octetAt 8, octetAt 0])
  where
    octetAt shiftAmount = (address `shiftR` shiftAmount) .&. 0xFF

applyPrefixMask :: Word8 -> Word32 -> Word32
applyPrefixMask prefixLength address = address .&. prefixMask prefixLength

prefixMask :: Word8 -> Word32
prefixMask prefixLength
  | prefixLength >= 32 = maxBound
  | prefixLength == 0 = 0
  | otherwise = maxBound `shiftL` fromIntegral (32 - prefixLength)

-- | The single choke point every proxy-forwarded header trust decision goes
-- through: the *actual* TCP peer (never client-supplied) must fall within a
-- configured trusted block before any @X-Forwarded-*@\/@Forwarded@ value is
-- read at all.
isTrustedForwardingPeer :: ForwardedHeaderTrust -> Socket.SockAddr -> Bool
isTrustedForwardingPeer forwardedHeaderTrust peerAddress =
  case forwardedHeaderTrust of
    NeverTrustForwarded -> False
    TrustForwardedFrom cidrBlocks ->
      case peerAddress of
        Socket.SockAddrInet _ hostAddress ->
          any (matchesHostAddress (tupleToWord32 (Socket.hostAddressToTuple hostAddress))) (NonEmpty.toList cidrBlocks)
        -- A Unix-domain-socket peer cannot be spoofed by a remote client at
        -- all: only a process with filesystem permission on this socket
        -- path can connect, a stronger guarantee than any IPv4 CIDR check
        -- could give. Once a deployment has opted into trusting *some*
        -- forwarding peer, that local reverse proxy is trusted too.
        Socket.SockAddrUnix _ -> True
        _ -> False

tupleToWord32 :: (Word8, Word8, Word8, Word8) -> Word32
tupleToWord32 (a, b, c, d) = packOctets a b c d

matchesHostAddress :: Word32 -> CidrBlock -> Bool
matchesHostAddress hostAddress cidrBlock =
  applyPrefixMask (cidrBlockPrefixLength cidrBlock) hostAddress == cidrBlockNetworkAddress cidrBlock
