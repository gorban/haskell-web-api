{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ForwardedTrustSpec (spec) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HarchWeb.Security
import Network.Socket qualified as Socket
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

-- | Each row is one previously-separate 'it' case, tabled per
-- @docs/design-guidance.md@'s CN decision record: one act
-- ('parseCidrBlock'), one comparison ('shouldBe'), differing only in the
-- input string and expected network. This is "Shape A" — each row keeps its
-- own name, so nothing is lost from the hspec output; the three cases that
-- used to be bundled two-per-'it' via 'expectAll' are now reported
-- separately instead.
parseCidrBlockCases :: [(String, Text, Maybe Text)]
parseCidrBlockCases =
  [ ("round-trips a canonical dotted-quad network address and prefix length", "10.0.0.0/8", Just "10.0.0.0/8"),
    ("masks off host bits so a non-canonical address still parses to its network", "10.1.2.3/8", Just "10.0.0.0/8"),
    ("accepts the maximum prefix length", "192.168.1.5/32", Just "192.168.1.5/32"),
    ("accepts the minimum prefix length", "0.0.0.0/0", Just "0.0.0.0/0"),
    ("rejects a prefix length outside 0-32", "10.0.0.0/33", Nothing),
    ("rejects a non-numeric prefix length", "10.0.0.0/eight", Nothing),
    ("rejects a missing prefix length", "10.0.0.0", Nothing),
    ("rejects an out-of-range high octet instead of silently wrapping it", "10.0.0.256/8", Nothing),
    ("rejects a negative octet instead of silently wrapping it", "10.0.0.-1/8", Nothing),
    ("rejects an out-of-range prefix length instead of silently wrapping it", "10.0.0.0/288", Nothing),
    ("rejects a dotted quad with too few segments", "10.0.0/8", Nothing),
    ("rejects a dotted quad with too many segments", "10.0.0.0.1/8", Nothing),
    ("rejects a non-numeric octet", "10.0.0.x/8", Nothing)
  ]

spec :: Spec
spec = do
  describe "parseCidrBlock" $ do
    parseCidrBlockCases `forM_` \(label, input, expected) ->
      it label $
        fmap cidrBlockText (parseCidrBlock input) `shouldBe` expected

    it "derives comparable Eq and printable Show instances" $
      let firstBlock = fromMaybe (error "expected a valid test CIDR block") (parseCidrBlock "10.0.0.0/8")
          secondBlock = fromMaybe (error "expected a valid test CIDR block") (parseCidrBlock "172.16.0.0/12")
       in expectAll
            ( ((parseCidrBlock "10.0.0.0/8" == parseCidrBlock "10.0.0.0/8") `shouldBe` True)
                :| [ (parseCidrBlock "10.0.0.0/8" == parseCidrBlock "172.16.0.0/12") `shouldBe` False,
                     (parseCidrBlock "10.0.0.0/8" == parseCidrBlock "10.0.0.0/16") `shouldBe` False,
                     (firstBlock /= secondBlock) `shouldBe` True,
                     -- A bare 'show' call, exercising CidrBlock's own explicit
                     -- 'show' method (distinct from 'showsPrec', which every
                     -- other assertion here reaches only indirectly).
                     show firstBlock `shouldBe` "CidrBlock {cidrBlockNetworkAddress = 167772160, cidrBlockPrefixLength = 8}",
                     -- 'show' on the 'Maybe' wrapper (not 'fmap show') so the nested
                     -- 'CidrBlock' renders at precedence 11, exercising the
                     -- Show's parenthesization branch that a bare 'show' never reaches.
                     show (parseCidrBlock "10.0.0.0/8")
                       `shouldBe` "Just (CidrBlock {cidrBlockNetworkAddress = 167772160, cidrBlockPrefixLength = 8})",
                     -- 'show' on a list exercises the Show class's default
                     -- 'showList' method, which a bare or Maybe-wrapped 'show'
                     -- never reaches.
                     show [firstBlock, secondBlock]
                       `shouldBe` "[CidrBlock {cidrBlockNetworkAddress = 167772160, cidrBlockPrefixLength = 8},CidrBlock {cidrBlockNetworkAddress = 2886729728, cidrBlockPrefixLength = 12}]"
                   ]
            )

  describe "isTrustedForwardingPeer" $ do
    let trustedBlock = fromMaybe (error "expected a valid test CIDR block") (parseCidrBlock "10.0.0.0/8")
        proxyTrust = TrustForwardedFrom (trustedBlock :| [])

    it "never trusts any peer when forwarding is disabled" $
      expectAll
        ( (isTrustedForwardingPeer NeverTrustForwarded (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (10, 0, 0, 1))) `shouldBe` False)
            :| [isTrustedForwardingPeer NeverTrustForwarded (Socket.SockAddrUnix "/tmp/harch-web.sock") `shouldBe` False]
        )

    it "trusts a peer inside a configured CIDR block" $
      isTrustedForwardingPeer proxyTrust (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (10, 5, 6, 7))) `shouldBe` True

    it "does not trust a peer outside every configured CIDR block" $
      isTrustedForwardingPeer proxyTrust (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (203, 0, 113, 1))) `shouldBe` False

    it "trusts a peer matching a second listed CIDR block" $
      let otherBlock = fromMaybe (error "expected a valid test CIDR block") (parseCidrBlock "172.16.0.0/12")
       in isTrustedForwardingPeer (TrustForwardedFrom (trustedBlock :| [otherBlock])) (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (172, 16, 4, 4))) `shouldBe` True

    it "trusts a Unix-domain-socket peer once forwarding is enabled at all, since it cannot be spoofed remotely" $
      isTrustedForwardingPeer proxyTrust (Socket.SockAddrUnix "/tmp/harch-web.sock") `shouldBe` True

    it "does not trust a non-IPv4, non-Unix-socket peer" $
      isTrustedForwardingPeer proxyTrust (Socket.SockAddrInet6 0 0 (0, 0, 0, 1) 0) `shouldBe` False

    it "derives a printable Show instance for both constructors" $
      expectAll
        ( (show NeverTrustForwarded `shouldBe` "NeverTrustForwarded")
            :| [ show proxyTrust `shouldContain` "TrustForwardedFrom",
                 -- 'show' on the 'Maybe' wrapper so 'TrustForwardedFrom' renders
                 -- at precedence 11, exercising its parenthesization branch.
                 show (Just proxyTrust) `shouldContain` "(TrustForwardedFrom",
                 -- 'show' on a list exercises the Show class's default
                 -- 'showList' method, which neither form above reaches.
                 show [NeverTrustForwarded, proxyTrust] `shouldBe` "[NeverTrustForwarded," <> show proxyTrust <> "]"
               ]
        )

    it "derives an Eq instance that distinguishes both constructors" $
      expectAll
        ( ((NeverTrustForwarded == proxyTrust) `shouldBe` False)
            :| [ (proxyTrust == NeverTrustForwarded) `shouldBe` False,
                 (proxyTrust == TrustForwardedFrom (trustedBlock :| [])) `shouldBe` True,
                 (NeverTrustForwarded /= proxyTrust) `shouldBe` True
               ]
        )
