{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
import Control.Monad ()
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty ()
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb ()
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security (RequestContextField (RequestContextField), RequestPolicyConfig (forwardedHeaderTrust), clientAddressText, defaultClientAddress, requestClientAddress)
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket (SockAddr (SockAddrInet), tupleToHostAddress)
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (defaultRequestPolicy, testTrustedForwardedProxy, waiRequestWithRemoteHostAndHeaders)

spec = do
  describe "RequestContextField" $
    it "keeps private context fields comparable and inspectable" $ do
      let clientAddress = Security.RequestContextField "client.address" "203.0.113.8"
          peerAddress = Security.RequestContextField "network.peer.address" "203.0.113.8"
      clientAddress `shouldNotBe` peerAddress
      show clientAddress `shouldBe` "RequestContextField {requestContextFieldName = \"client.address\", requestContextFieldValue = \"203.0.113.8\"}"
      show [clientAddress] `shouldBe` "[RequestContextField {requestContextFieldName = \"client.address\", requestContextFieldValue = \"203.0.113.8\"}]"

  describe "ClientAddress" $
    it "uses only trusted, address-shaped forwarding and redacts implicit rendering" $ do
      let socketPeer = Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          directRequest = waiRequestWithRemoteHostAndHeaders [] socketPeer [("X-Forwarded-For", "203.0.113.8")]
          trustedRequest = waiRequestWithRemoteHostAndHeaders [] socketPeer [("Forwarded", "for=\"[2001:DB8::1]\"")]
          emptyTrustedRequest = waiRequestWithRemoteHostAndHeaders [] socketPeer [("X-Forwarded-For", "")]
          emptyBracketedTrustedRequest = waiRequestWithRemoteHostAndHeaders [] socketPeer [("Forwarded", "for=\"[]\"")]
          malformedTrustedRequest = waiRequestWithRemoteHostAndHeaders [] socketPeer [("X-Forwarded-For", "not-an-address")]
          trustedPolicy = defaultRequestPolicy {Security.forwardedHeaderTrust = testTrustedForwardedProxy}
      Security.clientAddressText (Security.requestClientAddress defaultRequestPolicy directRequest) `shouldBe` "127.0.0.1"
      Security.clientAddressText (Security.requestClientAddress trustedPolicy trustedRequest) `shouldBe` "2001:db8::1"
      Security.clientAddressText (Security.requestClientAddress trustedPolicy emptyTrustedRequest) `shouldBe` "127.0.0.1"
      Security.clientAddressText (Security.requestClientAddress trustedPolicy emptyBracketedTrustedRequest) `shouldBe` "127.0.0.1"
      Security.clientAddressText (Security.requestClientAddress trustedPolicy malformedTrustedRequest) `shouldBe` "127.0.0.1"
      Security.requestClientAddress defaultRequestPolicy directRequest == Security.defaultClientAddress `shouldBe` True
      Security.requestClientAddress trustedPolicy trustedRequest /= Security.defaultClientAddress `shouldBe` True
      Security.clientAddressText Security.defaultClientAddress `shouldBe` "127.0.0.1"
      show Security.defaultClientAddress `shouldBe` "ClientAddress <redacted>"
      show [Security.defaultClientAddress] `shouldBe` "[ClientAddress <redacted>]"
