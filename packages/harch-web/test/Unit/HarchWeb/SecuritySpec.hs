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
import HarchWeb.Security qualified as Security (RequestContextField (RequestContextField))
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
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
import Unit.HarchWeb.TestSupport ()

spec = do
  describe "RequestContextField" $
    it "keeps private context fields comparable and inspectable" $ do
      let clientAddress = Security.RequestContextField "client.address" "203.0.113.8"
          peerAddress = Security.RequestContextField "network.peer.address" "203.0.113.8"
      clientAddress `shouldNotBe` peerAddress
      show clientAddress `shouldBe` "RequestContextField {requestContextFieldName = \"client.address\", requestContextFieldValue = \"203.0.113.8\"}"
      show [clientAddress] `shouldBe` "[RequestContextField {requestContextFieldName = \"client.address\", requestContextFieldValue = \"203.0.113.8\"}]"
