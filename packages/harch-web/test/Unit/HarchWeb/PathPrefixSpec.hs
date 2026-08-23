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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (applyRequestPathPrefix, mkPathPrefix, mkUrlPath, pathPrefixText, stripRequestPathPrefix, urlPathText)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
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
  describe "PathPrefix and UrlPath" $
    it "keeps both path roles distinct while applying and stripping prefixes" $
      expectAll
        ( (pathPrefixText (mkPathPrefix "/app") `shouldBe` "/app")
            :| [ urlPathText (mkUrlPath "/known") `shouldBe` "/known",
                 urlPathText (applyRequestPathPrefix (mkPathPrefix "/app") (mkUrlPath "/known")) `shouldBe` "/app/known",
                 urlPathText (stripRequestPathPrefix (mkPathPrefix "/app") (mkUrlPath "/app/known")) `shouldBe` "/known"
               ]
        )
