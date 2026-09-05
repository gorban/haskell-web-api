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
import Data.Text (Text)
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (PathPrefix, PathPrefixError (PathPrefixAmbiguousSegment, PathPrefixMultipleSlashes, PathPrefixUnsafeCharacter), applyRequestPathPrefix, mkUrlPath, parseRequestPathPrefix, pathPrefixText, stripRequestPathPrefix, urlPathText)
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
  describe "PathPrefix and UrlPath" $ do
    it "keeps both path roles distinct while applying and stripping prefixes" $
      let prefix = validPathPrefix "/app"
       in expectAll
            ( (pathPrefixText prefix `shouldBe` "/app")
                :| [ urlPathText (mkUrlPath "/known") `shouldBe` "/known",
                     urlPathText (applyRequestPathPrefix prefix (mkUrlPath "/known")) `shouldBe` "/app/known",
                     urlPathText (stripRequestPathPrefix prefix (mkUrlPath "/app/known")) `shouldBe` "/known"
                   ]
            )

    it "canonicalizes safe path segments and rejects ambiguous browser prefixes" $
      expectAll
        ( (pathPrefixText (validPathPrefix "app/~team_1") `shouldBe` "/app/~team_1")
            :| [ parseRequestPathPrefix "//attacker.example" `shouldBe` Left PathPrefixMultipleSlashes,
                 parseRequestPathPrefix "/app//nested" `shouldBe` Left PathPrefixMultipleSlashes,
                 parseRequestPathPrefix "/app\\nested" `shouldBe` Left PathPrefixUnsafeCharacter,
                 parseRequestPathPrefix "/app?next=external" `shouldBe` Left PathPrefixUnsafeCharacter,
                 parseRequestPathPrefix "/app#fragment" `shouldBe` Left PathPrefixUnsafeCharacter,
                 parseRequestPathPrefix "/app%2Fexternal" `shouldBe` Left PathPrefixUnsafeCharacter,
                 parseRequestPathPrefix "/app\NULnested" `shouldBe` Left PathPrefixUnsafeCharacter,
                 parseRequestPathPrefix "/app//" `shouldBe` Left PathPrefixMultipleSlashes,
                 parseRequestPathPrefix "/app/" `shouldBe` Right (validPathPrefix "/app"),
                 parseRequestPathPrefix "/app/./nested" `shouldBe` Left PathPrefixAmbiguousSegment,
                 parseRequestPathPrefix "/app/../nested" `shouldBe` Left PathPrefixAmbiguousSegment
               ]
        )

    it "keeps the public policy values comparable and diagnosable" $
      let canonicalPrefix = validPathPrefix "/app"
          equivalentPrefix = validPathPrefix "app"
          distinctPrefix = validPathPrefix "/other"
       in expectAll
            ( ((canonicalPrefix == equivalentPrefix) `shouldBe` True)
                :| [ (canonicalPrefix == distinctPrefix) `shouldBe` False,
                     (canonicalPrefix /= distinctPrefix) `shouldBe` True,
                     show canonicalPrefix `shouldBe` "PathPrefix \"/app\"",
                     showsPrec 11 canonicalPrefix "" `shouldBe` "(PathPrefix \"/app\")",
                     showList [canonicalPrefix] "" `shouldBe` "[PathPrefix \"/app\"]",
                     (PathPrefixMultipleSlashes == PathPrefixUnsafeCharacter) `shouldBe` False,
                     (PathPrefixMultipleSlashes /= PathPrefixUnsafeCharacter) `shouldBe` True,
                     show PathPrefixAmbiguousSegment `shouldBe` "PathPrefixAmbiguousSegment",
                     showsPrec 11 PathPrefixAmbiguousSegment "" `shouldBe` "PathPrefixAmbiguousSegment",
                     showList [PathPrefixAmbiguousSegment] "" `shouldBe` "[PathPrefixAmbiguousSegment]"
                   ]
            )

validPathPrefix :: Text -> PathPrefix
validPathPrefix value =
  case parseRequestPathPrefix value of
    Left parseError -> error ("invalid path-prefix fixture: " <> show parseError)
    Right pathPrefix -> pathPrefix
