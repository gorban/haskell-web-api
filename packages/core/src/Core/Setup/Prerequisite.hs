{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.Prerequisite
  ( TcpEndpoint (..),
    TracingEndpointParseError (..),
    checkTcpEndpointReachable,
    checkTcpEndpointReachableWithTimeout,
    checkTracingEndpointReachable,
    parseTracingEndpoint,
  )
where

import Control.Exception (IOException, bracket, try)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.Socket (AddrInfo (..), SocketType (Stream), close, connect, defaultHints, getAddrInfo, openSocket)
import System.Timeout (timeout)

data TcpEndpoint = TcpEndpoint
  { tcpEndpointHost :: Text,
    tcpEndpointPort :: Int
  }
  deriving (Eq, Show)

data TracingEndpointParseError
  = InvalidTracingEndpointFormat Text
  | UnsupportedTracingEndpointScheme Text
  | MissingTracingEndpointHost
  | InvalidTracingEndpointPort Text
  deriving (Eq, Show)

oneSecondMicroseconds :: Int
oneSecondMicroseconds = 1000000

checkTcpEndpointReachable :: TcpEndpoint -> IO Bool
checkTcpEndpointReachable =
  checkTcpEndpointReachableWithTimeout oneSecondMicroseconds

checkTcpEndpointReachableWithTimeout :: Int -> TcpEndpoint -> IO Bool
checkTcpEndpointReachableWithTimeout timeoutMicroseconds endpoint =
  fromMaybe False
    <$> timeout timeoutMicroseconds (resolveAndConnect endpoint)

parseTracingEndpoint :: Text -> Either TracingEndpointParseError TcpEndpoint
parseTracingEndpoint endpoint = do
  (scheme, authority) <- parseEndpointAuthority endpoint
  let defaultPort =
        case scheme of
          "http" -> 80
          "https" -> 443
          _ -> 0
  if defaultPort == 0
    then Left (UnsupportedTracingEndpointScheme scheme)
    else parseAuthority defaultPort authority

checkTracingEndpointReachable :: Text -> IO (Either TracingEndpointParseError Bool)
checkTracingEndpointReachable =
  traverse checkTcpEndpointReachable . parseTracingEndpoint

resolveAndConnect :: TcpEndpoint -> IO Bool
resolveAndConnect endpoint = do
  let hints = defaultHints {addrSocketType = Stream}
  resolvedAddresses <-
    try
      ( getAddrInfo
          (Just hints)
          (Just (Text.unpack (tcpEndpointHost endpoint)))
          (Just (show (tcpEndpointPort endpoint)))
      ) ::
      IO (Either IOException [AddrInfo])
  tryConnectAddresses (fromRight [] resolvedAddresses)

tryConnectAddresses :: [AddrInfo] -> IO Bool
tryConnectAddresses [] = pure False
tryConnectAddresses (address : remainingAddresses) = do
  didConnect <- tryConnectAddress address
  if didConnect
    then pure True
    else tryConnectAddresses remainingAddresses

tryConnectAddress :: AddrInfo -> IO Bool
tryConnectAddress address = do
  connectResult <-
    try
      ( bracket
          (openSocket address)
          close
          (\socket -> connect socket (addrAddress address))
      ) ::
      IO (Either IOException ())
  pure $
    case connectResult of
      Left _ -> False
      Right () -> True

parseEndpointAuthority :: Text -> Either TracingEndpointParseError (Text, Text)
parseEndpointAuthority endpoint =
  let (scheme, rest) = Text.breakOn "://" endpoint
   in if Text.null scheme || Text.null rest
        then Left (InvalidTracingEndpointFormat endpoint)
        else
          let authorityAndPath = Text.drop 3 rest
              authority = Text.takeWhile (/= '/') authorityAndPath
           in if Text.null authority
                then Left MissingTracingEndpointHost
                else Right (scheme, authority)

parseAuthority :: Int -> Text -> Either TracingEndpointParseError TcpEndpoint
parseAuthority defaultPort authority =
  case Text.uncons authority of
    Just ('[', bracketedAuthority) -> parseBracketedAuthority defaultPort bracketedAuthority
    _ -> parsePlainAuthority defaultPort authority

parsePlainAuthority :: Int -> Text -> Either TracingEndpointParseError TcpEndpoint
parsePlainAuthority defaultPort authority =
  let (host, remainder) = Text.breakOn ":" authority
   in if Text.null host
        then Left MissingTracingEndpointHost
        else do
          port <- parseOptionalPort defaultPort remainder
          pure
            TcpEndpoint
              { tcpEndpointHost = host,
                tcpEndpointPort = port
              }

parseBracketedAuthority :: Int -> Text -> Either TracingEndpointParseError TcpEndpoint
parseBracketedAuthority defaultPort bracketedAuthority =
  let (host, bracketSuffix) = Text.breakOn "]" bracketedAuthority
   in if Text.null host || Text.null bracketSuffix
        then Left MissingTracingEndpointHost
        else do
          port <- parseOptionalPort defaultPort (Text.drop 1 bracketSuffix)
          pure
            TcpEndpoint
              { tcpEndpointHost = host,
                tcpEndpointPort = port
              }

parseOptionalPort :: Int -> Text -> Either TracingEndpointParseError Int
parseOptionalPort defaultPort remainder =
  case Text.uncons remainder of
    Nothing -> Right defaultPort
    Just (':', portText) ->
      case reads (Text.unpack portText) of
        [(portNumber, "")] | portNumber > 0 -> Right portNumber
        _ -> Left (InvalidTracingEndpointPort portText)
    _ -> Left (InvalidTracingEndpointFormat remainder)
