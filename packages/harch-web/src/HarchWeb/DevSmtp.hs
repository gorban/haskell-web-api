{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.DevSmtp
  ( DevSmtpEmail (..),
    DevSmtpServer,
    devSmtpPort,
    devSmtpReceivedEmails,
    startDevSmtpServer,
    stopDevSmtpServer,
    takeLatestDevSmtpEmailTo,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16)
import Network.Socket qualified as Socket
import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hSetBuffering)

data DevSmtpEmail = DevSmtpEmail
  { devSmtpEnvelopeSender :: Text,
    devSmtpRecipients :: [Text],
    devSmtpRawMessage :: ByteString
  }

data DevSmtpServer = DevSmtpServer
  { devSmtpListener :: Socket.Socket,
    devSmtpThread :: ThreadId,
    devSmtpPort :: Word16,
    devSmtpMailbox :: MVar [DevSmtpEmail]
  }

startDevSmtpServer :: IO DevSmtpServer
startDevSmtpServer = do
  listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.setSocketOption listener Socket.ReuseAddr 1
  Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen listener 16
  port <- Socket.socketPort listener
  mailbox <- newMVar []
  thread <- forkIO (acceptConnections listener mailbox)
  pure (DevSmtpServer listener thread (fromIntegral port) mailbox)

stopDevSmtpServer :: DevSmtpServer -> IO ()
stopDevSmtpServer server = do
  killThread (devSmtpThread server)
  Socket.close (devSmtpListener server)

devSmtpReceivedEmails :: DevSmtpServer -> IO [DevSmtpEmail]
devSmtpReceivedEmails server = reverse <$> readMVar (devSmtpMailbox server)

takeLatestDevSmtpEmailTo :: DevSmtpServer -> Text -> IO (Maybe DevSmtpEmail)
takeLatestDevSmtpEmailTo server recipient = do
  let normalizedRecipient = Text.toCaseFold recipient
  modifyMVar (devSmtpMailbox server) $ \emails -> do
    let matchingEmail email = normalizedRecipient `elem` map Text.toCaseFold (devSmtpRecipients email)
        result = find matchingEmail emails
    pure (filter (not . matchingEmail) emails, result)

acceptConnections :: Socket.Socket -> MVar [DevSmtpEmail] -> IO ()
acceptConnections listener mailbox = forever $ do
  (socket, _) <- Socket.accept listener
  void . forkIO $ do
    email <- receiveDevSmtpEmail socket
    modifyMVar_ mailbox (pure . (email :))

receiveDevSmtpEmail :: Socket.Socket -> IO DevSmtpEmail
receiveDevSmtpEmail socket = do
  handle <- Socket.socketToHandle socket ReadWriteMode
  hSetBuffering handle NoBuffering
  flip finally (hClose handle) $ do
    writeHandle handle "220 harch development SMTP ready\r\n"
    expectCommand handle "250 harch development SMTP\r\n"
    sender <- receiveSender handle
    (recipients, rawMessage) <- receiveRecipients handle []
    writeHandle handle "250 message accepted\r\n"
    void (receiveHandleLine handle)
    writeHandle handle "221 goodbye\r\n"
    pure (DevSmtpEmail sender recipients rawMessage)

receiveSender :: Handle -> IO Text
receiveSender handle = do
  command <- receiveHandleLine handle
  if "AUTH PLAIN " `ByteString.isPrefixOf` command
    then writeHandle handle "235 authenticated\r\n" >> expectAddressCommand handle "MAIL FROM:" "250 sender accepted\r\n"
    else do
      sender <- smtpAddress command "MAIL FROM:"
      writeHandle handle "250 sender accepted\r\n"
      pure sender

receiveRecipients :: Handle -> [Text] -> IO ([Text], ByteString)
receiveRecipients handle recipients = do
  command <- receiveHandleLine handle
  if "RCPT TO:" `ByteString.isPrefixOf` command
    then do
      recipient <- smtpAddress command "RCPT TO:"
      writeHandle handle "250 recipient accepted\r\n"
      receiveRecipients handle (recipients <> [recipient])
    else do
      writeHandle handle "354 send message\r\n"
      rawMessage <- receiveMessage handle
      pure (recipients, rawMessage)

receiveMessage :: Handle -> IO ByteString
receiveMessage handle = go []
  where
    go messageLines = do
      line <- receiveHandleLine handle
      if line == "."
        then pure (ByteString.intercalate "\r\n" (reverse messageLines))
        else go (unstuff line : messageLines)
    unstuff = fromMaybePrefix ".." "."

expectCommand :: Handle -> ByteString -> IO ()
expectCommand handle response = do
  void (receiveHandleLine handle)
  writeHandle handle response

expectAddressCommand :: Handle -> ByteString -> ByteString -> IO Text
expectAddressCommand handle prefix response = do
  command <- receiveHandleLine handle
  address <- smtpAddress command prefix
  writeHandle handle response
  pure address

smtpAddress :: ByteString -> ByteString -> IO Text
smtpAddress command prefix =
  pure
    ( TextEncoding.decodeUtf8
        ( ByteString.dropWhileEnd
            (== 62)
            (ByteString.dropWhile (== 60) (fromMaybePrefix prefix "" command))
        )
    )

receiveHandleLine :: Handle -> IO ByteString
receiveHandleLine handle = do
  line <- ByteStringChar8.hGetLine handle
  pure (ByteString.dropWhileEnd (== 13) line)

writeHandle :: Handle -> ByteString -> IO ()
writeHandle handle response = ByteString.hPut handle response >> hFlush handle

fromMaybePrefix :: ByteString -> ByteString -> ByteString -> ByteString
fromMaybePrefix prefix replacement value =
  case ByteString.stripPrefix prefix value of
    Just suffix -> replacement <> suffix
    Nothing -> value
