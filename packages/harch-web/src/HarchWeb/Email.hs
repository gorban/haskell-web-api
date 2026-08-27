{-# LANGUAGE OverloadedStrings #-}

-- | SMTP delivery with a deliberately narrow authenticated-transport policy.
--
-- Decision (AW, 2026-08-18): extend the existing SMTP configuration boundary
-- rather than adding an application-owned transport wrapper. Credentials made
-- with 'smtpAuthentication' require the server to advertise STARTTLS, upgrade
-- through a system-trust-store and hostname-validated TLS handshake, then
-- advertise AUTH PLAIN again before they are sent; the distinct first-byte
-- TLS protocol is available as 'smtpAuthenticationOverImplicitTls'. The only
-- plaintext escape hatch is named 'smtpAuthenticationForLocalDevelopment', so
-- a production configuration cannot accidentally send a password over a clear
-- socket. Message rendering uses 7bit or base64 instead of assuming 8BITMIME,
-- and RFC 2047 encoded words for non-ASCII subjects.
module HarchWeb.Email
  ( EmailAddress,
    EmailDelivery (..),
    EmailLocale (..),
    EmailMessage,
    EmailMessageInput (..),
    SmtpAddressResolver,
    SmtpAuthentication,
    SmtpConfig,
    SmtpConfigInput (..),
    SmtpHeloName,
    SmtpHost,
    SmtpPassword,
    SmtpUsername,
    deliverSmtpEmail,
    deliverSmtpEmailWithResolver,
    emailMessageBody,
    emailMessageRecipient,
    emailMessageSubject,
    emailAddressText,
    mkEmailAddress,
    mkEmailMessage,
    mkSmtpConfig,
    renderEmailMessage,
    smtpAuthentication,
    smtpAuthenticationForLocalDevelopment,
    smtpAuthenticationOverImplicitTls,
    smtpLoginPassword,
    smtpLoginUsername,
    smtpServerHeloName,
    smtpServerHost,
    verificationEmail,
  )
where

import Control.Exception (bracket, bracketOnError)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isControl, isDigit)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16, Word8)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLSCipher
import System.X509 qualified as SystemX509

newtype EmailAddress = EmailAddress Text
  deriving (Eq, Show)

data EmailMessage = EmailMessage
  { emailMessageRecipient :: EmailAddress,
    emailMessageSubject :: Text,
    emailMessageBody :: Text
  }
  deriving (Eq, Show)

data EmailMessageInput = EmailMessageInput
  { emailInputRecipient :: EmailAddress,
    emailInputSubject :: Text,
    emailInputBody :: Text
  }

data EmailLocale
  = EmailEnglish
  | EmailSpanish
  deriving (Eq, Show)

newtype EmailDelivery = EmailDelivery
  { deliverEmail :: EmailMessage -> IO ()
  }

data SmtpConfig = SmtpConfig
  { smtpHost :: Text,
    smtpPort :: Word16,
    smtpHeloName :: Text,
    smtpEnvelopeSender :: EmailAddress,
    smtpCredentials :: Maybe SmtpCredentials
  }

newtype SmtpHost = SmtpHost Text

newtype SmtpHeloName = SmtpHeloName Text

newtype SmtpUsername = SmtpUsername Text

newtype SmtpPassword = SmtpPassword Text

-- | The only permitted route for SMTP credentials. The ordinary constructor
-- requires an authenticated STARTTLS upgrade; the explicitly named local
-- development constructor is the opt-in escape hatch for a disposable
-- loopback server.
data SmtpAuthentication = SmtpAuthentication SmtpAuthenticationTransport SmtpUsername SmtpPassword

data SmtpAuthenticationTransport
  = SmtpAuthenticationRequiresStartTls
  | SmtpAuthenticationRequiresImplicitTls
  | SmtpAuthenticationAllowsPlaintextForLocalDevelopment

data SmtpConfigInput = SmtpConfigInput
  { smtpInputHost :: SmtpHost,
    smtpInputPort :: Word16,
    smtpInputHeloName :: SmtpHeloName,
    smtpInputEnvelopeSender :: EmailAddress,
    smtpInputAuthentication :: Maybe SmtpAuthentication
  }

data SmtpCredentials = SmtpCredentials
  { smtpCredentialTransport :: SmtpAuthenticationTransport,
    smtpUsername :: Text,
    smtpPassword :: Text
  }

data SmtpWire = SmtpWire
  { smtpWireRead :: IO ByteString.ByteString,
    smtpWireWrite :: ByteString.ByteString -> IO ()
  }

data SmtpConnection = SmtpConnection
  { smtpConnectionSocket :: Socket.Socket,
    smtpConnectionWire :: IORef SmtpWire,
    smtpConnectionBufferedBytes :: IORef ByteString.ByteString
  }

data SmtpResponse = SmtpResponse
  { smtpResponseCode :: ByteString.ByteString,
    smtpResponseLines :: [ByteString.ByteString]
  }

type SmtpAddressResolver = SmtpConfig -> IO (Maybe Socket.AddrInfo)

mkEmailAddress :: Text -> Maybe EmailAddress
mkEmailAddress value =
  case Text.splitOn "@" value of
    [localPart, domainPart]
      | validEmailAddressLength value && validLocalPart localPart && validDomainPart domainPart -> Just (EmailAddress value)
    _ -> Nothing

emailAddressText :: EmailAddress -> Text
emailAddressText (EmailAddress value) = value

mkEmailMessage :: EmailMessageInput -> Maybe EmailMessage
mkEmailMessage
  EmailMessageInput
    { emailInputRecipient = recipient,
      emailInputSubject = subject,
      emailInputBody = body
    } =
    if validEmailSubject subject
      then Just (EmailMessage recipient subject body)
      else Nothing

smtpServerHost :: Text -> SmtpHost
smtpServerHost = SmtpHost

smtpServerHeloName :: Text -> SmtpHeloName
smtpServerHeloName = SmtpHeloName

smtpLoginUsername :: Text -> SmtpUsername
smtpLoginUsername = SmtpUsername

smtpLoginPassword :: Text -> SmtpPassword
smtpLoginPassword = SmtpPassword

smtpAuthentication :: SmtpUsername -> SmtpPassword -> SmtpAuthentication
smtpAuthentication = SmtpAuthentication SmtpAuthenticationRequiresStartTls

-- | An intentionally conspicuous escape hatch for a disposable local SMTP
-- server. Never use this with a real host: it permits @AUTH PLAIN@ before a
-- TLS upgrade.
smtpAuthenticationForLocalDevelopment :: SmtpUsername -> SmtpPassword -> SmtpAuthentication
smtpAuthenticationForLocalDevelopment = SmtpAuthentication SmtpAuthenticationAllowsPlaintextForLocalDevelopment

-- | Use TLS from the first byte for an SMTP submission service that is
-- configured for implicit TLS (normally port 465). The server certificate is
-- still validated against the configured host and the system trust store.
smtpAuthenticationOverImplicitTls :: SmtpUsername -> SmtpPassword -> SmtpAuthentication
smtpAuthenticationOverImplicitTls = SmtpAuthentication SmtpAuthenticationRequiresImplicitTls

mkSmtpConfig :: SmtpConfigInput -> Maybe SmtpConfig
mkSmtpConfig
  SmtpConfigInput
    { smtpInputHost = SmtpHost host,
      smtpInputPort = port,
      smtpInputHeloName = SmtpHeloName heloName,
      smtpInputEnvelopeSender = sender,
      smtpInputAuthentication = authentication
    } =
    if not (Text.null host) && port > 0 && validHeaderValue heloName
      then SmtpConfig host port heloName sender <$> traverse toCredentials authentication
      else Nothing
    where
      toCredentials (SmtpAuthentication transport (SmtpUsername username) (SmtpPassword password)) =
        if validAuthenticationValue username && validAuthenticationValue password
          then Just (SmtpCredentials transport username password)
          else Nothing

verificationEmail :: EmailLocale -> EmailAddress -> Text -> EmailMessage
verificationEmail locale recipient verificationUrl =
  case locale of
    EmailEnglish ->
      EmailMessage
        { emailMessageRecipient = recipient,
          emailMessageSubject = "Verify your email address",
          emailMessageBody = "Open this link to verify your email address:\n" <> verificationUrl
        }
    EmailSpanish ->
      EmailMessage
        { emailMessageRecipient = recipient,
          emailMessageSubject = "Verifica tu correo electrónico",
          emailMessageBody = "Abre este enlace para verificar tu correo electrónico:\n" <> verificationUrl
        }

deliverSmtpEmail :: SmtpConfig -> EmailMessage -> IO ()
deliverSmtpEmail = deliverSmtpEmailWithResolver resolveSmtpAddress

deliverSmtpEmailWithResolver :: SmtpAddressResolver -> SmtpConfig -> EmailMessage -> IO ()
deliverSmtpEmailWithResolver resolveAddress config message =
  Socket.withSocketsDo $
    withSmtpConnection resolveAddress config $ \connection -> do
      _ <- expectSmtpResponse connection "220"
      capabilities <- sendSmtpCommand connection ("EHLO " <> smtpHeloName config) "250"
      for_ (smtpCredentials config) (authenticateSmtp connection config capabilities)
      _ <- sendSmtpCommand connection ("MAIL FROM:<" <> emailAddressText (smtpEnvelopeSender config) <> ">") "250"
      _ <- sendSmtpCommand connection ("RCPT TO:<" <> emailAddressText (emailMessageRecipient message) <> ">") "250"
      _ <- sendSmtpCommand connection "DATA" "354"
      writeSmtpBytes connection (renderSmtpMessage config message <> "\r\n.\r\n")
      _ <- expectSmtpResponse connection "250"
      void (sendSmtpCommand connection "QUIT" "221")

withSmtpConnection :: SmtpAddressResolver -> SmtpConfig -> (SmtpConnection -> IO value) -> IO value
withSmtpConnection resolveAddress config action = do
  maybeAddress <- resolveAddress config
  case maybeAddress of
    Nothing -> ioError (userError "SMTP hostname did not resolve to a connectable address")
    Just address ->
      bracketOnError
        (Socket.socket (Socket.addrFamily address) (Socket.addrSocketType address) (Socket.addrProtocol address))
        Socket.close
        ( \socket -> do
            Socket.connect socket (Socket.addrAddress address)
            wireReference <- newIORef (socketSmtpWire socket)
            bufferedBytes <- newIORef ByteString.empty
            let connection = SmtpConnection socket wireReference bufferedBytes
            when (usesImplicitTls config) $
              upgradeSmtpConnectionToTls connection (smtpHost config)
            bracket
              (pure connection)
              closeSmtpConnection
              action
        )

usesImplicitTls :: SmtpConfig -> Bool
usesImplicitTls config =
  case smtpCredentials config of
    Just credentials ->
      case smtpCredentialTransport credentials of
        SmtpAuthenticationRequiresImplicitTls -> True
        _ -> False
    Nothing -> False

resolveSmtpAddress :: SmtpAddressResolver
resolveSmtpAddress config =
  listToMaybe
    <$> Socket.getAddrInfo
      (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
      (Just (Text.unpack (smtpHost config)))
      (Just (show (smtpPort config)))

sendSmtpCommand :: SmtpConnection -> Text -> ByteString.ByteString -> IO SmtpResponse
sendSmtpCommand connection command expectedCode = do
  writeSmtpBytes connection (TextEncoding.encodeUtf8 command <> "\r\n")
  expectSmtpResponse connection expectedCode

authenticateSmtp :: SmtpConnection -> SmtpConfig -> SmtpResponse -> SmtpCredentials -> IO ()
authenticateSmtp connection config capabilities credentials = do
  case smtpCredentialTransport credentials of
    SmtpAuthenticationRequiresStartTls -> do
      requireSmtpCapability capabilities "STARTTLS"
      _ <- sendSmtpCommand connection "STARTTLS" "220"
      upgradeSmtpConnectionToTls connection (smtpHost config)
      securedCapabilities <- sendSmtpCommand connection ("EHLO " <> smtpHeloName config) "250"
      requireSmtpCapability securedCapabilities "AUTH PLAIN"
    SmtpAuthenticationRequiresImplicitTls ->
      requireSmtpCapability capabilities "AUTH PLAIN"
    SmtpAuthenticationAllowsPlaintextForLocalDevelopment ->
      requireSmtpCapability capabilities "AUTH PLAIN"
  void
    ( sendSmtpCommand
        connection
        ( "AUTH PLAIN "
            <> TextEncoding.decodeUtf8
              ( Base64.encode
                  ( "\NUL"
                      <> TextEncoding.encodeUtf8 (smtpUsername credentials)
                      <> "\NUL"
                      <> TextEncoding.encodeUtf8 (smtpPassword credentials)
                  )
              )
        )
        "235"
    )

expectSmtpResponse :: SmtpConnection -> ByteString.ByteString -> IO SmtpResponse
expectSmtpResponse connection expectedCode = do
  response <- readSmtpResponse connection
  unless (smtpResponseCode response == expectedCode) $
    ioError (userError ("Unexpected SMTP response: " <> show (smtpResponseLines response)))
  pure response

writeSmtpBytes :: SmtpConnection -> ByteString.ByteString -> IO ()
writeSmtpBytes connection bytes = do
  wire <- readIORef (smtpConnectionWire connection)
  smtpWireWrite wire bytes

readSmtpResponse :: SmtpConnection -> IO SmtpResponse
readSmtpResponse connection = do
  (responseCode, responseSeparator, firstLine) <- readSmtpResponseLine connection
  remainingLines <- readContinuationLines responseCode responseSeparator []
  pure (SmtpResponse responseCode (firstLine : reverse remainingLines))
  where
    readContinuationLines expectedCode separator accumulatedLines
      | separator == 45 = do
          (nextCode, nextSeparator, nextLine) <- readSmtpResponseLine connection
          unless (nextCode == expectedCode) $
            ioError (userError "SMTP multiline response changed status code")
          readContinuationLines expectedCode nextSeparator (nextLine : accumulatedLines)
      | separator == 32 = pure accumulatedLines
      | otherwise = ioError (userError "SMTP response used an invalid continuation separator")

readSmtpResponseLine :: SmtpConnection -> IO (ByteString.ByteString, Word8, ByteString.ByteString)
readSmtpResponseLine connection = do
  line <- readSmtpLine connection
  if ByteString.length line >= 4 && ByteString.all isAsciiDigit (ByteString.take 3 line)
    then pure (ByteString.take 3 line, ByteString.index line 3, ByteString.drop 4 line)
    else ioError (userError ("Malformed SMTP response: " <> show line))
  where
    isAsciiDigit byte = byte >= 48 && byte <= 57

readSmtpLine :: SmtpConnection -> IO ByteString.ByteString
readSmtpLine connection = do
  buffered <- readIORef (smtpConnectionBufferedBytes connection)
  go buffered
  where
    maxSmtpResponseLineBytes = 16384

    go buffered =
      case ByteString.breakSubstring "\r\n" buffered of
        (line, remainder)
          | not (ByteString.null remainder) -> do
              writeIORef (smtpConnectionBufferedBytes connection) (ByteString.drop 2 remainder)
              pure line
          | ByteString.length buffered >= maxSmtpResponseLineBytes ->
              ioError (userError "SMTP response line exceeds 16384 bytes")
          | otherwise -> do
              wire <- readIORef (smtpConnectionWire connection)
              next <- smtpWireRead wire
              if ByteString.null next
                then ioError (userError "SMTP server closed the connection before completing a response")
                else go (buffered <> next)

requireSmtpCapability :: SmtpResponse -> ByteString.ByteString -> IO ()
requireSmtpCapability response capability =
  unless (any (offersSmtpCapability capability) (smtpResponseLines response)) $
    ioError (userError ("SMTP server did not advertise required capability " <> show capability))

offersSmtpCapability :: ByteString.ByteString -> ByteString.ByteString -> Bool
offersSmtpCapability capability responseLine =
  let normalizedCapability = asciiUpper capability
      normalizedLine = asciiUpper responseLine
      capabilityLength = ByteString.length normalizedCapability
   in normalizedCapability `ByteString.isPrefixOf` normalizedLine
        && (ByteString.length normalizedLine == capabilityLength || ByteString.index normalizedLine capabilityLength == 32)

asciiUpper :: ByteString.ByteString -> ByteString.ByteString
asciiUpper = ByteString.map toUpperAscii
  where
    toUpperAscii byte
      | byte >= 97 && byte <= 122 = byte - 32
      | otherwise = byte

upgradeSmtpConnectionToTls :: SmtpConnection -> Text -> IO ()
upgradeSmtpConnectionToTls connection host = do
  buffered <- readIORef (smtpConnectionBufferedBytes connection)
  unless (ByteString.null buffered) $
    ioError (userError "SMTP server sent plaintext bytes after accepting STARTTLS")
  certificateStore <- SystemX509.getSystemCertificateStore
  -- AW deliberately constructs fresh client parameters for every SMTP
  -- connection. The default empty service identity scopes only the TLS
  -- validation cache; with the required no-cache policy, chain and hostname
  -- validation happen every time. This path is not optimized for validation
  -- caching or rapid connection reuse. A cache needs a new ADR because it
  -- changes certificate rotation, trust-store, and revocation semantics.
  --
  -- The user explicitly approved this last-resort strictness point after the
  -- full coverage gate repeatedly left TLS's required, otherwise inert cache
  -- identity unticked. It changes only when @ByteString.empty@ is evaluated;
  -- it does not introduce caching or change certificate/hostname validation.
  let socket = smtpConnectionSocket connection
      defaultParameters = TLS.defaultParamsClient (Text.unpack host) $! ByteString.empty
      parameters =
        defaultParameters
          { TLS.clientSupported =
              (TLS.clientSupported defaultParameters)
                { TLS.supportedCiphers = TLSCipher.ciphersuite_default
                },
            TLS.clientShared =
              (TLS.clientShared defaultParameters)
                { TLS.sharedCAStore = certificateStore
                }
          }
  context <- TLS.contextNew socket parameters
  TLS.handshake context
  writeIORef
    (smtpConnectionWire connection)
    SmtpWire
      { smtpWireRead = TLS.recvData context,
        smtpWireWrite = TLS.sendData context . LazyByteString.fromStrict
      }

socketSmtpWire :: Socket.Socket -> SmtpWire
socketSmtpWire socket =
  SmtpWire
    { smtpWireRead = SocketByteString.recv socket 4096,
      smtpWireWrite = SocketByteString.sendAll socket
    }

closeSmtpConnection :: SmtpConnection -> IO ()
closeSmtpConnection = Socket.close . smtpConnectionSocket

renderSmtpMessage :: SmtpConfig -> EmailMessage -> ByteString.ByteString
renderSmtpMessage config =
  renderEmailMessage (smtpEnvelopeSender config)

renderEmailMessage :: EmailAddress -> EmailMessage -> ByteString.ByteString
renderEmailMessage sender message =
  TextEncoding.encodeUtf8
    ( Text.concat
        [ "From: <",
          emailAddressText sender,
          ">\r\nTo: <",
          emailAddressText (emailMessageRecipient message),
          ">\r\nSubject: ",
          renderEmailSubject (emailMessageSubject message),
          "\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: ",
          renderedTransferEncoding,
          "\r\n\r\n",
          renderedBody
        ]
    )
  where
    (renderedTransferEncoding, renderedBody) =
      if Text.all isAscii (emailMessageBody message)
        then ("7bit", dotStuff (emailMessageBody message))
        else ("base64", base64Lines (TextEncoding.encodeUtf8 (emailMessageBody message)))

renderEmailSubject :: Text -> Text
renderEmailSubject subject
  | Text.all isAscii subject = subject
  | otherwise = "=?UTF-8?B?" <> TextEncoding.decodeUtf8 (Base64.encode (TextEncoding.encodeUtf8 subject)) <> "?="

base64Lines :: ByteString.ByteString -> Text
base64Lines = Text.intercalate "\r\n" . Text.chunksOf 76 . TextEncoding.decodeUtf8 . Base64.encode

dotStuff :: Text -> Text
dotStuff body =
  Text.intercalate
    "\r\n"
    ( map
        ( \line ->
            if Text.isPrefixOf "." line
              then "." <> line
              else line
        )
        (Text.splitOn "\n" (Text.replace "\r\n" "\n" body))
    )

validLocalPart :: Text -> Bool
validLocalPart value =
  not (Text.null value) && Text.all isLocalPartCharacter value

-- | The RFC 5321 mailbox limit is 254 ASCII octets.  'EmailAddress' accepts
-- only ASCII characters, so 'Text.length' is the same bound in this parser;
-- keeping it here rejects an oversized login identifier before it can own a
-- durable authentication-attempt key.
validEmailAddressLength :: Text -> Bool
validEmailAddressLength value = Text.length value <= 254

validDomainPart :: Text -> Bool
validDomainPart value =
  not (Text.null value) && Text.all isDomainPartCharacter value

validHeaderValue :: Text -> Bool
validHeaderValue =
  Text.all (\character -> isAscii character && not (isControl character))

validEmailSubject :: Text -> Bool
validEmailSubject = Text.all (\character -> not (isControl character) || character == '\t')

validAuthenticationValue :: Text -> Bool
validAuthenticationValue value =
  not (Text.null value) && Text.all (\character -> character /= '\NUL' && character /= '\r' && character /= '\n') value

isLocalPartCharacter :: Char -> Bool
isLocalPartCharacter character =
  isAsciiLower character || isAsciiUpper character || isDigit character || character `elem` ("._+-" :: String)

isDomainPartCharacter :: Char -> Bool
isDomainPartCharacter character =
  isAsciiLower character || isAsciiUpper character || isDigit character || character `elem` (".-" :: String)
