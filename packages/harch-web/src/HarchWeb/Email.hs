{-# LANGUAGE OverloadedStrings #-}

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
    smtpLoginPassword,
    smtpLoginUsername,
    smtpServerHeloName,
    smtpServerHost,
    verificationEmail,
  )
where

import Control.Exception (bracket, bracketOnError)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16)
import Network.Socket qualified as Socket
import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hSetBuffering)

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

data SmtpAuthentication = SmtpAuthentication SmtpUsername SmtpPassword

data SmtpConfigInput = SmtpConfigInput
  { smtpInputHost :: SmtpHost,
    smtpInputPort :: Word16,
    smtpInputHeloName :: SmtpHeloName,
    smtpInputEnvelopeSender :: EmailAddress,
    smtpInputAuthentication :: Maybe SmtpAuthentication
  }

data SmtpCredentials = SmtpCredentials
  { smtpUsername :: Text,
    smtpPassword :: Text
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
    if validHeaderValue subject
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
smtpAuthentication = SmtpAuthentication

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
      toCredentials (SmtpAuthentication (SmtpUsername username) (SmtpPassword password)) =
        if validAuthenticationValue username && validAuthenticationValue password
          then Just (SmtpCredentials username password)
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
          emailMessageSubject = "Verifica tu correo electronico",
          emailMessageBody = "Abre este enlace para verificar tu correo electronico:\n" <> verificationUrl
        }

deliverSmtpEmail :: SmtpConfig -> EmailMessage -> IO ()
deliverSmtpEmail = deliverSmtpEmailWithResolver resolveSmtpAddress

deliverSmtpEmailWithResolver :: SmtpAddressResolver -> SmtpConfig -> EmailMessage -> IO ()
deliverSmtpEmailWithResolver resolveAddress config message =
  Socket.withSocketsDo $
    withSmtpConnection resolveAddress config $ \handle -> do
      expectSmtpResponse handle "220"
      sendSmtpCommand handle ("EHLO " <> smtpHeloName config) "250"
      maybe (hFlush handle) (authenticateSmtp handle) (smtpCredentials config)
      sendSmtpCommand handle ("MAIL FROM:<" <> emailAddressText (smtpEnvelopeSender config) <> ">") "250"
      sendSmtpCommand handle ("RCPT TO:<" <> emailAddressText (emailMessageRecipient message) <> ">") "250"
      sendSmtpCommand handle "DATA" "354"
      writeSmtpBytes handle (renderSmtpMessage config message <> "\r\n.\r\n")
      expectSmtpResponse handle "250"
      sendSmtpCommand handle "QUIT" "221"

withSmtpConnection :: SmtpAddressResolver -> SmtpConfig -> (Handle -> IO value) -> IO value
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
            handle <- Socket.socketToHandle socket ReadWriteMode
            hSetBuffering handle NoBuffering
            bracket (pure handle) hClose action
        )

resolveSmtpAddress :: SmtpAddressResolver
resolveSmtpAddress config =
  listToMaybe
    <$> Socket.getAddrInfo
      (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
      (Just (Text.unpack (smtpHost config)))
      (Just (show (smtpPort config)))

sendSmtpCommand :: Handle -> Text -> ByteString.ByteString -> IO ()
sendSmtpCommand handle command expectedCode = do
  writeSmtpBytes handle (TextEncoding.encodeUtf8 command <> "\r\n")
  expectSmtpResponse handle expectedCode

authenticateSmtp :: Handle -> SmtpCredentials -> IO ()
authenticateSmtp handle credentials =
  sendSmtpCommand
    handle
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

expectSmtpResponse :: Handle -> ByteString.ByteString -> IO ()
expectSmtpResponse handle expectedCode = do
  response <- ByteStringChar8.hGetLine handle
  unless ((expectedCode <> " ") `ByteString.isPrefixOf` response) $
    ioError (userError ("Unexpected SMTP response: " <> show response))

writeSmtpBytes :: Handle -> ByteString.ByteString -> IO ()
writeSmtpBytes handle bytes = do
  ByteString.hPut handle bytes
  hFlush handle

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
          emailMessageSubject message,
          "\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\n",
          dotStuff (emailMessageBody message)
        ]
    )

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
  Text.all (\character -> isAscii character && character /= '\r' && character /= '\n')

validAuthenticationValue :: Text -> Bool
validAuthenticationValue value =
  not (Text.null value) && Text.all (\character -> character /= '\NUL' && character /= '\r' && character /= '\n') value

isLocalPartCharacter :: Char -> Bool
isLocalPartCharacter character =
  isAsciiLower character || isAsciiUpper character || isDigit character || character `elem` ("._+-" :: String)

isDomainPartCharacter :: Char -> Bool
isDomainPartCharacter character =
  isAsciiLower character || isAsciiUpper character || isDigit character || character `elem` (".-" :: String)
