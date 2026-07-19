{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Email
  ( EmailAddress,
    EmailDelivery (..),
    EmailLocale (..),
    EmailMessage,
    SmtpAddressResolver,
    SmtpConfig,
    deliverSmtpEmail,
    deliverSmtpEmailWithResolver,
    emailMessageBody,
    emailMessageRecipient,
    emailMessageSubject,
    emailAddressText,
    mkAuthenticatedSmtpConfig,
    mkEmailAddress,
    mkEmailMessage,
    mkSmtpConfig,
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

data SmtpCredentials = SmtpCredentials
  { smtpUsername :: Text,
    smtpPassword :: Text
  }

type SmtpAddressResolver = SmtpConfig -> IO (Maybe Socket.AddrInfo)

mkEmailAddress :: Text -> Maybe EmailAddress
mkEmailAddress value =
  case Text.splitOn "@" value of
    [localPart, domainPart]
      | validLocalPart localPart && validDomainPart domainPart -> Just (EmailAddress value)
    _ -> Nothing

emailAddressText :: EmailAddress -> Text
emailAddressText (EmailAddress value) = value

mkEmailMessage :: EmailAddress -> Text -> Text -> Maybe EmailMessage
mkEmailMessage recipient subject body =
  if validHeaderValue subject
    then Just (EmailMessage recipient subject body)
    else Nothing

mkSmtpConfig :: Text -> Word16 -> Text -> EmailAddress -> Maybe SmtpConfig
mkSmtpConfig host port heloName sender =
  mkSmtpConfigWithCredentials host port heloName sender Nothing

mkAuthenticatedSmtpConfig :: Text -> Word16 -> Text -> EmailAddress -> Text -> Text -> Maybe SmtpConfig
mkAuthenticatedSmtpConfig host port heloName sender username password =
  if validAuthenticationValue username && validAuthenticationValue password
    then mkSmtpConfigWithCredentials host port heloName sender (Just (SmtpCredentials username password))
    else Nothing

mkSmtpConfigWithCredentials :: Text -> Word16 -> Text -> EmailAddress -> Maybe SmtpCredentials -> Maybe SmtpConfig
mkSmtpConfigWithCredentials host port heloName sender credentials =
  if not (Text.null host) && port > 0 && validHeaderValue heloName
    then Just (SmtpConfig host port heloName sender credentials)
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
renderSmtpMessage config message =
  TextEncoding.encodeUtf8
    ( Text.concat
        [ "From: <",
          emailAddressText (smtpEnvelopeSender config),
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
