{-# LANGUAGE OverloadedStrings #-}

-- | Private parsing and derived defaults for certbot command-line options.
--
-- The framework facade retains the supported helpers, while certificate
-- acquisition owns the policy that turns an ACME configuration into a safe
-- certbot invocation.
module HarchWeb.Acme.Certbot.Options
  ( certbotAuthenticatorValues,
    certbotHasExplicitAuthenticator,
    certbotHasFlag,
    certbotHasOption,
    certbotHasWebrootPathOption,
    certbotNeedsDerivedWebrootAuthenticator,
    certbotOptionValues,
    certbotShouldUseWebroot,
    certbotUsesWebroot,
    certbotUsesWebrootFlagOrAuthenticator,
    firstCertbotDomain,
    splitCertbotDomainValue,
  )
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

firstCertbotDomain :: [Text] -> Maybe Text
firstCertbotDomain arguments =
  listToMaybe . concatMap splitCertbotDomainValue $
    certbotOptionValues "-d" arguments
      <> certbotOptionValues "--domain" arguments
      <> certbotOptionValues "--domains" arguments

splitCertbotDomainValue :: Text -> [Text]
splitCertbotDomainValue =
  filter (not . Text.null) . map Text.strip . Text.splitOn ","

certbotOptionValues :: Text -> [Text] -> [Text]
certbotOptionValues optionName arguments =
  [ optionValue
  | (argument, optionValue) <- zip arguments (drop 1 arguments),
    argument == optionName
  ]
    <> [ optionValue
       | argument <- arguments,
         Just optionValue <- [Text.stripPrefix (optionName <> "=") argument]
       ]

certbotHasOption :: Text -> [Text] -> Bool
certbotHasOption optionName =
  not . null . certbotOptionValues optionName

certbotHasFlag :: Text -> [Text] -> Bool
certbotHasFlag =
  elem

certbotNeedsDerivedWebrootAuthenticator :: [Text] -> Bool
certbotNeedsDerivedWebrootAuthenticator configuredArguments =
  not (certbotHasExplicitAuthenticator configuredArguments)
    || (certbotHasWebrootPathOption configuredArguments && not (certbotUsesWebrootFlagOrAuthenticator configuredArguments))

certbotShouldUseWebroot :: [Text] -> Bool
certbotShouldUseWebroot configuredArguments =
  certbotNeedsDerivedWebrootAuthenticator configuredArguments
    || certbotUsesWebroot configuredArguments

certbotHasExplicitAuthenticator :: [Text] -> Bool
certbotHasExplicitAuthenticator configuredArguments =
  certbotUsesWebroot configuredArguments
    || any (`certbotHasFlag` configuredArguments) ["--standalone", "--manual", "--apache", "--nginx"]
    || any ("--dns-" `Text.isPrefixOf`) configuredArguments
    || any isExplicitAuthenticator (certbotAuthenticatorValues configuredArguments)

certbotAuthenticatorValues :: [Text] -> [Text]
certbotAuthenticatorValues configuredArguments =
  certbotOptionValues "-a" configuredArguments
    <> certbotOptionValues "--authenticator" configuredArguments

isExplicitAuthenticator :: Text -> Bool
isExplicitAuthenticator authenticator =
  authenticator `elem` ["standalone", "manual", "apache", "nginx"]
    || "dns-" `Text.isPrefixOf` authenticator

certbotUsesWebroot :: [Text] -> Bool
certbotUsesWebroot configuredArguments =
  certbotUsesWebrootFlagOrAuthenticator configuredArguments
    || certbotHasWebrootPathOption configuredArguments

certbotUsesWebrootFlagOrAuthenticator :: [Text] -> Bool
certbotUsesWebrootFlagOrAuthenticator configuredArguments =
  certbotHasFlag "--webroot" configuredArguments
    || elem
      "webroot"
      (certbotOptionValues "-a" configuredArguments <> certbotOptionValues "--authenticator" configuredArguments)

certbotHasWebrootPathOption :: [Text] -> Bool
certbotHasWebrootPathOption configuredArguments =
  certbotHasOption "-w" configuredArguments
    || certbotHasOption "--webroot-path" configuredArguments
