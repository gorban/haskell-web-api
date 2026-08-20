-- | Public ACME and certificate-automation API.
--
-- The implementation is deliberately split into private modules so callers can
-- depend on this stable boundary rather than certbot or challenge-store
-- internals. Certificate acquisition is always certbot-backed
-- ('AcmeConfig' requires a 'CertbotConfig'); see the DG decision record in
-- @docs/design-guidance.md@ for why an in-process ACME protocol client is
-- deliberately not part of this surface.
module HarchWeb.Acme
  ( AcmeChallengeStore (..),
    AcmeConfig (..),
    ActiveAcmeChallenge (..),
    CertbotConfig (..),
    JsonValue (..),
    RuntimeAcmeBindPlan (..),
    acmeChallengeResponseForRequest,
    acmeHttp01ChallengeToken,
    certbotCertificateName,
    certbotHasOption,
    certbotOptionValues,
    escapeJsonCharacter,
    firstCertbotDomain,
    jsonArrayBytes,
    jsonArrayItems,
    jsonBoolBytes,
    jsonObjectEntryParser,
    jsonObjectBytes,
    jsonObjectFields,
    jsonOptionalTextArrayField,
    jsonOptionalTextField,
    jsonRequiredField,
    jsonRequiredTextField,
    jsonStringCharacterParser,
    jsonStringBytes,
    jsonTextField,
    jsonValueParser,
    matchesRuntimeAcmeChallenge,
    parseJsonValue,
    prepareCertbotManualTlsBindPlan,
    registerAcmeChallenges,
    runtimeCertbotArguments,
    splitCertbotDomainValue,
    unicodeJsonCharacterParser,
    unregisterAcmeChallenges,
    validAcmeHttp01ChallengeToken,
  )
where

import HarchWeb.Acme.Certbot.Options
  ( certbotHasOption,
    certbotOptionValues,
    firstCertbotDomain,
    splitCertbotDomainValue,
  )
import HarchWeb.Acme.Certbot.Runtime
  ( RuntimeAcmeBindPlan (..),
    certbotCertificateName,
    prepareCertbotManualTlsBindPlan,
    runtimeCertbotArguments,
  )
import HarchWeb.Acme.Challenge
  ( AcmeChallengeStore (..),
    ActiveAcmeChallenge (..),
    acmeChallengeResponseForRequest,
    acmeHttp01ChallengeToken,
    matchesRuntimeAcmeChallenge,
    registerAcmeChallenges,
    unregisterAcmeChallenges,
    validAcmeHttp01ChallengeToken,
  )
import HarchWeb.Acme.Json
  ( JsonValue (..),
    escapeJsonCharacter,
    jsonArrayBytes,
    jsonArrayItems,
    jsonBoolBytes,
    jsonObjectBytes,
    jsonObjectEntryParser,
    jsonObjectFields,
    jsonOptionalTextArrayField,
    jsonOptionalTextField,
    jsonRequiredField,
    jsonRequiredTextField,
    jsonStringBytes,
    jsonStringCharacterParser,
    jsonTextField,
    jsonValueParser,
    parseJsonValue,
    unicodeJsonCharacterParser,
  )
import HarchWeb.Server.Config (AcmeConfig (..), CertbotConfig (..))
