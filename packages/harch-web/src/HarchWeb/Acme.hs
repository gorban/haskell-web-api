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
    RuntimeAcmeBindPlan (..),
    acmeChallengeResponseForRequest,
    acmeHttp01ChallengeToken,
    certbotCertificateName,
    certbotHasOption,
    certbotOptionValues,
    firstCertbotDomain,
    matchesRuntimeAcmeChallenge,
    prepareCertbotManualTlsBindPlan,
    registerAcmeChallenges,
    runtimeCertbotArguments,
    splitCertbotDomainValue,
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
import HarchWeb.Server.Config (AcmeConfig (..), CertbotConfig (..))
