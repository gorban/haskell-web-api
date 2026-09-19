-- | Stable, source-compatible CSRF facade.
--
-- Most applications need the page/action lifecycle exported here.  A signed
-- authority is configured through 'HarchWeb.Csrf.Signed': its keyring, policy,
-- clock, and binding resolver are deployment collaborators, rather than page
-- lifecycle inputs.  The signed names remain re-exported here for existing
-- imports, but new code should import that focused public module directly.
--
-- Decision (Q-F7, 2026-09-19): keep this compatibility façade because moving
-- its established signed configuration names would be a source-incompatible
-- API break and would not make a key or token less observable.  Expose the
-- separately documented signed configuration module instead, while keeping
-- one opaque 'CsrfProtection' lifecycle and the private implementation owner.
module HarchWeb.Csrf
  ( CsrfBinding,
    CsrfBindingDigest,
    CsrfBindingResolution (..),
    CsrfCookieDisposition (..),
    CsrfCookieMaxAgeSeconds,
    CsrfKeyId,
    CsrfIssuance (..),
    CsrfPagePreparationFailure (..),
    CsrfProtection (..),
    CsrfSigningKey,
    CsrfToken,
    CsrfVerification (..),
    SignedCsrfKeyring,
    SignedCsrfPolicy,
    PageCsrf,
    PageSecurity,
    csrfBindingFromCanonicalBytes,
    csrfBindingDigest,
    csrfBindingDigestText,
    csrfClearCookieHeader,
    csrfCookieMaxAgeSeconds,
    mkCsrfCookieMaxAgeSeconds,
    csrfTokenText,
    defaultCsrfCookieMaxAgeSeconds,
    defaultSignedCsrfPolicy,
    generateCsrfSigningKey,
    generateCsrfToken,
    mkCsrfKeyId,
    mkCsrfToken,
    mkSignedCsrfPolicy,
    mkCsrfSigningKey,
    mkSignedCsrfKeyring,
    mkPageCsrf,
    mkPageSecurity,
    pageCsrfBinding,
    pageCsrfCookieDisposition,
    pageCsrfCookieMaxAge,
    pageCsrfValue,
    pageSecurityCsrf,
    pageSecurityRuntimeNonce,
    samePageSecurity,
    preparePageSecurity,
    csrfProtectionUnavailable,
    SignedCsrfDependencies (..),
    signedCsrfProtection,
    validateCsrfToken,
  )
where

import HarchWeb.Csrf.Lifecycle
import HarchWeb.Csrf.Signed (SignedCsrfDependencies (..), signedCsrfProtection)
