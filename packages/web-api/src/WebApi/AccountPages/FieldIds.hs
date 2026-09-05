{-# LANGUAGE OverloadedStrings #-}

-- | Stable DOM identities shared by account-form rendering and action focus.
-- Keeping these as typed values prevents response producers from drifting
-- away from the controls replaced by their region patches.
module WebApi.AccountPages.FieldIds
  ( loginAuthenticatorCodeErrorId,
    loginAuthenticatorCodeHintId,
    loginAuthenticatorCodeId,
    loginIdentifierErrorId,
    loginIdentifierId,
    loginPasswordId,
    loginPasswordErrorId,
    loginProofId,
    loginProofErrorId,
    loginRecoveryCodeErrorId,
    loginRecoveryCodeHintId,
    loginRecoveryCodeId,
    loginSummaryId,
    mfaCodeId,
    mfaCodeErrorId,
    mfaCodeHintId,
    registrationDisplayNameId,
    registrationEmailId,
    registrationEmailErrorId,
    registrationPasswordId,
    registrationPasswordErrorId,
    registrationPasswordHintId,
    registrationSummaryId,
    registrationUsernameId,
    registrationUsernameErrorId,
    verificationTokenId,
    verificationTokenErrorId,
    verificationTokenHintId,
  )
where

import HarchWeb qualified

loginAuthenticatorCodeErrorId, loginAuthenticatorCodeHintId, loginAuthenticatorCodeId, loginIdentifierErrorId, loginIdentifierId, loginPasswordErrorId, loginPasswordId, loginProofErrorId, loginProofId, loginRecoveryCodeErrorId, loginRecoveryCodeHintId, loginRecoveryCodeId, loginSummaryId :: HarchWeb.ElementId
loginAuthenticatorCodeId = HarchWeb.literalElementId "login-authenticator-code"
loginAuthenticatorCodeHintId = HarchWeb.literalElementId "login-authenticator-code-hint"
loginAuthenticatorCodeErrorId = HarchWeb.literalElementId "login-authenticator-code-error"
loginIdentifierId = HarchWeb.literalElementId "login-identifier"
loginIdentifierErrorId = HarchWeb.literalElementId "login-identifier-error"
loginPasswordId = HarchWeb.literalElementId "login-password"
loginPasswordErrorId = HarchWeb.literalElementId "login-password-error"
loginProofId = HarchWeb.literalElementId "login-proof"
loginProofErrorId = HarchWeb.literalElementId "login-proof-error"
loginRecoveryCodeId = HarchWeb.literalElementId "login-recovery-code"
loginRecoveryCodeHintId = HarchWeb.literalElementId "login-recovery-code-hint"
loginRecoveryCodeErrorId = HarchWeb.literalElementId "login-recovery-code-error"
loginSummaryId = HarchWeb.literalElementId "login-error-summary"

mfaCodeErrorId, mfaCodeHintId, mfaCodeId :: HarchWeb.ElementId
mfaCodeId = HarchWeb.literalElementId "mfa-code"
mfaCodeHintId = HarchWeb.literalElementId "mfa-code-hint"
mfaCodeErrorId = HarchWeb.literalElementId "mfa-code-error"

registrationDisplayNameId, registrationEmailErrorId, registrationEmailId, registrationPasswordErrorId, registrationPasswordHintId, registrationPasswordId, registrationSummaryId, registrationUsernameErrorId, registrationUsernameId :: HarchWeb.ElementId
registrationDisplayNameId = HarchWeb.literalElementId "registration-display-name"
registrationEmailId = HarchWeb.literalElementId "registration-email"
registrationEmailErrorId = HarchWeb.literalElementId "registration-email-error"
registrationPasswordId = HarchWeb.literalElementId "registration-password"
registrationPasswordErrorId = HarchWeb.literalElementId "registration-password-error"
registrationPasswordHintId = HarchWeb.literalElementId "registration-password-hint"
registrationSummaryId = HarchWeb.literalElementId "registration-error-summary"
registrationUsernameId = HarchWeb.literalElementId "registration-username"
registrationUsernameErrorId = HarchWeb.literalElementId "registration-username-error"

verificationTokenErrorId, verificationTokenHintId, verificationTokenId :: HarchWeb.ElementId
verificationTokenId = HarchWeb.literalElementId "verification-token"
verificationTokenHintId = HarchWeb.literalElementId "verification-token-hint"
verificationTokenErrorId = HarchWeb.literalElementId "verification-token-error"
