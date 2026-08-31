{-# LANGUAGE OverloadedStrings #-}

-- | Stable DOM identities shared by account-form rendering and action focus.
-- Keeping these as typed values prevents response producers from drifting
-- away from the controls replaced by their region patches.
module WebApi.AccountPages.FieldIds
  ( loginCodeId,
    loginEmailId,
    loginPasswordId,
    loginProofId,
    mfaCodeId,
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
  )
where

import HarchWeb qualified

loginCodeId, loginEmailId, loginPasswordId, loginProofId :: HarchWeb.ElementId
loginCodeId = HarchWeb.literalElementId "login-code"
loginEmailId = HarchWeb.literalElementId "login-email"
loginPasswordId = HarchWeb.literalElementId "login-password"
loginProofId = HarchWeb.literalElementId "login-proof"

mfaCodeId :: HarchWeb.ElementId
mfaCodeId = HarchWeb.literalElementId "mfa-code"

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

verificationTokenId :: HarchWeb.ElementId
verificationTokenId = HarchWeb.literalElementId "verification-token"
