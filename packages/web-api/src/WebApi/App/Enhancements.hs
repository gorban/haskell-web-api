{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Enhancements
  ( pageEnhancementHooks,
  )
where

import Data.Text (Text)
import WebApi.Route (AppRoute (..))

pageEnhancementHooks :: AppRoute -> [Text]
pageEnhancementHooks route =
  case route of
    HomeRoute -> []
    SecondRoute -> ["second-page"]
    RegistrationRoute -> []
    EmailVerificationRoute -> []
    MfaEnrollmentRoute -> []
    LoginRoute -> []
    LogoutRoute -> []
    StatusApiRoute -> []
    NotFoundRoute -> []
