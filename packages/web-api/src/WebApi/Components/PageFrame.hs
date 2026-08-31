{-# LANGUAGE OverloadedStrings #-}

-- | App-owned page composition primitives.
--
-- AHI-1 deliberately keeps this layer as ordinary, pure typed functions over
-- 'HarchWeb.Html'.  Harch already supplies the escaping, markup, and scoped
-- class boundaries, so a framework design-system or CSS EDSL would duplicate
-- an existing capability without evidence of a framework gap.
module WebApi.Components.PageFrame
  ( PageFrameProps (..),
    PageKind (..),
    pageFrame,
  )
where

import Data.Text (Text)
import HarchWeb qualified

-- | The page kinds rendered by the reference application's shared frame.
-- Keeping the set closed prevents arbitrary strings from silently becoming
-- selector and test contracts.
data PageKind
  = SecondPageFrame
  | SpacesPageFrame
  | RegistrationPageFrame
  | EmailVerificationPageFrame
  | MfaEnrollmentPageFrame
  | LoginPageFrame
  | LogoutPageFrame
  | ProfilePageFrame
  | NotFoundPageFrame

-- | Cohesive, explicit inputs for the repeated page-level structure.
data PageFrameProps = PageFrameProps
  { pageFrameKind :: PageKind,
    pageFrameHeading :: Text,
    pageFrameSummary :: Maybe Text,
    pageFrameContent :: [HarchWeb.Html]
  }

pageFrame :: PageFrameProps -> HarchWeb.Html
pageFrame PageFrameProps {pageFrameKind, pageFrameHeading, pageFrameSummary, pageFrameContent} =
  HarchWeb.element
    HarchWeb.sectionTag
    [ HarchWeb.dataAttribute "page" (pageKindText pageFrameKind),
      HarchWeb.className (HarchWeb.ScopedCssClass pageFrameScope "root")
    ]
    [ HarchWeb.element
        HarchWeb.headingOneTag
        [ HarchWeb.dataAttribute "page-title" "true",
          HarchWeb.className (HarchWeb.ScopedCssClass pageFrameScope "title")
        ]
        [HarchWeb.text pageFrameHeading],
      maybe
        (HarchWeb.fragment [])
        ( HarchWeb.element
            HarchWeb.paragraphTag
            [HarchWeb.className (HarchWeb.ScopedCssClass pageFrameScope "summary")]
            . pure
            . HarchWeb.text
        )
        pageFrameSummary,
      HarchWeb.element
        HarchWeb.divTag
        [HarchWeb.className (HarchWeb.ScopedCssClass pageFrameScope "content")]
        pageFrameContent
    ]

pageFrameScope :: HarchWeb.CssScope
pageFrameScope = HarchWeb.cssScope "page-frame"

pageKindText :: PageKind -> Text
pageKindText pageKind =
  case pageKind of
    SecondPageFrame -> "second"
    SpacesPageFrame -> "spaces"
    RegistrationPageFrame -> "registration"
    EmailVerificationPageFrame -> "email-verification"
    MfaEnrollmentPageFrame -> "mfa-enrollment"
    LoginPageFrame -> "login"
    LogoutPageFrame -> "logout"
    ProfilePageFrame -> "profile"
    NotFoundPageFrame -> "not-found"
