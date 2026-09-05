{-# LANGUAGE OverloadedStrings #-}

-- | Pure app-owned profile presentation.
--
-- AHI-1 keeps optional identity values explicit in props and renders absence
-- as absence.  The component does not read session/application state; the
-- exhaustive 'WebApi.Page.Model.ProfilePageModel' fold remains at the route
-- rendering boundary that owns those states.
module WebApi.Components.Profile
  ( ProfileIdentityProps (..),
    profileIdentity,
  )
where

import Data.Text (Text)
import HarchWeb qualified

data ProfileIdentityProps = ProfileIdentityProps
  { profileIdentityUsername :: Maybe Text,
    profileIdentityDisplayName :: Maybe Text,
    profileIdentityEmail :: Maybe Text
  }

profileIdentity :: ProfileIdentityProps -> HarchWeb.Html
profileIdentity ProfileIdentityProps {profileIdentityUsername, profileIdentityDisplayName, profileIdentityEmail} =
  HarchWeb.fragment
    [ optionalIdentityValue "profile-username" profileIdentityUsername,
      optionalIdentityValue "profile-display-name" profileIdentityDisplayName,
      optionalIdentityValue "profile-email" profileIdentityEmail
    ]

optionalIdentityValue :: HarchWeb.DataAttributeSuffix -> Maybe Text -> HarchWeb.Html
optionalIdentityValue attributeName =
  maybe
    (HarchWeb.fragment [])
    ( HarchWeb.element
        HarchWeb.paragraphTag
        [ HarchWeb.dataAttribute attributeName "true",
          HarchWeb.className (HarchWeb.ScopedCssClass profileIdentityScope "value")
        ]
        . pure
        . HarchWeb.text
    )

profileIdentityScope :: HarchWeb.CssScope
profileIdentityScope = HarchWeb.cssScope "profile-identity"
