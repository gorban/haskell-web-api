{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.MarkupComponents
  ( AccountProfile (..),
    AvatarSize (..),
    HeroCardProps (..),
    ProfileCardProps (..),
    heroCard,
    profileCard,
    userAvatar,
  )
where

import Data.Text (Text)
import HarchWeb (Html, dataAttribute, element, headingTwoTag, paragraphTag, sectionTag, text)

newtype HeroCardProps = HeroCardProps
  { heroTitle :: Text
  }

heroCard :: HeroCardProps -> [Html] -> Html
heroCard props children =
  element
    sectionTag
    [dataAttribute "hero-card" "true"]
    (element headingTwoTag [] [text (heroTitle props)] : children)

newtype ProfileCardProps = ProfileCardProps
  { profileCardTitle :: Text
  }

profileCard :: ProfileCardProps -> [Html] -> Html
profileCard props children =
  element paragraphTag [dataAttribute "profile-card" "true"] (text (profileCardTitle props) : children)

newtype AccountProfile = AccountProfile Text

data AvatarSize = SmallAvatar

userAvatar :: AccountProfile -> AvatarSize -> [Html] -> Html
userAvatar (AccountProfile accountName) SmallAvatar children =
  element paragraphTag [dataAttribute "user-avatar" "small"] (text accountName : children)
