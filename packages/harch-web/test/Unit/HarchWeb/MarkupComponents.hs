{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.MarkupComponents
  ( AccountProfile (..),
    AvatarSize (..),
    HeroCardProps (..),
    ItemProps (..),
    ProfileCardProps (..),
    TypedActionFormProps (..),
    heroCard,
    item,
    profileCard,
    typedActionForm,
    userAvatar,
  )
where

import Data.Text (Text)
import HarchWeb (Html, dataAttribute, element, headingTwoTag, paragraphTag, sectionTag, text)
import HarchWeb qualified
import HarchWeb.Action qualified as Action

newtype HeroCardProps = HeroCardProps
  { heroTitle :: Text
  }

heroCard :: HeroCardProps -> [Html] -> Html
heroCard props children =
  element
    sectionTag
    [dataAttribute "hero-card" "true"]
    (element headingTwoTag [] [text (heroTitle props)] : children)

newtype ItemProps = ItemProps
  { itemLabel :: Text
  }

item :: ItemProps -> [Html] -> Html
item props _children = text (itemLabel props)

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

data TypedActionFormProps = TypedActionFormProps
  { action :: Text,
    ariaLabel :: Text
  }

typedActionForm :: TypedActionFormProps -> [Html] -> Html
typedActionForm TypedActionFormProps {action, ariaLabel} =
  HarchWeb.actionForm typedActionCodec () action HarchWeb.defaultActionFormAttributes {HarchWeb.actionFormAriaLabel = Just ariaLabel}

typedActionCodec :: Action.ActionCodec Text () Text
typedActionCodec =
  case Action.actionCodec [Action.action "/actions/subscribe" (Action.post "/actions/subscribe") (pure "/actions/subscribe")] of
    Left codecError -> error (show codecError)
    Right codec -> codec
