{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.MarkupComponents
  ( ProfileCardProps (..),
    profileCard,
  )
where

import Data.Text (Text)
import HarchWeb (Html, dataAttribute, element, paragraphTag, text)

newtype ProfileCardProps = ProfileCardProps
  { profileCardTitle :: Text
  }

profileCard :: ProfileCardProps -> [Html] -> Html
profileCard props children =
  element paragraphTag [dataAttribute "profile-card" "true"] (text (profileCardTitle props) : children)
