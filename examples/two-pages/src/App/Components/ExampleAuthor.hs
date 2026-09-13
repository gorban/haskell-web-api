{-# LANGUAGE OverloadedStrings #-}

module App.Components.ExampleAuthor
  ( AuthorCardProps (..),
    AuthorIdentity (..),
    AvatarSize (..),
    authorAvatar,
    authorCard,
  )
where

import Data.Text (Text)
import HarchWeb
  ( Html,
    dataAttribute,
    divTag,
    element,
    paragraphTag,
    sectionTag,
    text,
  )

data AuthorCardProps = AuthorCardProps
  { authorName :: Text,
    authorRole :: Text
  }

authorCard :: AuthorCardProps -> [Html] -> Html
authorCard props children =
  element
    sectionTag
    [dataAttribute "example-author-card" "true"]
    ( element paragraphTag [] [text (authorName props)]
        : element paragraphTag [] [text (authorRole props)]
        : children
    )

newtype AuthorIdentity = AuthorIdentity Text

data AvatarSize = CompactAvatar

authorAvatar :: AuthorIdentity -> AvatarSize -> [Html] -> Html
authorAvatar (AuthorIdentity initials) CompactAvatar children =
  element
    divTag
    [dataAttribute "example-author-avatar" "compact"]
    (element paragraphTag [] [text initials] : children)
