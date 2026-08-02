{-# LANGUAGE OverloadedStrings #-}

module App.Components.SubscriptionEmailField
  ( SubscriptionEmailFieldProps (..),
    subscriptionEmailField,
  )
where

import HarchWeb
  ( Html,
    autocomplete,
    element,
    elementId,
    fragment,
    inputTag,
    inputType,
    labelFor,
    labelTag,
    literalElementId,
    name,
    required,
    text,
    voidElement,
  )

data SubscriptionEmailFieldProps = SubscriptionEmailFieldProps

subscriptionEmailField :: SubscriptionEmailFieldProps -> [Html] -> Html
subscriptionEmailField SubscriptionEmailFieldProps children =
  fragment
    [ element labelTag [labelFor emailId] [text "Email address"],
      voidElement inputTag [elementId emailId, name "email", inputType "email", autocomplete "email", required],
      fragment children
    ]
  where
    emailId = literalElementId "subscription-email"
