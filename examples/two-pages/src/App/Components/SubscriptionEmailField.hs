{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Components.SubscriptionEmailField
  ( SubscriptionEmailFieldProps (..),
    subscriptionEmailId,
    subscriptionEmailField,
  )
where

import HarchWeb
  ( ElementId,
    Html,
    harch,
    literalElementId,
  )

data SubscriptionEmailFieldProps = SubscriptionEmailFieldProps

subscriptionEmailId :: ElementId
subscriptionEmailId = literalElementId "subscription-email"

subscriptionEmailField :: SubscriptionEmailFieldProps -> [Html] -> Html
subscriptionEmailField SubscriptionEmailFieldProps children =
  [harch|
    <label for={subscriptionEmailId}>Email address</label>
    <input id={subscriptionEmailId} name="email" type="email" autocomplete="email" required />
    {children}
  |]
