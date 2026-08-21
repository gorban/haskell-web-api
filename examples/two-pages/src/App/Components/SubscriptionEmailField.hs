{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Components.SubscriptionEmailField
  ( SubscriptionEmailFieldProps (..),
    subscriptionEmailField,
  )
where

import HarchWeb
  ( Html,
    harch,
  )

data SubscriptionEmailFieldProps = SubscriptionEmailFieldProps

subscriptionEmailField :: SubscriptionEmailFieldProps -> [Html] -> Html
subscriptionEmailField SubscriptionEmailFieldProps children =
  [harch|
    <label for="subscription-email">Email address</label>
    <input id="subscription-email" name="email" type="email" autocomplete="email" required />
    {children}
  |]
