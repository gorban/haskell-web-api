{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import WebApi.AccountPages (AccountActionTarget (..))
import WebApi.Page (AppPageModel (..), AuthenticatedProfilePageDetails (..), CallToAction (..), PendingProfilePageDetails (..), ProfilePageModel (..), SignedOutProfilePageDetails (..), UnavailableProfilePageDetails (..))
import WebApi.Route (AppRoute (..))

spec =
  describe "ProfilePageModel and its detail records" $
    it "compares every rendered profile identity field and keeps models printable" $ do
      let signInAction = CallToAction "Sign in" LoginRoute "/login"
          registrationAction = CallToAction "Create account" RegistrationRoute "/register"
          signOutAction = CallToAction "Sign out" LogoutRoute "/logout"
          signedOutModel = SignedOutProfilePage (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction)
          pendingModel = PendingProfilePage (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction)
          pendingModelWithIdentity = PendingProfilePage (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" (Just "pending-person") (Just "Pending Person") UpdateProfileTarget "Resend verification email" signOutAction)
          authenticatedModel = AuthenticatedProfilePage (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction)
          authenticatedModelWithIdentity = AuthenticatedProfilePage (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" (Just "authenticated-person") (Just "Authenticated Person") signOutAction)
          unavailableModel = UnavailableProfilePage (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction)
          models =
            [ (signedOutModel, "SignedOutProfilePage"),
              (pendingModel, "PendingProfilePage"),
              (authenticatedModel, "AuthenticatedProfilePage"),
              (unavailableModel, "UnavailableProfilePage")
            ]
      mapM_ assertProfilePageModelShow models
      -- Each detail record's own 'deriving (Eq, Show)' is only reached
      -- indirectly above, through the outer 'ProfilePageModel' constructor's
      -- derived instances; HPC does not credit those four declarations from
      -- that alone, confirmed directly by the coverage gate rather than
      -- assumed, so each is exercised here too, directly and on its own.
      -- Same-value, different-construction (not 'x == x').
      SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction
        `shouldBe` SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction
      PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
        `shouldBe` PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
      AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
        `shouldBe` AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
      UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction
        `shouldBe` UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction
      -- 'deriving (Eq)' writes only '=='; the unoverridden '/=' default
      -- method HPC boxes separately (this codebase's own established
      -- derived-instance lesson), so a genuine inequality is exercised too.
      SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction
        `shouldNotBe` SignedOutProfilePageDetails "Other" "Sign in to view and manage your profile." signInAction registrationAction
      PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
        `shouldNotBe` PendingProfilePageDetails "Other" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
      AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
        `shouldNotBe` AuthenticatedProfilePageDetails "Other" "You are signed in." "person@example.test" Nothing Nothing signOutAction
      UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction
        `shouldNotBe` UnavailableProfilePageDetails "Other" "Your profile is temporarily unavailable." signInAction
      show (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction)
        `shouldContain` "signedOutProfileHeading = \"Profile\""
      show (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction)
        `shouldContain` "pendingProfileHeading = \"Profile\""
      show (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction)
        `shouldContain` "authenticatedProfileHeading = \"Profile\""
      show (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction)
        `shouldContain` "unavailableProfileHeading = \"Profile\""
      -- Derived 'Show' also writes distinct 'showsPrec'/'showList' methods.
      showsPrec 11 (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction) ""
        `shouldBe` "("
        <> show (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction)
        <> ")"
      showsPrec 11 (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction) ""
        `shouldBe` "("
        <> show (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction)
        <> ")"
      showsPrec 11 (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction) ""
        `shouldBe` "("
        <> show (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction)
        <> ")"
      showsPrec 11 (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction) ""
        `shouldBe` "("
        <> show (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction)
        <> ")"
      show [SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction]
        `shouldContain` "signedOutProfileHeading = \"Profile\""
      show [PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction]
        `shouldContain` "pendingProfileHeading = \"Profile\""
      show [AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction]
        `shouldContain` "authenticatedProfileHeading = \"Profile\""
      show [UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction]
        `shouldContain` "unavailableProfileHeading = \"Profile\""
      expectAll
        ( (ProfilePage signedOutModel == ProfilePage pendingModel `shouldBe` False)
            :| [ (pendingModel /= pendingModelWithIdentity)
                   `shouldBe` True,
                 (authenticatedModel /= authenticatedModelWithIdentity)
                   `shouldBe` True,
                 (pendingModel /= authenticatedModel)
                   `shouldBe` True
               ]
        )

assertProfilePageModelShow :: (ProfilePageModel, Text) -> Expectation
assertProfilePageModelShow (profilePageModel, expectedPrefix) =
  Text.pack (show (ProfilePage profilePageModel)) `shouldSatisfy` Text.isPrefixOf ("ProfilePage (" <> expectedPrefix)
