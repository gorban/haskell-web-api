module WebApi.Page.Model
  ( AppPageModel (..),
    AuthenticatedProfilePageDetails (..),
    CallToAction (..),
    NotFoundPageModel (..),
    LanguagePageModel (..),
    HelpPageModel (..),
    PendingProfilePageDetails (..),
    ProfilePageModel (..),
    SecondPageModel (..),
    SignedOutProfilePageDetails (..),
    SpacesPageModel (..),
    UnavailableProfilePageDetails (..),
  )
where

import Data.Text (Text)
import HarchWeb (SafeUrl)
import WebApi.AccountPages.Actions.Contract (AccountActionTarget)
import WebApi.AccountPages.Forms
  ( LoginForm (..),
    MfaEnrollmentForm (..),
    RegistrationForm (..),
    VerificationForm (..),
  )
import WebApi.Route (AppRoute)

data CallToAction = CallToAction
  { callToActionLabel :: Text,
    callToActionRoute :: AppRoute,
    callToActionHref :: SafeUrl
  }
  deriving (Eq, Show)

data SecondPageModel = SecondPageModel
  { secondHeading :: Text,
    secondSummary :: Text,
    secondHighlights :: [Text],
    secondErrorMessage :: Maybe Text,
    secondPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data SpacesPageModel = SpacesPageModel
  { spacesHeading :: Text,
    spacesSummary :: Text
  }
  deriving (Eq, Show)

data NotFoundPageModel = NotFoundPageModel
  { notFoundHeading :: Text,
    notFoundSummary :: Text,
    notFoundPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data LanguagePageModel = LanguagePageModel
  { languageHeading :: Text,
    languageSummary :: Text
  }
  deriving (Eq, Show)

data HelpPageModel = HelpPageModel
  { helpHeading :: Text,
    helpSummary :: Text,
    helpAccountGuidance :: Text,
    helpSignInAction :: CallToAction,
    helpRegistrationAction :: CallToAction
  }
  deriving (Eq, Show)

data ProfilePageModel
  = SignedOutProfilePage SignedOutProfilePageDetails
  | PendingProfilePage PendingProfilePageDetails
  | AuthenticatedProfilePage AuthenticatedProfilePageDetails
  | UnavailableProfilePage UnavailableProfilePageDetails
  deriving (Eq, Show)

-- | Every 'ProfilePageModel' constructor's fields are wrapped in their own
-- single-constructor record like this one, following this module's own
-- 'SecondPageModel'\/'NotFoundPageModel' convention of a
-- type-specific field prefix, so each accessor is total: a field once
-- absent from some 'ProfilePageModel' constructors (e.g. only
-- 'SignedOutProfilePage' and 'UnavailableProfilePage' had a sign-in action)
-- made every other constructor's use of that field partial.
data SignedOutProfilePageDetails = SignedOutProfilePageDetails
  { signedOutProfileHeading :: Text,
    signedOutProfileSummary :: Text,
    signedOutProfileSignInAction :: CallToAction,
    signedOutProfileRegistrationAction :: CallToAction
  }
  deriving (Eq, Show)

data PendingProfilePageDetails = PendingProfilePageDetails
  { pendingProfileHeading :: Text,
    pendingProfileSummary :: Text,
    pendingProfileEmail :: Text,
    pendingProfileUsername :: Maybe Text,
    pendingProfileDisplayName :: Maybe Text,
    pendingProfileResendPath :: AccountActionTarget,
    pendingProfileResendLabel :: Text,
    pendingProfileSignOutAction :: CallToAction
  }
  deriving (Eq, Show)

data AuthenticatedProfilePageDetails = AuthenticatedProfilePageDetails
  { authenticatedProfileHeading :: Text,
    authenticatedProfileSummary :: Text,
    authenticatedProfileEmail :: Text,
    authenticatedProfileUsername :: Maybe Text,
    authenticatedProfileDisplayName :: Maybe Text,
    authenticatedProfileSignOutAction :: CallToAction
  }
  deriving (Eq, Show)

data UnavailableProfilePageDetails = UnavailableProfilePageDetails
  { unavailableProfileHeading :: Text,
    unavailableProfileSummary :: Text,
    unavailableProfileSignInAction :: CallToAction
  }
  deriving (Eq, Show)

data AppPageModel
  = SecondPage SecondPageModel
  | SpacesPage SpacesPageModel
  | RegistrationPage AccountActionTarget RegistrationForm
  | EmailVerificationPage AccountActionTarget VerificationForm
  | MfaEnrollmentPage AccountActionTarget MfaEnrollmentForm
  | LoginPage AccountActionTarget LoginForm
  | LogoutPage AccountActionTarget
  | ProfilePage ProfilePageModel
  | LanguagePage LanguagePageModel
  | HelpPage HelpPageModel
  | NotFoundPage NotFoundPageModel
  deriving (Eq, Show)
