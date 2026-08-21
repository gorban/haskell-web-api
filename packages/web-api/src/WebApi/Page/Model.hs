module WebApi.Page.Model
  ( AppPageModel (..),
    CallToAction (..),
    HomePageModel (..),
    NotFoundPageModel (..),
    ProfilePageModel (..),
    SecondPageModel (..),
    SpacesPageModel (..),
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

data HomePageModel = HomePageModel
  { homeHeading :: Text,
    homeSummary :: Text,
    homeErrorMessage :: Maybe Text,
    homePrimaryAction :: CallToAction
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

data ProfilePageModel
  = SignedOutProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileSignInAction :: CallToAction,
        profileRegistrationAction :: CallToAction
      }
  | PendingProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileEmail :: Text,
        profileUsername :: Maybe Text,
        profileDisplayName :: Maybe Text,
        profileResendPath :: AccountActionTarget,
        profileResendLabel :: Text,
        profileSignOutAction :: CallToAction
      }
  | AuthenticatedProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileEmail :: Text,
        profileUsername :: Maybe Text,
        profileDisplayName :: Maybe Text,
        profileSignOutAction :: CallToAction
      }
  | UnavailableProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileSignInAction :: CallToAction
      }
  deriving (Eq, Show)

data AppPageModel
  = HomePage HomePageModel
  | SecondPage SecondPageModel
  | SpacesPage SpacesPageModel
  | RegistrationPage AccountActionTarget RegistrationForm
  | EmailVerificationPage AccountActionTarget VerificationForm
  | MfaEnrollmentPage AccountActionTarget MfaEnrollmentForm
  | LoginPage AccountActionTarget LoginForm
  | LogoutPage AccountActionTarget
  | ProfilePage ProfilePageModel
  | NotFoundPage NotFoundPageModel
  deriving (Eq, Show)
