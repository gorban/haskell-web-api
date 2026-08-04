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
    callToActionHref :: Text
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

instance Eq ProfilePageModel where
  left == right =
    case (left, right) of
      ( SignedOutProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileSignInAction = leftSignInAction, profileRegistrationAction = leftRegistrationAction},
        SignedOutProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileSignInAction = rightSignInAction, profileRegistrationAction = rightRegistrationAction}
        ) ->
          (leftHeading, leftSummary, leftSignInAction, leftRegistrationAction)
            == (rightHeading, rightSummary, rightSignInAction, rightRegistrationAction)
      ( PendingProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileEmail = leftEmail, profileResendPath = leftResendPath, profileResendLabel = leftResendLabel, profileSignOutAction = leftSignOutAction},
        PendingProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileEmail = rightEmail, profileResendPath = rightResendPath, profileResendLabel = rightResendLabel, profileSignOutAction = rightSignOutAction}
        ) ->
          (leftHeading, leftSummary, leftEmail, leftResendPath, leftResendLabel, leftSignOutAction)
            == (rightHeading, rightSummary, rightEmail, rightResendPath, rightResendLabel, rightSignOutAction)
      ( AuthenticatedProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileEmail = leftEmail, profileSignOutAction = leftSignOutAction},
        AuthenticatedProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileEmail = rightEmail, profileSignOutAction = rightSignOutAction}
        ) ->
          (leftHeading, leftSummary, leftEmail, leftSignOutAction)
            == (rightHeading, rightSummary, rightEmail, rightSignOutAction)
      ( UnavailableProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileSignInAction = leftSignInAction},
        UnavailableProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileSignInAction = rightSignInAction}
        ) ->
          (leftHeading, leftSummary, leftSignInAction)
            == (rightHeading, rightSummary, rightSignInAction)
      _ -> False

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

instance Eq AppPageModel where
  left == right = equalAppPageModel left right

equalAppPageModel :: AppPageModel -> AppPageModel -> Bool
equalAppPageModel left right =
  case left of
    HomePage page -> equalHomePage page right
    SecondPage page -> equalSecondPage page right
    SpacesPage page -> equalSpacesPage page right
    RegistrationPage path form -> equalRegistrationPage path form right
    EmailVerificationPage path form -> equalEmailVerificationPage path form right
    MfaEnrollmentPage path form -> equalMfaEnrollmentPage path form right
    LoginPage path form -> equalLoginPage path form right
    LogoutPage path -> equalLogoutPage path right
    ProfilePage page -> equalProfilePage page right
    NotFoundPage page -> equalNotFoundPage page right

equalHomePage :: HomePageModel -> AppPageModel -> Bool
equalHomePage left right =
  case right of
    HomePage page -> left == page
    _ -> False

equalSecondPage :: SecondPageModel -> AppPageModel -> Bool
equalSecondPage left right =
  case right of
    SecondPage page -> left == page
    _ -> False

equalSpacesPage :: SpacesPageModel -> AppPageModel -> Bool
equalSpacesPage left right =
  case right of
    SpacesPage page -> left == page
    _ -> False

equalRegistrationPage :: AccountActionTarget -> RegistrationForm -> AppPageModel -> Bool
equalRegistrationPage leftPath leftForm right =
  case right of
    RegistrationPage path form -> (leftPath, leftForm) == (path, form)
    _ -> False

equalEmailVerificationPage :: AccountActionTarget -> VerificationForm -> AppPageModel -> Bool
equalEmailVerificationPage leftPath leftForm right =
  case right of
    EmailVerificationPage path form -> (leftPath, leftForm) == (path, form)
    _ -> False

equalMfaEnrollmentPage :: AccountActionTarget -> MfaEnrollmentForm -> AppPageModel -> Bool
equalMfaEnrollmentPage leftPath leftForm right =
  case right of
    MfaEnrollmentPage path form -> (leftPath, leftForm) == (path, form)
    _ -> False

equalLoginPage :: AccountActionTarget -> LoginForm -> AppPageModel -> Bool
equalLoginPage leftPath leftForm right =
  case right of
    LoginPage path form -> (leftPath, leftForm) == (path, form)
    _ -> False

equalLogoutPage :: AccountActionTarget -> AppPageModel -> Bool
equalLogoutPage leftPath right =
  case right of
    LogoutPage path -> leftPath == path
    _ -> False

equalProfilePage :: ProfilePageModel -> AppPageModel -> Bool
equalProfilePage left right =
  case right of
    ProfilePage page -> left == page
    _ -> False

equalNotFoundPage :: NotFoundPageModel -> AppPageModel -> Bool
equalNotFoundPage left right =
  case right of
    NotFoundPage page -> left == page
    _ -> False

instance Show AppPageModel where
  showsPrec precedence (HomePage homePage) =
    showParen (precedence > 10) (showString "HomePage " . showsPrec 11 homePage)
  showsPrec precedence (SecondPage secondPage) =
    showParen (precedence > 10) (showString "SecondPage " . showsPrec 11 secondPage)
  showsPrec precedence (SpacesPage spacesPage) =
    showParen (precedence > 10) (showString "SpacesPage " . showsPrec 11 spacesPage)
  showsPrec precedence (RegistrationPage registrationPath RegistrationForm {registrationFormUsername, registrationFormEmail, registrationFormDisplayName, registrationFormMessage, registrationFormIsError}) =
    showParen
      (precedence > 10)
      ( showString "RegistrationPage "
          . shows registrationPath
          . showChar ' '
          . showString "(RegistrationForm {registrationFormUsername = "
          . shows registrationFormUsername
          . showString ", registrationFormEmail = "
          . shows registrationFormEmail
          . showString ", registrationFormDisplayName = "
          . shows registrationFormDisplayName
          . showString ", registrationFormMessage = "
          . shows registrationFormMessage
          . showString ", registrationFormIsError = "
          . shows registrationFormIsError
          . showString "})"
      )
  showsPrec precedence (EmailVerificationPage verificationPath VerificationForm {verificationFormToken, verificationFormMessage, verificationFormIsError}) =
    showParen
      (precedence > 10)
      ( showString "EmailVerificationPage "
          . shows verificationPath
          . showChar ' '
          . showString "(VerificationForm {verificationFormToken = "
          . shows verificationFormToken
          . showString ", verificationFormMessage = "
          . shows verificationFormMessage
          . showString ", verificationFormIsError = "
          . shows verificationFormIsError
          . showString "})"
      )
  showsPrec precedence (MfaEnrollmentPage mfaEnrollmentPath MfaEnrollmentForm {mfaEnrollmentFormAccountId, mfaEnrollmentFormMessage, mfaEnrollmentFormIsError}) =
    showParen (precedence > 10) (showString "MfaEnrollmentPage " . shows mfaEnrollmentPath . showChar ' ' . shows mfaEnrollmentFormAccountId . showChar ' ' . shows mfaEnrollmentFormMessage . showChar ' ' . shows mfaEnrollmentFormIsError)
  showsPrec precedence (LoginPage loginPath LoginForm {loginFormEmail, loginFormMessage, loginFormIsError}) =
    showParen (precedence > 10) (showString "LoginPage " . shows loginPath . showChar ' ' . shows loginFormEmail . showChar ' ' . shows loginFormMessage . showChar ' ' . shows loginFormIsError)
  showsPrec precedence (LogoutPage logoutPath) =
    showParen (precedence > 10) (showString "LogoutPage " . shows logoutPath)
  showsPrec precedence (ProfilePage profilePage) =
    showProfilePage precedence profilePage
  showsPrec precedence (NotFoundPage notFoundPage) =
    showParen (precedence > 10) (showString "NotFoundPage " . showsPrec 11 notFoundPage)

showProfilePage :: Int -> ProfilePageModel -> ShowS
showProfilePage precedence profilePage =
  case profilePage of
    SignedOutProfilePage {profileHeading, profileSummary, profileSignInAction, profileRegistrationAction} ->
      showParen (precedence > 10) (showString "SignedOutProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileSignInAction . showChar ' ' . shows profileRegistrationAction)
    PendingProfilePage {profileHeading, profileSummary, profileEmail, profileUsername, profileDisplayName, profileResendPath, profileResendLabel, profileSignOutAction} ->
      showParen (precedence > 10) (showString "PendingProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileEmail . showChar ' ' . shows profileUsername . showChar ' ' . shows profileDisplayName . showChar ' ' . shows profileResendPath . showChar ' ' . shows profileResendLabel . showChar ' ' . shows profileSignOutAction)
    AuthenticatedProfilePage {profileHeading, profileSummary, profileEmail, profileUsername, profileDisplayName, profileSignOutAction} ->
      showParen (precedence > 10) (showString "AuthenticatedProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileEmail . showChar ' ' . shows profileUsername . showChar ' ' . shows profileDisplayName . showChar ' ' . shows profileSignOutAction)
    UnavailableProfilePage {profileHeading, profileSummary, profileSignInAction} ->
      showParen (precedence > 10) (showString "UnavailableProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileSignInAction)
