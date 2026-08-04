module App.Components.Controls
  ( ActionFormProps (..),
    PageLinkProps (..),
    actionForm,
    pageLink,
  )
where

import App.Routes
  ( TwoPageActionTarget,
    TwoPageNavigationTarget,
    twoPageActionPath,
    twoPageNavigationPath,
  )
import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Controls qualified as Controls

newtype PageLinkProps = PageLinkProps
  { to :: TwoPageNavigationTarget
  }

pageLink :: PageLinkProps -> [HarchWeb.Html] -> HarchWeb.Html
pageLink PageLinkProps {to} = Controls.pageLink twoPageNavigationPath to

data ActionFormProps = ActionFormProps
  { action :: TwoPageActionTarget,
    ariaLabel :: Text
  }

actionForm :: ActionFormProps -> [HarchWeb.Html] -> HarchWeb.Html
actionForm ActionFormProps {action, ariaLabel} =
  Controls.actionForm twoPageActionPath action Controls.ActionFormAttributes {Controls.actionFormAriaLabel = Just ariaLabel}
