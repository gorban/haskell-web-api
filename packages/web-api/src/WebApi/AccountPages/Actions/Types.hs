-- | Types shared by the account action interpreters.
--
-- Decision (AHI-5-WA-MH, 2026-09-29): request, response, and effect aliases
-- live below the public 'Common' module so focused collaborators can depend
-- on their contract without importing that re-export boundary. 'Common'
-- continues to re-export the same aliases for existing callers.
module WebApi.AccountPages.Actions.Types
  ( AccountActionRequest,
    AccountActionResponse,
    AccountActionWorkflow,
  )
where

import HarchWeb qualified
import WebApi.AccountPages.Actions.Contract (AccountAction)
import WebApi.AppEffect (AppM)
import WebApi.Route (AppRequestContext, AppRoute)

type AccountActionRequest = HarchWeb.ClientActionRequest AppRoute AccountAction AppRequestContext

type AccountActionResponse = HarchWeb.ClientActionResponse AppRoute AppRequestContext

-- | The account action effect exposes one public client-action response on
-- both its success and failure rails. Focused workflow modules use this
-- shared boundary instead of inventing per-action effect stacks.
type AccountActionWorkflow = AppM AccountActionResponse AccountActionResponse
