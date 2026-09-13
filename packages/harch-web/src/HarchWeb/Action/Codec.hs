{-# LANGUAGE OverloadedStrings #-}

-- | Validated declarative client-action endpoint codecs.
--
-- Decision record (2026-08-18): extend 'ActionCodec', the existing owner of
-- action declaration, rendering lookup, and dispatch; do not add a parallel
-- control registry. A target lookup is now total ('Maybe'), and
-- 'HarchWeb.Controls.actionForm' turns an absent target into an explicit
-- rendering result rather than an exception or an invisible empty fragment.
-- A third-party decoder can still manufacture the former @([], Nothing)@
-- convention, so 'decodeAction' evaluates and normalizes the decoder result
-- to the stable 'InvalidClientActionDecoder' value rather than throwing. This keeps an
-- undeclared control from claiming capture readiness while preserving its
-- authored content for an accessible configuration diagnostic.
--
-- Decision record (FQ1, 2026-08-29): a static action path is now represented
-- explicitly in 'ActionPath', rather than as a dynamic renderer applied to
-- @()@. 'staticActionPath' can therefore render only an action declaration
-- that proves it is context-free. Dynamic paths retain 'actionPath' and their
-- explicit context; a static renderer never invents one.
--
-- Decision record (AHI-4B, 2026-09-02): retain authored action declarations
-- only until codec construction validates default metadata.  The opaque
-- 'ActionCodec' thereafter stores a private validated endpoint form, so
-- module mounting and context prefixing cannot observe or recreate an
-- unresolved metadata state.  This strengthens the existing construction
-- failure rail without adding a second codec abstraction.
--
-- Decision record (PR-F5, 2026-09-05): endpoint declaration/validation stays
-- with this private codec owner; field parsing and mount adaptation have
-- distinct owners behind the stable 'HarchWeb.Action' facade.  A mount calls
-- the private transformation here rather than rebuilding a codec, preserving
-- the one validated endpoint representation and one action dispatcher.
module HarchWeb.Action.Codec
  ( ActionCodec (..),
    ActionCodecError (..),
    ActionEndpoint,
    ActionCompletionPolicy (..),
    ActionMethod (..),
    ActionPath (..),
    ActionReauthenticationPolicy (..),
    ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionPayload (..),
    action,
    actionEndpointMetadata,
    actionCompletionPolicy,
    actionEndpointTarget,
    actionReauthenticationPolicy,
    actionCodec,
    combineActionCodecs,
    declaredActionEndpointMetadata,
    actionMethod,
    actionMethodText,
    actionPath,
    actionWithMetadata,
    mapActionCodec,
    prefixActionCodecByContext,
    decodeAction,
    delete,
    deleteAt,
    emptyActionCodec,
    get,
    getAt,
    methodAt,
    patch,
    patchAt,
    post,
    postAt,
    publicAction,
    put,
    putAt,
    singleActionCodec,
    singleActionCodecWithMetadata,
    staticActionEndpointMetadata,
    staticActionPath,
    ValidatedActionEndpoint (..),
  )
where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Action.Field
  ( ActionDecoder,
    ClientActionParseError (..),
    runActionDecoder,
  )
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated, RequireAuthenticated, RequireAuthorized),
    EndpointMetadata,
    EndpointMetadataError,
    EndpointName,
    RouteTemplate,
    endpointAccess,
    mkEndpointMetadata,
    mkEndpointName,
    mkRouteTemplate,
  )
import HarchWeb.EndpointMetadata qualified as EndpointMetadata

-- | Methods supported by the client-action protocol. Forms can author POST
-- endpoints directly; the other methods remain useful to non-form clients and
-- make method negotiation explicit at the shared codec boundary.
data ActionMethod
  = ActionGet
  | ActionPost
  | ActionPut
  | ActionPatch
  | ActionDelete
  deriving (Eq, Show)

actionMethodText :: ActionMethod -> Text
actionMethodText methodValue =
  case methodValue of
    ActionGet -> "GET"
    ActionPost -> "POST"
    ActionPut -> "PUT"
    ActionPatch -> "PATCH"
    ActionDelete -> "DELETE"

data ClientActionPayload context = ClientActionPayload
  { clientActionMethod :: Text,
    clientActionPath :: Text,
    clientActionFields :: [(Text, Text)],
    clientActionCsrfToken :: Maybe Text,
    clientActionIdempotencyKey :: Maybe ClientActionIdempotencyKey,
    clientActionPayloadContext :: context
  }
  deriving (Eq, Show)

-- | A client-generated identity carried unchanged across explicitly
-- idempotent retries. The action handler uses it at its server-side
-- deduplication boundary; it is never logged by the framework.
type ClientActionIdempotencyKey = Text

data ClientActionDecodeResult action
  = DecodedClientAction action
  | UnrecognizedClientAction
  | MethodNotAllowedClientAction (NonEmpty ActionMethod)
  | MalformedClientAction (NonEmpty ClientActionParseError)
  | InvalidClientActionDecoder
  deriving (Eq, Show)

data ActionCodecError
  = DuplicateActionEndpoint ActionMethod Text
  | InvalidActionEndpointMetadata EndpointMetadataError
  deriving (Eq, Show)

data ActionPath context = ActionPath
  { actionPathMethod :: ActionMethod,
    actionPathIdentity :: Text,
    renderActionPath :: context -> Text,
    actionStaticPath :: Maybe Text
  }

data DeclaredActionMetadata authorization
  = DefaultProtectedActionMetadata
  | ExplicitActionMetadata (EndpointMetadata authorization)

-- | The only action response whose typed navigation an application-owned
-- reauthentication continuation may deliberately consume.  The normal case
-- remains entirely framework-owned: patches settle and a typed navigation is
-- applied immediately.  This declaration is intentionally narrower than a
-- general JavaScript callback registry.
data ActionCompletionPolicy
  = ApplyActionResponse
  | ReauthenticationContinuation
  deriving (Eq, Show)

-- | Whether the capture kernel may retain this declared action after Harch's
-- recognized pre-handler authentication challenge.  Choosing this at the
-- endpoint declaration, rather than a permissive form default, makes an
-- action author's replay decision explicit before its bounded input snapshot
-- can be kept in tab memory.
data ActionReauthenticationPolicy
  = DoNotRetain
  | RetainForExplicitRetry
  deriving (Eq, Show)

data ActionEndpoint target context authorization action = ActionEndpoint target ActionReauthenticationPolicy ActionCompletionPolicy (ActionPath context) (DeclaredActionMetadata authorization) (ActionDecoder action)

-- | A codec only stores declarations after their default metadata has been
-- resolved and their metadata validated.  Keeping the authored declaration
-- separate from this runtime representation makes the "unresolved default"
-- state unrepresentable to transformations and dispatch.
data ValidatedActionEndpoint target context authorization action = ValidatedActionEndpoint target ActionReauthenticationPolicy ActionCompletionPolicy (ActionPath context) (EndpointMetadata authorization) (ActionDecoder action)

newtype ActionCodec target context authorization action = ActionCodec [ValidatedActionEndpoint target context authorization action]

action :: target -> ActionReauthenticationPolicy -> ActionCompletionPolicy -> ActionPath context -> ActionDecoder actionValue -> ActionEndpoint target context authorization actionValue
action target reauthenticationPolicy completionPolicy path = ActionEndpoint target reauthenticationPolicy completionPolicy path DefaultProtectedActionMetadata

-- | Name an action's endpoint explicitly, including any anonymous or
-- application-specific authorization requirement.
actionWithMetadata :: target -> ActionReauthenticationPolicy -> ActionCompletionPolicy -> ActionPath context -> EndpointMetadata authorization -> ActionDecoder actionValue -> ActionEndpoint target context authorization actionValue
actionWithMetadata target reauthenticationPolicy completionPolicy path metadata = ActionEndpoint target reauthenticationPolicy completionPolicy path (ExplicitActionMetadata metadata)

-- | Declare an explicitly anonymous action. Unlike 'actionWithMetadata', this
-- constructor cannot accidentally carry a protected requirement under a
-- misleading name. Applications use 'actionWithMetadata' when a domain
-- authorization requirement is part of the declaration.
publicAction :: target -> ActionReauthenticationPolicy -> ActionCompletionPolicy -> ActionPath context -> EndpointName -> RouteTemplate -> ActionDecoder actionValue -> ActionEndpoint target context authorization actionValue
publicAction target reauthenticationPolicy completionPolicy path endpointName routeTemplate =
  actionWithMetadata
    target
    reauthenticationPolicy
    completionPolicy
    path
    (mkEndpointMetadata endpointName routeTemplate EndpointMetadata.ActionEndpoint AllowUnauthenticated)

-- | Build a codec after proving that no two declarations claim the same
-- method and stable path identity. Dynamic paths must use a distinct identity
-- through 'postAt', so duplicate declarations remain a construction error.
actionCodec :: [ActionEndpoint target context authorization action] -> Either ActionCodecError (ActionCodec target context authorization action)
actionCodec endpoints =
  case duplicateEndpoint endpoints of
    Nothing -> ActionCodec <$> traverse validateActionMetadata endpoints
    Just (methodValue, pathIdentity) -> Left (DuplicateActionEndpoint methodValue pathIdentity)

-- | Combine already validated action codecs under the one client-action
-- decoder.  The same construction check rejects cross-module method/identity
-- collisions before the server starts; it does not introduce dispatcher
-- precedence for ambiguous actions.
combineActionCodecs :: NonEmpty (ActionCodec target context authorization action) -> Either ActionCodecError (ActionCodec target context authorization action)
combineActionCodecs codecs =
  case duplicateValidatedEndpoint endpoints of
    Nothing -> Right (ActionCodec endpoints)
    Just (methodValue, pathIdentity) -> Left (DuplicateActionEndpoint methodValue pathIdentity)
  where
    endpoints = concatMap codecEndpoints (toList codecs)
    codecEndpoints (ActionCodec codecDeclarations) = codecDeclarations
    toList (firstCodec :| remainingCodecs) = firstCodec : remainingCodecs

-- | The declared endpoint metadata used by application-module construction
-- to reject duplicate endpoint names across routes and actions.  Every codec
-- constructor normalizes defaults before exposing a codec, so this list
-- contains only validated declarations.
declaredActionEndpointMetadata :: ActionCodec target context authorization action -> [EndpointMetadata authorization]
declaredActionEndpointMetadata (ActionCodec endpoints) =
  [metadata | ValidatedActionEndpoint _ _ _ _ metadata _ <- endpoints]

-- | A one-endpoint codec is intrinsically free of duplicate endpoint declarations,
-- but its default metadata is still derived from the supplied path. Keep that
-- construction failure on the ordinary configuration rail rather than raising
-- an exception for malformed paths.
singleActionCodec :: target -> ActionReauthenticationPolicy -> ActionCompletionPolicy -> ActionPath context -> ActionDecoder action -> Either ActionCodecError (ActionCodec target context authorization action)
singleActionCodec target reauthenticationPolicy completionPolicy path decoder =
  actionCodec [action target reauthenticationPolicy completionPolicy path decoder]

-- | Build one explicit endpoint declaration without an avoidable configuration
-- failure rail. A supplied 'EndpointMetadata' is already validated, and one
-- endpoint cannot duplicate another declaration.
singleActionCodecWithMetadata :: target -> ActionReauthenticationPolicy -> ActionCompletionPolicy -> ActionPath context -> EndpointMetadata authorization -> ActionDecoder action -> ActionCodec target context authorization action
singleActionCodecWithMetadata target reauthenticationPolicy completionPolicy path metadata decoder =
  ActionCodec [ValidatedActionEndpoint target reauthenticationPolicy completionPolicy path metadata decoder]

emptyActionCodec :: ActionCodec target context authorization action
emptyActionCodec = ActionCodec []

-- | Look up an action's validated declaration before its body is read. The
-- request executor uses this to run endpoint admission ahead of action field
-- decoding and handler invocation.
actionEndpointMetadata :: ActionCodec target context authorization action -> context -> Text -> Text -> Maybe (EndpointMetadata authorization)
actionEndpointMetadata (ActionCodec endpoints) requestContext methodValue pathValue =
  listToMaybe
    [ metadata
    | ValidatedActionEndpoint _ _ _ endpointActionPath metadata _ <- endpoints,
      renderActionPath endpointActionPath requestContext == pathValue,
      actionMethodText (actionPathMethod endpointActionPath) == methodValue
    ]

-- | Resolve the authored target of the same validated endpoint selected by
-- 'actionEndpointMetadata'. The module composition boundary uses this before
-- decoding to recover the action's typed owning route for guards and route
-- observation; it never derives ownership from an untrusted path.
actionEndpointTarget :: ActionCodec target context authorization action -> context -> Text -> Text -> Maybe target
actionEndpointTarget (ActionCodec endpoints) requestContext methodValue pathValue =
  listToMaybe
    [ target
    | ValidatedActionEndpoint target _ _ endpointActionPath _ _ <- endpoints,
      renderActionPath endpointActionPath requestContext == pathValue,
      actionMethodText (actionPathMethod endpointActionPath) == methodValue
    ]

-- | Look up a static action endpoint without inventing a request context.
-- Dynamic action paths deliberately do not match: their context-dependent
-- declaration must be selected through 'actionEndpointMetadata'.
staticActionEndpointMetadata :: ActionCodec target context authorization action -> Text -> Text -> Maybe (EndpointMetadata authorization)
staticActionEndpointMetadata (ActionCodec endpoints) methodValue pathValue =
  listToMaybe
    [ metadata
    | ValidatedActionEndpoint _ _ _ endpointActionPath metadata _ <- endpoints,
      actionStaticPath endpointActionPath == Just pathValue,
      actionMethodText (actionPathMethod endpointActionPath) == methodValue
    ]

-- | Lift a child action declaration into a parent algebra without giving the
-- child access to the full parent request context or authorization type.  The
-- endpoint's method, declared identity, decoder, and static-path proof are
-- retained; only the values that cross the module boundary are mapped.
--
-- This is deliberately a map of one already validated codec, not a merger of
-- two codecs.  A root that combines declarations remains responsible for
-- rejecting duplicate action identities before it starts serving requests.
mapActionCodec ::
  (childTarget -> parentTarget) ->
  (parentContext -> childContext) ->
  (childAuthorization -> parentAuthorization) ->
  (childAction -> parentAction) ->
  ActionCodec childTarget childContext childAuthorization childAction ->
  ActionCodec parentTarget parentContext parentAuthorization parentAction
mapActionCodec embedTarget projectContext projectAuthorization embedAction (ActionCodec endpoints) =
  ActionCodec (map mapEndpoint endpoints)
  where
    mapEndpoint (ValidatedActionEndpoint target reauthenticationPolicy completionPolicy path metadata decoder) =
      ValidatedActionEndpoint
        (embedTarget target)
        reauthenticationPolicy
        completionPolicy
        (mapActionPath path)
        (mapMetadata metadata)
        (fmap embedAction decoder)

    mapActionPath path =
      ActionPath
        { actionPathMethod = actionPathMethod path,
          actionPathIdentity = actionPathIdentity path,
          renderActionPath = renderActionPath path . projectContext,
          actionStaticPath = actionStaticPath path
        }

    mapMetadata endpointMetadata =
      endpointMetadata
        { endpointAccess = mapAccessRequirement (endpointAccess endpointMetadata)
        }

    mapAccessRequirement requirement =
      case requirement of
        AllowUnauthenticated -> AllowUnauthenticated
        RequireAuthenticated -> RequireAuthenticated
        RequireAuthorized authorization -> RequireAuthorized (projectAuthorization authorization)

-- | Prefix every action path with a value selected from its already trusted
-- request context.  This is for a root-owned dynamic namespace such as an
-- allowlisted locale; child modules remain unaware of it.  Since the rendered
-- prefix varies by context, even an originally static child action has no
-- static root path after this transformation.  The declared template is
-- supplied separately so endpoint metadata remains low-cardinality.
prefixActionCodecByContext ::
  (context -> Text) ->
  Text ->
  ActionCodec target context authorization action ->
  Either ActionCodecError (ActionCodec target context authorization action)
prefixActionCodecByContext renderPrefix templatePrefix (ActionCodec endpoints) =
  ActionCodec <$> traverse prefixEndpoint endpoints
  where
    prefixEndpoint (ValidatedActionEndpoint target reauthenticationPolicy completionPolicy childPath childMetadata decoder) = do
      let prefixedPath =
            ActionPath
              { actionPathMethod = actionPathMethod childPath,
                actionPathIdentity = appendPrefix templatePrefix (actionPathIdentity childPath),
                renderActionPath = \requestContext -> appendPrefix (renderPrefix requestContext) (renderActionPath childPath requestContext),
                actionStaticPath = Nothing
              }
      prefixedMetadata <- prefixMetadata childMetadata
      pure (ValidatedActionEndpoint target reauthenticationPolicy completionPolicy prefixedPath prefixedMetadata decoder)

    prefixMetadata metadata = do
      prefixedTemplate <- firstMetadataError (mkRouteTemplate (appendPrefix templatePrefix (EndpointMetadata.routeTemplateText (EndpointMetadata.endpointRouteTemplate metadata))))
      pure (metadata {EndpointMetadata.endpointRouteTemplate = prefixedTemplate})

    firstMetadataError = either (Left . InvalidActionEndpointMetadata) Right

appendPrefix :: Text -> Text -> Text
appendPrefix prefix path
  | path == "/" = prefix
  | otherwise = prefix <> "/" <> Text.dropWhile (== '/') path

validateActionMetadata :: ActionEndpoint target context authorization action -> Either ActionCodecError (ValidatedActionEndpoint target context authorization action)
validateActionMetadata endpoint =
  case endpoint of
    ActionEndpoint target reauthenticationPolicy completionPolicy path DefaultProtectedActionMetadata decoder -> do
      metadata <- defaultActionMetadata path
      pure (ValidatedActionEndpoint target reauthenticationPolicy completionPolicy path metadata decoder)
    ActionEndpoint target reauthenticationPolicy completionPolicy path (ExplicitActionMetadata metadata) decoder ->
      Right (ValidatedActionEndpoint target reauthenticationPolicy completionPolicy path metadata decoder)

defaultActionMetadata :: ActionPath context -> Either ActionCodecError (EndpointMetadata authorization)
defaultActionMetadata path = do
  endpointName <- firstMetadataError (mkEndpointName ("action." <> Text.toLower (actionMethodText (actionPathMethod path)) <> "." <> normalizedIdentity))
  routeTemplate <- firstMetadataError (mkRouteTemplate (actionPathIdentity path))
  pure (mkEndpointMetadata endpointName routeTemplate EndpointMetadata.ActionEndpoint RequireAuthenticated)
  where
    normalizedIdentity =
      let normalized = Text.map normalizeCharacter (Text.dropWhile (== '/') (actionPathIdentity path))
       in if Text.null normalized then "root" else normalized
    normalizeCharacter character
      | Text.any (== character) "abcdefghijklmnopqrstuvwxyz0123456789" = character
      | otherwise = '-'
    firstMetadataError = either (Left . InvalidActionEndpointMetadata) Right

-- | Look up the rendered path of a declared target. An absent target is an
-- ordinary construction/configuration result, never a server exception.
actionPath :: (Eq target) => ActionCodec target context authorization action -> context -> target -> Maybe Text
actionPath (ActionCodec endpoints) context target =
  renderActionPath <$> actionTargetPath endpoints target <*> pure context

-- | Look up a declaration that proves its path is independent of request
-- context. A dynamic declaration returns 'Nothing' rather than receiving an
-- invented context value.
staticActionPath :: (Eq target) => ActionCodec target context authorization action -> target -> Maybe Text
staticActionPath (ActionCodec endpoints) target =
  actionStaticPath =<< actionTargetPath endpoints target

-- | Look up the method of a declared target. Pair this with 'actionPath' or
-- use 'HarchWeb.Controls.actionForm', which makes an undeclared target an
-- explicit rendering result.
actionMethod :: (Eq target) => ActionCodec target context authorization action -> target -> Maybe ActionMethod
actionMethod (ActionCodec endpoints) target =
  actionPathMethod <$> actionTargetPath endpoints target

-- | Recover the explicit reauthentication retention decision for a declared
-- target.  Rendering uses this same validated declaration; an absent target
-- remains an ordinary configuration result.
actionReauthenticationPolicy :: (Eq target) => ActionCodec target context authorization action -> target -> Maybe ActionReauthenticationPolicy
actionReauthenticationPolicy (ActionCodec endpoints) target =
  listToMaybe
    [ endpointPolicy
    | ValidatedActionEndpoint endpointTarget endpointPolicy _ _ _ _ <- endpoints,
      endpointTarget == target
    ]

-- | Recover the explicit completion decision for a declared target.  Only
-- 'ReauthenticationContinuation' permits the client runtime to offer its
-- typed navigation to application recovery code.
actionCompletionPolicy :: (Eq target) => ActionCodec target context authorization action -> target -> Maybe ActionCompletionPolicy
actionCompletionPolicy (ActionCodec endpoints) target =
  listToMaybe
    [ endpointPolicy
    | ValidatedActionEndpoint endpointTarget _ endpointPolicy _ _ _ <- endpoints,
      endpointTarget == target
    ]

decodeAction :: ActionCodec target context authorization action -> ClientActionPayload context -> ClientActionDecodeResult action
decodeAction (ActionCodec endpoints) payload =
  case filter (matchesActionPath payload) endpoints of
    [] -> UnrecognizedClientAction
    firstPathMatch : remainingPathMatches ->
      case filter (matchesActionMethod payload) (firstPathMatch : remainingPathMatches) of
        [] -> MethodNotAllowedClientAction (declaredMethods (firstPathMatch :| remainingPathMatches))
        ValidatedActionEndpoint _ _ _ _ _ decoder : _ ->
          case runActionDecoder decoder (clientActionFields payload) of
            (parseErrors, decodedAction) ->
              case decodedAction of
                Nothing ->
                  maybe InvalidClientActionDecoder MalformedClientAction (nonEmpty parseErrors)
                Just actionValue ->
                  maybe (DecodedClientAction actionValue) MalformedClientAction (nonEmpty parseErrors)

matchesActionPath :: ClientActionPayload context -> ValidatedActionEndpoint target context authorization action -> Bool
matchesActionPath payload (ValidatedActionEndpoint _ _ _ endpointActionPath _ _) =
  renderActionPath endpointActionPath (clientActionPayloadContext payload) == clientActionPath payload

matchesActionMethod :: ClientActionPayload context -> ValidatedActionEndpoint target context authorization action -> Bool
matchesActionMethod payload (ValidatedActionEndpoint _ _ _ endpointActionPath _ _) =
  actionMethodText (actionPathMethod endpointActionPath) == clientActionMethod payload

methodAt :: ActionMethod -> Text -> (context -> Text) -> ActionPath context
methodAt methodValue identity render =
  ActionPath
    { actionPathMethod = methodValue,
      actionPathIdentity = identity,
      renderActionPath = render,
      actionStaticPath = Nothing
    }

get :: Text -> ActionPath context
get = staticPath ActionGet

getAt :: Text -> (context -> Text) -> ActionPath context
getAt = methodAt ActionGet

post :: Text -> ActionPath context
post = staticPath ActionPost

postAt :: Text -> (context -> Text) -> ActionPath context
postAt = methodAt ActionPost

put :: Text -> ActionPath context
put = staticPath ActionPut

putAt :: Text -> (context -> Text) -> ActionPath context
putAt = methodAt ActionPut

patch :: Text -> ActionPath context
patch = staticPath ActionPatch

patchAt :: Text -> (context -> Text) -> ActionPath context
patchAt = methodAt ActionPatch

delete :: Text -> ActionPath context
delete = staticPath ActionDelete

deleteAt :: Text -> (context -> Text) -> ActionPath context
deleteAt = methodAt ActionDelete

staticPath :: ActionMethod -> Text -> ActionPath context
staticPath methodValue path =
  ActionPath
    { actionPathMethod = methodValue,
      actionPathIdentity = path,
      renderActionPath = const path,
      actionStaticPath = Just path
    }

actionTargetPath :: (Eq target) => [ValidatedActionEndpoint target context authorization action] -> target -> Maybe (ActionPath context)
actionTargetPath endpoints target =
  listToMaybe
    [ endpointActionPath
    | ValidatedActionEndpoint endpointTargetValue _ _ endpointActionPath _ _ <- endpoints,
      endpointTargetValue == target
    ]

duplicateEndpoint :: [ActionEndpoint target context authorization action] -> Maybe (ActionMethod, Text)
duplicateEndpoint endpoints =
  listToMaybe
    [ identity
    | (index, identity) <- zip [0 ..] identities,
      identity `elem` drop (index + 1) identities
    ]
  where
    identities = [(actionPathMethod endpointActionPath, actionPathIdentity endpointActionPath) | ActionEndpoint _ _ _ endpointActionPath _ _ <- endpoints]

duplicateValidatedEndpoint :: [ValidatedActionEndpoint target context authorization action] -> Maybe (ActionMethod, Text)
duplicateValidatedEndpoint endpoints =
  listToMaybe
    [ identity
    | (index, identity) <- zip [0 ..] identities,
      identity `elem` drop (index + 1) identities
    ]
  where
    identities = [(actionPathMethod endpointActionPath, actionPathIdentity endpointActionPath) | ValidatedActionEndpoint _ _ _ endpointActionPath _ _ <- endpoints]

declaredMethods :: NonEmpty (ValidatedActionEndpoint target context authorization action) -> NonEmpty ActionMethod
declaredMethods (ValidatedActionEndpoint _ _ _ endpointActionPath _ _ :| remainingEndpoints) =
  firstMethod :| nub (filter (/= firstMethod) (map endpointMethod remainingEndpoints))
  where
    firstMethod = actionPathMethod endpointActionPath
    endpointMethod (ValidatedActionEndpoint _ _ _ actionPathValue _ _) = actionPathMethod actionPathValue
