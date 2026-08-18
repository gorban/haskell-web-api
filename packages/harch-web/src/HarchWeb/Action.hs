{-# LANGUAGE OverloadedStrings #-}

-- | Declarative, bidirectional client-action endpoint codecs.
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
module HarchWeb.Action
  ( ActionCodec,
    ActionCodecError (..),
    ActionDecoder,
    ActionEndpoint,
    ActionMethod (..),
    ActionPath,
    ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionParseError (..),
    ClientActionPayload (..),
    FieldValue,
    FormField,
    action,
    actionCodec,
    actionMethod,
    actionMethodText,
    actionPath,
    decodeAction,
    delete,
    deleteAt,
    emptyActionCodec,
    exactlyOne,
    formField,
    get,
    getAt,
    methodAt,
    optional,
    parseField,
    patch,
    patchAt,
    post,
    postAt,
    put,
    putAt,
    required,
    singleActionCodec,
    singleOrDefault,
    textValue,
  )
where

import Data.Functor.Compose (Compose (..), getCompose)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

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

data ClientActionParseError
  = MissingActionField Text
  | DuplicateActionField Text
  | InvalidActionField Text
  deriving (Eq, Show)

data ClientActionDecodeResult action
  = DecodedClientAction action
  | UnrecognizedClientAction
  | MethodNotAllowedClientAction (NonEmpty ActionMethod)
  | MalformedClientAction (NonEmpty ClientActionParseError)
  | InvalidClientActionDecoder
  deriving (Eq, Show)

data ActionCodecError
  = DuplicateActionEndpoint ActionMethod Text
  deriving (Eq, Show)

data ActionPath context = ActionPath
  { actionPathMethod :: ActionMethod,
    actionPathIdentity :: Text,
    renderActionPath :: context -> Text
  }

data ActionEndpoint target context action = ActionEndpoint target (ActionPath context) (ActionDecoder action)

newtype ActionCodec target context action = ActionCodec [ActionEndpoint target context action]

-- | An applicative action decoder. Its public result is normalized by
-- 'decodeAction': malformed fields carry their non-empty stable errors, and
-- a third-party decoder that violates the internal @([], Nothing)@ convention
-- becomes 'InvalidClientActionDecoder' rather than an exception.
type ActionDecoder action = Compose ((->) [(Text, Text)]) (Compose ((,) [ClientActionParseError]) Maybe) action

newtype FormField value = FormField ([(Text, Text)] -> ([ClientActionParseError], Maybe value))

newtype FieldValue value = FieldValue
  { runFieldValue :: Text -> Maybe value
  }

runActionDecoder :: ActionDecoder action -> [(Text, Text)] -> ([ClientActionParseError], Maybe action)
runActionDecoder decoder fields = getCompose (getCompose decoder fields)

actionDecoder :: ([(Text, Text)] -> ([ClientActionParseError], Maybe action)) -> ActionDecoder action
actionDecoder decode = Compose (Compose . decode)

action :: target -> ActionPath context -> ActionDecoder actionValue -> ActionEndpoint target context actionValue
action = ActionEndpoint

-- | Build a codec after proving that no two declarations claim the same
-- method and stable path identity. Dynamic paths must use a distinct identity
-- through 'postAt', so duplicate declarations remain a construction error.
actionCodec :: [ActionEndpoint target context action] -> Either ActionCodecError (ActionCodec target context action)
actionCodec endpoints =
  case duplicateEndpoint endpoints of
    Nothing -> Right (ActionCodec endpoints)
    Just (methodValue, pathIdentity) -> Left (DuplicateActionEndpoint methodValue pathIdentity)

-- | A one-endpoint codec is intrinsically free of duplicate endpoint declarations.
singleActionCodec :: target -> ActionPath context -> ActionDecoder action -> ActionCodec target context action
singleActionCodec target path decoder = ActionCodec [action target path decoder]

emptyActionCodec :: ActionCodec target context action
emptyActionCodec = ActionCodec []

-- | Look up the rendered path of a declared target. An absent target is an
-- ordinary construction/configuration result, never a server exception.
actionPath :: (Eq target) => ActionCodec target context action -> context -> target -> Maybe Text
actionPath (ActionCodec endpoints) context target =
  renderActionPath <$> actionTargetPath endpoints target <*> pure context

-- | Look up the method of a declared target. Pair this with 'actionPath' or
-- use 'HarchWeb.Controls.actionForm', which makes an undeclared target an
-- explicit rendering result.
actionMethod :: (Eq target) => ActionCodec target context action -> target -> Maybe ActionMethod
actionMethod (ActionCodec endpoints) target =
  actionPathMethod <$> actionTargetPath endpoints target

decodeAction :: ActionCodec target context action -> ClientActionPayload context -> ClientActionDecodeResult action
decodeAction (ActionCodec endpoints) payload =
  case filter (matchesActionPath payload) endpoints of
    [] -> UnrecognizedClientAction
    firstPathMatch : remainingPathMatches ->
      case filter (matchesActionMethod payload) (firstPathMatch : remainingPathMatches) of
        [] -> MethodNotAllowedClientAction (declaredMethods (firstPathMatch :| remainingPathMatches))
        ActionEndpoint _ _ decoder : _ ->
          case runActionDecoder decoder (clientActionFields payload) of
            (parseErrors, decodedAction) ->
              case decodedAction of
                Nothing ->
                  maybe InvalidClientActionDecoder MalformedClientAction (nonEmpty parseErrors)
                Just actionValue ->
                  maybe (DecodedClientAction actionValue) MalformedClientAction (nonEmpty parseErrors)

matchesActionPath :: ClientActionPayload context -> ActionEndpoint target context action -> Bool
matchesActionPath payload (ActionEndpoint _ endpointActionPath _) =
  renderActionPath endpointActionPath (clientActionPayloadContext payload) == clientActionPath payload

matchesActionMethod :: ClientActionPayload context -> ActionEndpoint target context action -> Bool
matchesActionMethod payload (ActionEndpoint _ endpointActionPath _) =
  actionMethodText (actionPathMethod endpointActionPath) == clientActionMethod payload

methodAt :: ActionMethod -> Text -> (context -> Text) -> ActionPath context
methodAt methodValue identity render =
  ActionPath
    { actionPathMethod = methodValue,
      actionPathIdentity = identity,
      renderActionPath = render
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
staticPath methodValue path = methodAt methodValue path (const path)

formField :: Text -> FieldValue value -> FormField value
formField fieldName valueDecoder =
  FormField $ \fields ->
    case [fieldValue | (name, fieldValue) <- fields, name == fieldName] of
      [fieldValue] ->
        maybe
          ([InvalidActionField fieldName], noFieldValue)
          (\value -> ([], Just value))
          (runFieldValue valueDecoder fieldValue)
      [] -> ([MissingActionField fieldName], noFieldValue)
      _ -> ([DuplicateActionField fieldName], noFieldValue)

required :: FormField value -> ActionDecoder value
required (FormField decode) = actionDecoder decode

exactlyOne :: FormField value -> ActionDecoder value
exactlyOne = required

optional :: FormField value -> ActionDecoder (Maybe value)
optional (FormField decode) =
  actionDecoder $ \fields ->
    case decode fields of
      ([], Just value) -> ([], Just (Just value))
      ([MissingActionField _], Nothing) -> ([], Just noFieldValue)
      (parseErrors, _) -> (parseErrors, noFieldValue)

singleOrDefault :: value -> FormField value -> ActionDecoder value
singleOrDefault defaultValue (FormField decode) =
  actionDecoder $ \fields ->
    case decode fields of
      ([], Just value) -> ([], Just value)
      ([MissingActionField _], Nothing) -> ([], Just defaultValue)
      parseErrors -> parseErrors

-- | The absent parsed value shared by failed field decoders and by a valid
-- optional field that was not supplied. Sharing it documents that both cases
-- use the same value-level absence while their error lists distinguish them.
noFieldValue :: Maybe value
noFieldValue = Nothing

actionTargetPath :: (Eq target) => [ActionEndpoint target context action] -> target -> Maybe (ActionPath context)
actionTargetPath endpoints target =
  listToMaybe
    [ endpointActionPath
    | ActionEndpoint endpointTargetValue endpointActionPath _ <- endpoints,
      endpointTargetValue == target
    ]

textValue :: FieldValue Text
textValue = FieldValue Just

parseField :: (Text -> Maybe value) -> FieldValue value
parseField = FieldValue

duplicateEndpoint :: [ActionEndpoint target context action] -> Maybe (ActionMethod, Text)
duplicateEndpoint endpoints =
  listToMaybe
    [ identity
    | (index, identity) <- zip [0 ..] identities,
      identity `elem` drop (index + 1) identities
    ]
  where
    identities = [(actionPathMethod endpointActionPath, actionPathIdentity endpointActionPath) | ActionEndpoint _ endpointActionPath _ <- endpoints]

declaredMethods :: NonEmpty (ActionEndpoint target context action) -> NonEmpty ActionMethod
declaredMethods (ActionEndpoint _ endpointActionPath _ :| remainingEndpoints) =
  firstMethod :| nub (filter (/= firstMethod) (map endpointMethod remainingEndpoints))
  where
    firstMethod = actionPathMethod endpointActionPath
    endpointMethod (ActionEndpoint _ actionPathValue _) = actionPathMethod actionPathValue
