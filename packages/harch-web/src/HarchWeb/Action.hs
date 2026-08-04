{-# LANGUAGE OverloadedStrings #-}

-- | Declarative, bidirectional client-action endpoint codecs.
module HarchWeb.Action
  ( ActionCodec,
    ActionCodecError (..),
    ActionDecoder,
    ActionEndpoint,
    ActionMethod (..),
    ActionPath,
    ClientActionDecodeResult (..),
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
    singleOrDefault,
    textValue,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
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
  deriving (Eq, Ord, Show)

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
    clientActionPayloadContext :: context
  }
  deriving (Eq, Show)

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

newtype ActionDecoder action = ActionDecoder
  { runActionDecoder :: [(Text, Text)] -> ParseResult action
  }

newtype FormField value = FormField ([(Text, Text)] -> ParseResult value)

newtype FieldValue value = FieldValue
  { runFieldValue :: Text -> Maybe value
  }

data ParseResult value
  = Parsed value
  | ParseErrors (NonEmpty ClientActionParseError)

instance Functor ParseResult where
  fmap transform parsed =
    case parsed of
      Parsed value -> Parsed (transform value)
      ParseErrors errors -> ParseErrors errors

instance Applicative ParseResult where
  pure = Parsed
  functionResult <*> valueResult =
    case (functionResult, valueResult) of
      (Parsed transform, Parsed value) -> Parsed (transform value)
      (ParseErrors leftErrors, ParseErrors rightErrors) -> ParseErrors (leftErrors <> rightErrors)
      (ParseErrors errors, _) -> ParseErrors errors
      (_, ParseErrors errors) -> ParseErrors errors

instance Functor ActionDecoder where
  fmap transform (ActionDecoder decode) = ActionDecoder (fmap transform . decode)

instance Applicative ActionDecoder where
  pure value = ActionDecoder (const (pure value))
  ActionDecoder decodeFunction <*> ActionDecoder decodeValue =
    ActionDecoder $ \fields -> decodeFunction fields <*> decodeValue fields

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

emptyActionCodec :: ActionCodec target context action
emptyActionCodec = ActionCodec []

actionPath :: (Eq target) => ActionCodec target context action -> context -> target -> Text
actionPath (ActionCodec endpoints) context target =
  case [renderActionPath endpointActionPath context | ActionEndpoint endpointTargetValue endpointActionPath _ <- endpoints, endpointTargetValue == target] of
    path : _ -> path
    [] -> error "client action target is not declared by this codec"

actionMethod :: (Eq target) => ActionCodec target context action -> target -> ActionMethod
actionMethod (ActionCodec endpoints) target =
  case [actionPathMethod endpointActionPath | ActionEndpoint endpointTargetValue endpointActionPath _ <- endpoints, endpointTargetValue == target] of
    methodValue : _ -> methodValue
    [] -> error "client action target is not declared by this codec"

decodeAction :: ActionCodec target context action -> ClientActionPayload context -> ClientActionDecodeResult action
decodeAction (ActionCodec endpoints) payload =
  case filter (matchesActionPath payload) endpoints of
    [] -> UnrecognizedClientAction
    pathMatches ->
      case filter (matchesActionMethod payload) pathMatches of
        [] -> MethodNotAllowedClientAction (declaredMethods pathMatches)
        ActionEndpoint _ _ decoder : _ ->
          case runActionDecoder decoder (clientActionFields payload) of
            Parsed decodedAction -> DecodedClientAction decodedAction
            ParseErrors parseErrors -> MalformedClientAction parseErrors

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
get path = getAt path (const path)

getAt :: Text -> (context -> Text) -> ActionPath context
getAt = methodAt ActionGet

post :: Text -> ActionPath context
post path = postAt path (const path)

postAt :: Text -> (context -> Text) -> ActionPath context
postAt = methodAt ActionPost

put :: Text -> ActionPath context
put path = putAt path (const path)

putAt :: Text -> (context -> Text) -> ActionPath context
putAt = methodAt ActionPut

patch :: Text -> ActionPath context
patch path = patchAt path (const path)

patchAt :: Text -> (context -> Text) -> ActionPath context
patchAt = methodAt ActionPatch

delete :: Text -> ActionPath context
delete path = deleteAt path (const path)

deleteAt :: Text -> (context -> Text) -> ActionPath context
deleteAt = methodAt ActionDelete

formField :: Text -> FieldValue value -> FormField value
formField fieldName valueDecoder =
  FormField $ \fields ->
    case [fieldValue | (name, fieldValue) <- fields, name == fieldName] of
      [fieldValue] ->
        maybe
          (ParseErrors (InvalidActionField fieldName :| []))
          Parsed
          (runFieldValue valueDecoder fieldValue)
      [] -> ParseErrors (MissingActionField fieldName :| [])
      _ -> ParseErrors (DuplicateActionField fieldName :| [])

required :: FormField value -> ActionDecoder value
required (FormField decode) = ActionDecoder decode

exactlyOne :: FormField value -> ActionDecoder value
exactlyOne = required

optional :: FormField value -> ActionDecoder (Maybe value)
optional (FormField decode) =
  ActionDecoder $ \fields ->
    case decode fields of
      Parsed value -> Parsed (Just value)
      ParseErrors (MissingActionField _ :| []) -> Parsed Nothing
      ParseErrors errors -> ParseErrors errors

singleOrDefault :: value -> FormField value -> ActionDecoder value
singleOrDefault defaultValue (FormField decode) =
  ActionDecoder $ \fields ->
    case decode fields of
      Parsed value -> Parsed value
      ParseErrors (MissingActionField _ :| []) -> Parsed defaultValue
      ParseErrors errors -> ParseErrors errors

textValue :: FieldValue Text
textValue = FieldValue Just

parseField :: (Text -> Maybe value) -> FieldValue value
parseField = FieldValue

duplicateEndpoint :: [ActionEndpoint target context action] -> Maybe (ActionMethod, Text)
duplicateEndpoint endpoints =
  case [(actionPathMethod endpointActionPath, actionPathIdentity endpointActionPath) | ActionEndpoint _ endpointActionPath _ <- endpoints] of
    [] -> Nothing
    identity : identities -> if identity `elem` identities then Just identity else firstDuplicate identities
  where
    firstDuplicate remaining =
      case remaining of
        [] -> Nothing
        identity : identities ->
          if identity `elem` identities
            then Just identity
            else firstDuplicate identities

declaredMethods :: [ActionEndpoint target context action] -> NonEmpty ActionMethod
declaredMethods (ActionEndpoint _ endpointActionPath _ : remainingEndpoints) =
  actionPathMethod endpointActionPath :| uniqueMethods remainingEndpoints [actionPathMethod endpointActionPath]
declaredMethods [] = error "declaredMethods requires a matched endpoint"

uniqueMethods :: [ActionEndpoint target context action] -> [ActionMethod] -> [ActionMethod]
uniqueMethods endpoints seen =
  case endpoints of
    [] -> []
    ActionEndpoint _ endpointActionPath _ : remainingEndpoints ->
      let methodValue = actionPathMethod endpointActionPath
       in if methodValue `elem` seen
            then uniqueMethods remainingEndpoints seen
            else methodValue : uniqueMethods remainingEndpoints (seen <> [methodValue])
