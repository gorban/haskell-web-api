-- | Applicative client-action field decoding.
--
-- This private owner parses only submitted field values. Endpoint selection,
-- metadata, mount adaptation, and handler dispatch remain outside it.
module HarchWeb.Action.Field
  ( ActionDecoder,
    ClientActionParseError (..),
    FieldValue,
    FormField,
    exactlyOne,
    formField,
    optional,
    parseField,
    required,
    runActionDecoder,
    singleOrDefault,
    textValue,
  )
where

import Data.Functor.Compose (Compose (..), getCompose)
import Data.Text (Text)

data ClientActionParseError
  = MissingActionField Text
  | DuplicateActionField Text
  | InvalidActionField Text
  deriving (Eq, Show)

-- | An applicative action decoder. Its result is normalized by the codec
-- owner: malformed fields carry non-empty stable errors and an invalid
-- third-party convention becomes a typed decoder rejection.
type ActionDecoder action = Compose ((->) [(Text, Text)]) (Compose ((,) [ClientActionParseError]) Maybe) action

newtype FormField value = FormField ([(Text, Text)] -> ([ClientActionParseError], Maybe value))

newtype FieldValue value = FieldValue
  { runFieldValue :: Text -> Maybe value
  }

runActionDecoder :: ActionDecoder action -> [(Text, Text)] -> ([ClientActionParseError], Maybe action)
runActionDecoder decoder fields = getCompose (getCompose decoder fields)

actionDecoder :: ([(Text, Text)] -> ([ClientActionParseError], Maybe action)) -> ActionDecoder action
actionDecoder decode = Compose (Compose . decode)

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

noFieldValue :: Maybe value
noFieldValue = Nothing

textValue :: FieldValue Text
textValue = FieldValue Just

parseField :: (Text -> Maybe value) -> FieldValue value
parseField = FieldValue
