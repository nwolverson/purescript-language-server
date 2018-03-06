module IdePurescript.PscErrors where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (decodeJson, class DecodeJson)
import Data.Argonaut.Core (JObject, toObject)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (singleton)
import Data.Either (either, Either(Left))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

data RebuildResult = RebuildResult (Array PscError) | RebuildError String

type PscResult =
  { warnings :: Array PscError
  , errors :: Array PscError
  }

newtype PscError = PscError
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  , errorLink :: String
  , suggestion :: Maybe PscSuggestion
  }

derive instance pscErrorNewtype :: Newtype PscError _

type PscSuggestion =
  { replacement :: String
  , replaceRange :: Maybe Position
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

instance decodeRebuildResult :: DecodeJson RebuildResult where
  decodeJson json = (RebuildResult <$> (decodeJson json <|> (singleton <$> decodeJson json)))
    <|> (RebuildError <$> decodeJson json)

instance decodeJsonPscError :: DecodeJson PscError where
  decodeJson json = decodeJson json >>= parsePscError

parsePscOutput :: String -> Either String PscResult
parsePscOutput = maybe (Left "not object") parsePscResult <<< toObject <=< jsonParser

parsePscResult :: JObject -> Either String PscResult
parsePscResult obj =
  { warnings: _
  , errors: _
  } <$> (obj .? "warnings" >>= traverse parsePscError)
    <*> (obj .? "errors" >>= traverse parsePscError)

parsePscError :: JObject -> Either String PscError
parsePscError obj = PscError <$> (
  { moduleName: _
  , errorCode: _
  , message: _
  , filename: _
  , position: _
  , errorLink: _
  , suggestion: _
  } <$> obj .? "moduleName"
    <*> obj .? "errorCode"
    <*> obj .? "message"
    <*> obj .? "filename"
    <*> (obj .? "position" >>= parsePosition)
    <*> obj .? "errorLink"
    <*> (obj .? "suggestion" >>= parseSuggestion))

parsePosition :: Maybe JObject -> Either String (Maybe Position)
parsePosition =
  maybe (pure Nothing) \obj -> map Just $
    { startLine: _
    , startColumn: _
    , endLine: _
    , endColumn: _
    } <$> obj .? "startLine"
      <*> obj .? "startColumn"
      <*> obj .? "endLine"
      <*> obj .? "endColumn"

parseSuggestion :: Maybe JObject -> Either String (Maybe PscSuggestion)
parseSuggestion =
  maybe (pure Nothing) \obj -> do
    replacement <- obj .? "replacement"
    replaceRange <- pure $ either (const Nothing) id (obj .? "replaceRange" >>= parsePosition)
    pure $ Just { replacement, replaceRange }
