module IdePurescript.PscErrors where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import PscIde.Command (RebuildError)

newtype PscResult
  = PscResult
  { warnings :: Array RebuildError
  , errors :: Array RebuildError
  }

instance decodePscResult :: DecodeJson PscResult where
  decodeJson json = do
    o <- decodeJson json
    warnings <- o .: "warnings"
    errors <- o .: "errors"
    pure $ PscResult { warnings, errors }

parsePscOutput :: String -> Either String PscResult
parsePscOutput =
  (lmap show <<< decodeJson) <=< jsonParser
