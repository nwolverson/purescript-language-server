module LanguageServer.IdePurescript.SuggestionRank
  ( SuggestionRank
  , fromInt
  , toString
  ) where

import Prelude

import Data.Char as Char
import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))
import Data.Ordering (invert)
import Data.String as String

newtype SuggestionRank = SuggestionRank Int

derive instance eqSuggestionRank :: Eq SuggestionRank

instance ordSuggestionRank :: Ord SuggestionRank where
  compare (SuggestionRank a) (SuggestionRank b) = invert (compare a b)

instance boundedSuggestionRank :: Bounded SuggestionRank where
  top = SuggestionRank 0
  bottom = SuggestionRank 100

instance enumSuggestionRank :: Enum SuggestionRank where
  succ (SuggestionRank n)
    | n == 0    = Nothing
    | otherwise = Just (SuggestionRank (n - 1))
  pred (SuggestionRank n)
    | n == 100  = Nothing
    | otherwise = Just (SuggestionRank (n + 1))

fromInt :: Int -> SuggestionRank
fromInt = SuggestionRank <<< clamp 0 100

toString :: SuggestionRank -> String
toString (SuggestionRank n) = String.singleton (Char.fromCharCode (65 + n))
