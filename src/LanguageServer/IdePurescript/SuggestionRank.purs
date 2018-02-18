module LanguageServer.IdePurescript.SuggestionRank
  ( SuggestionRank
  , fromInt
  , toString
  , Ranking(..)
  ) where

import Prelude

import Data.Char as Char
import Data.Enum (class Enum)
import Data.Functor.Contravariant (class Contravariant)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
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

instance semigroupSuggestionRank :: Semigroup SuggestionRank where
  append (SuggestionRank a) (SuggestionRank b) = SuggestionRank (min a b)

instance monoidSuggestionRank :: Monoid SuggestionRank where
  mempty = bottom

fromInt :: Int -> SuggestionRank
fromInt = SuggestionRank <<< clamp 0 100

toString :: SuggestionRank -> String
toString (SuggestionRank n) = String.singleton (Char.fromCharCode (65 + n))

newtype Ranking a = Ranking (a -> SuggestionRank)

derive instance newtypeRanking :: Newtype (Ranking a) _

instance semigroupRanking :: Semigroup (Ranking a) where
  append (Ranking f) (Ranking g) = Ranking \a ->
    let rank = f a in if rank == top then rank else rank <> g a

instance monoidRanking :: Monoid (Ranking a) where
  mempty = Ranking (const bottom)

instance contravariantRanking :: Contravariant Ranking where
  cmap f (Ranking g) = Ranking (f >>> g)
