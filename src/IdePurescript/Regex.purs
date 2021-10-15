module IdePurescript.Regex where

import Prelude
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex as R

replace' :: forall a. Either a R.Regex -> String -> String -> String
replace' (Left _) _ s = s
replace' (Right r) t s = R.replace r t s

match' :: forall a. Either a R.Regex -> String -> Maybe (Array (Maybe String))
match' (Left _) _ = Nothing
match' (Right r) s = NEA.toArray <$> R.match r s

test' :: forall a. Either a R.Regex -> String -> Boolean
test' (Left _) = const false
test' (Right r) = R.test r
