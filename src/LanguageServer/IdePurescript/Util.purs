module LanguageServer.IdePurescript.Util where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, runAff)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST.Types (Module)

launchAffLog :: forall a. Notify -> Aff a -> Effect (Fiber Unit)
launchAffLog notify =
  runAff $ either (notify Error <<< show) (const $ pure unit)

maybeParseResult :: forall a. a -> (forall b. Module b -> a) -> RecoveredParserResult Module -> a
maybeParseResult default f = 
  case _ of
    ParseSucceeded x -> f x
    ParseSucceededWithErrors x _errs -> f x
    ParseFailed _err -> default