module LanguageServer.IdePurescript.Util where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, runAff)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)

launchAffLog :: forall a. Notify -> Aff a -> Effect (Fiber Unit)
launchAffLog notify =
  runAff $ either (notify Error <<< show) (const $ pure unit)
