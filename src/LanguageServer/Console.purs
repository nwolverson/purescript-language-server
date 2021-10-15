module LanguageServer.Console where

import Prelude
import Effect (Effect)
import LanguageServer.Types (Connection)

foreign import log :: Connection -> String -> Effect Unit
foreign import info :: Connection -> String -> Effect Unit
foreign import warn :: Connection -> String -> Effect Unit
foreign import error :: Connection -> String -> Effect Unit
