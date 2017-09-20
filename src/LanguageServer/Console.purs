module LanguageServer.Console where

import Prelude
import LanguageServer.Types (CONN, Connection)
import Control.Monad.Eff (Eff)

foreign import log :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
foreign import info :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
foreign import warn :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
foreign import error :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
