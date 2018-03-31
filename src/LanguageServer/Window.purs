module LanguageServer.Window (showError, showErrorWithActions, showWarning, showWarningWithActions, showInformation, showInformationWithActions) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import LanguageServer.Types (CONN, Connection)

type MessageAction = { title :: String }

foreign import showError :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
foreign import showErrorWithActionsImpl :: forall eff. Connection -> String -> Array MessageAction-> Eff (conn :: CONN | eff) (Promise (Nullable MessageAction))

convertMessageAction :: Nullable MessageAction -> Maybe String
convertMessageAction act = _.title <$> toMaybe act 

showErrorWithActions :: forall eff. Connection -> String -> Array String -> Aff (conn :: CONN | eff) (Maybe String)
showErrorWithActions conn msg acts =
  convertMessageAction <$> (Promise.toAffE $ showErrorWithActionsImpl conn msg (map (\title -> { title }) acts))

foreign import showWarning :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
foreign import showWarningWithActionsImpl :: forall eff. Connection -> String -> Array MessageAction-> Eff (conn :: CONN | eff) (Promise (Nullable MessageAction))

showWarningWithActions :: forall eff. Connection -> String -> Array String -> Aff (conn :: CONN | eff) (Maybe String)
showWarningWithActions conn msg acts =
  convertMessageAction <$> (Promise.toAffE $ showWarningWithActionsImpl conn msg (map (\title -> { title }) acts))


foreign import showInformation :: forall eff. Connection -> String -> Eff (conn :: CONN | eff) Unit
foreign import showInformationWithActionsImpl :: forall eff. Connection -> String -> Array MessageAction-> Eff (conn :: CONN | eff) (Promise (Nullable MessageAction))

showInformationWithActions :: forall eff. Connection -> String -> Array String -> Aff (conn :: CONN | eff) (Maybe String)
showInformationWithActions conn msg acts =
  convertMessageAction <$> (Promise.toAffE $ showInformationWithActionsImpl conn msg (map (\title -> { title }) acts))
