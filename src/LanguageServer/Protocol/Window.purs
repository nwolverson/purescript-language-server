module LanguageServer.Protocol.Window
  ( showError
  , showErrorWithActions
  , showWarning
  , showWarningWithActions
  , showInformation
  , showInformationWithActions
  , workDone
  , workBegin
  , report
  , report2
  , reportMsg
  , WorkDoneProgressReporter
  , createWorkDoneProgress
  ) where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import LanguageServer.Protocol.Types (Connection)

type MessageAction = { title :: String }

foreign import showError :: Connection -> String -> Effect Unit
foreign import showErrorWithActionsImpl ::
  Connection ->
  String ->
  Array MessageAction ->
  Effect (Promise (Nullable MessageAction))

convertMessageAction :: Nullable MessageAction -> Maybe String
convertMessageAction act = _.title <$> toMaybe act

showErrorWithActions ::
  Connection -> String -> Array String -> Aff (Maybe String)
showErrorWithActions conn msg acts =
  convertMessageAction <$>
    ( Promise.toAffE $ showErrorWithActionsImpl conn msg
        (map (\title -> { title }) acts)
    )

foreign import showWarning :: Connection -> String -> Effect Unit
foreign import showWarningWithActionsImpl ::
  Connection ->
  String ->
  Array MessageAction ->
  Effect (Promise (Nullable MessageAction))

showWarningWithActions ::
  Connection -> String -> Array String -> Aff (Maybe String)
showWarningWithActions conn msg acts =
  convertMessageAction <$>
    ( Promise.toAffE $ showWarningWithActionsImpl conn msg
        (map (\title -> { title }) acts)
    )

foreign import showInformation :: Connection -> String -> Effect Unit
foreign import showInformationWithActionsImpl ::
  Connection ->
  String ->
  Array MessageAction ->
  Effect (Promise (Nullable MessageAction))

showInformationWithActions ::
  Connection -> String -> Array String -> Aff (Maybe String)
showInformationWithActions conn msg acts =
  convertMessageAction <$>
    ( Promise.toAffE $ showInformationWithActionsImpl conn msg
        (map (\title -> { title }) acts)
    )

foreign import data WorkDoneProgressReporter :: Type

foreign import workDone :: WorkDoneProgressReporter -> Effect Unit
foreign import workBegin ::
  WorkDoneProgressReporter -> { title :: String } -> Effect Unit

foreign import report :: WorkDoneProgressReporter -> Number -> Effect Unit
foreign import reportMsg :: WorkDoneProgressReporter -> String -> Effect Unit
foreign import report2 ::
  WorkDoneProgressReporter -> Number -> String -> Effect Unit

foreign import createWorkDoneProgressImpl ::
  Connection -> Effect (Promise WorkDoneProgressReporter)

createWorkDoneProgress :: Connection -> Aff WorkDoneProgressReporter
createWorkDoneProgress conn = Promise.toAffE $ createWorkDoneProgressImpl conn
