module LanguageServer.Setup where

import Prelude
import LanguageServer.Types (CONN, Connection, DocumentStore)
import Control.Monad.Eff (Eff, kind Effect)
import Data.Nullable (Nullable)

newtype InitParams = InitParams { rootUri :: Nullable String, rootPath :: Nullable String, trace :: Nullable String }
type InitResult = { conn :: Connection, params :: InitParams }

foreign import initConnection :: forall eff. Array String -> (InitResult -> Eff (conn :: CONN | eff) Unit) ->  Eff (conn :: CONN | eff) Connection

foreign import initDocumentStore :: forall eff. Connection -> Eff (conn :: CONN | eff) DocumentStore

