module LanguageServer.Setup where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import LanguageServer.Types (Connection, DocumentStore, DocumentUri)

newtype InitParams = InitParams { rootUri :: Nullable DocumentUri, rootPath :: Nullable String, trace :: Nullable String }
type InitResult = { conn :: Connection, params :: InitParams }

foreign import initConnection :: Array String -> (InitResult -> Effect Unit) -> Effect Connection

foreign import initDocumentStore :: Connection -> Effect DocumentStore

