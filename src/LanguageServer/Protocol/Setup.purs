module LanguageServer.Protocol.Setup where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import LanguageServer.Protocol.Types (ClientCapabilities, Connection, DocumentStore, DocumentUri)

newtype InitParams = InitParams
  { rootUri :: Nullable DocumentUri
  , rootPath :: Nullable String
  , trace :: Nullable String
  , capabilities :: ClientCapabilities
  }

type InitResult = { conn :: Connection, params :: InitParams }

type Res a = Effect (Promise a)

foreign import initConnection ::
  Array String -> (InitResult -> Effect Unit) -> Effect Connection

foreign import initDocumentStore :: Connection -> Effect DocumentStore

foreign import getConfigurationImpl :: Connection -> Res Foreign

getConfiguration :: Connection -> Aff Foreign
getConfiguration conn = Promise.toAffE $ getConfigurationImpl conn
