module LanguageServer.IdePurescript.Types where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Fiber)
import Foreign (Foreign)
import Foreign.Object (Object)
import IdePurescript.Modules (State) as Modules
import LanguageServer.Protocol.TextDocument (TextDocument)
import LanguageServer.Protocol.Types (Connection, DocumentStore, DocumentUri, Settings, ClientCapabilities)
import PscIde.Command (RebuildError)

newtype ServerState
  = ServerState
  { port :: Maybe Int
  , root :: Maybe String
  , conn :: Maybe Connection
  , clientCapabilities :: Maybe ClientCapabilities
  , deactivate :: Aff Unit
  , runningRebuild :: Maybe (Fiber Unit)
  , modules :: Modules.State
  , modulesFile :: Maybe DocumentUri
  , buildQueue :: Object TextDocument
  , diagnostics :: Object (Array RebuildError)
  }

derive instance newtypeServerState :: Newtype ServerState _

type CommandHandler a
  = DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff a
