module LanguageServer.IdePurescript.Types where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Fiber)
import Foreign (Foreign)
import Foreign.Object (Object)
import IdePurescript.Modules (State) as Modules
import LanguageServer.Protocol.TextDocument (TextDocument)
import LanguageServer.Protocol.Types (ClientCapabilities, Connection, DocumentStore, DocumentUri, Settings)
import PscIde.Command (RebuildError)
import PureScript.CST (RecoveredParserResult)
import PureScript.CST.Types (Module)

newtype ServerState
  = ServerState
  { port :: Maybe Int
  , root :: Maybe String
  , conn :: Maybe Connection
  , clientCapabilities :: Maybe ClientCapabilities
  , deactivate :: Aff Unit
  , runningRebuild :: Maybe { fiber :: Fiber Unit, uri :: DocumentUri, version :: Number }
  , modules :: Modules.State
  , modulesFile :: Maybe DocumentUri
  , buildQueue :: Object TextDocument
  , diagnostics :: Object (Array RebuildError)
  , parsedModules :: Map DocumentUri { version :: Number, parsed :: RecoveredParserResult Module }
  }

derive instance newtypeServerState :: Newtype ServerState _

type CommandHandler a
  = DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff a
