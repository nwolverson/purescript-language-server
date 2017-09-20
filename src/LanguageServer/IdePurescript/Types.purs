module LanguageServer.IdePurescript.Types where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import IdePurescript.Modules (State)
import IdePurescript.PscErrors (PscError)
import LanguageServer.Types (CONN, Connection, DocumentStore, DocumentUri, Settings)
import Node.Buffer (BUFFER)
import Node.ChildProcess (CHILD_PROCESS)
import Node.FS (FS)
import Node.Process (PROCESS)
import PscIde (NET)

type MainEff eff =
    ( process :: PROCESS
    , conn :: CONN
    , ref :: REF
    , avar :: AVAR
    , buffer :: BUFFER
    , console :: CONSOLE
    , cp :: CHILD_PROCESS
    , exception :: EXCEPTION
    , fs :: FS
    , net :: NET
    , random :: RANDOM | eff)

newtype ServerState eff = ServerState
  { port :: Maybe Int
  , deactivate :: Aff eff Unit
  , root :: Maybe String
  , conn :: Maybe Connection
  , modules :: State
  , modulesFile :: Maybe DocumentUri
  , diagnostics :: StrMap (Array PscError)
  }

derive instance newtypeServerState :: Newtype (ServerState eff) _

type CommandHandler eff a = DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) a

