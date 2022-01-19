module LanguageServer.Protocol.Workspace where

import Prelude

import Effect (Effect)
import LanguageServer.Protocol.Types (Connection)

foreign import codeLensRefresh :: Connection -> Effect Unit
