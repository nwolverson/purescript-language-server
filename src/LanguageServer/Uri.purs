-- | Convert uris for language-server using vscode-uri module
module LanguageServer.Uri where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import LanguageServer.Types (DocumentUri)

foreign import uriToFilename :: forall eff. DocumentUri -> Eff (exception :: EXCEPTION | eff) String
foreign import filenameToUri :: forall eff. String -> Eff (exception :: EXCEPTION | eff) DocumentUri

