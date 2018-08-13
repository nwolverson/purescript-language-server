-- | Convert uris for language-server using vscode-uri module
module LanguageServer.Uri where

import Effect (Effect)
import LanguageServer.Types (DocumentUri)

foreign import uriToFilename :: DocumentUri -> Effect String
foreign import filenameToUri :: String -> Effect DocumentUri
