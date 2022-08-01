module LanguageServer.IdePurescript.FileTypes where

import Prelude

import Control.Alternative ((<|>))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String

import LanguageServer.IdePurescript.Types (ServerState(..), ServerStateRec)
import LanguageServer.Protocol.TextDocument (TextDocument, getText, getUri, getVersion)
import LanguageServer.Protocol.Types (DocumentStore, DocumentUri(..))

-- | Type representing files passsed by the IDE
data RelevantFileType
  = PureScriptFile
  | JavaScriptFile
  | UnsupportedFile

derive instance Eq RelevantFileType
instance Show RelevantFileType where
  show PureScriptFile = "PureScriptFile"
  show JavaScriptFile = "JavaScriptFile"
  show UnsupportedFile = "UnsupportedFile"

uriToRelevantFileType uri = f
  where
    extIs = uriExtensionIs uri
    f
      | extIs "purs" = PureScriptFile
      | extIs "js" = JavaScriptFile
      | otherwise = UnsupportedFile

uriExtensionIs (DocumentUri uri) ext = ext' == after
  where
    ext' = "." <> ext
    {after} = String.splitAt (String.length uri - String.length ext') uri

jsUriToMayPsUri (DocumentUri str) =
  (String.stripSuffix (String.Pattern ".js") str)
  <#> (_ <> ".purs")
  <#> DocumentUri


tryGetDocument :: ServerState -> DocumentUri -> Maybe TextDocument
tryGetDocument (ServerState ssr) uri =
    -- TODO: find out from maintainers if this fallback ordering is correct
    (Map.lookup uri ssr.fastRebuildQueue)
    <|> (Map.lookup uri ssr.diagnosticsQueue)
    <|> (_.document <$> Map.lookup uri ssr.parsedModules)