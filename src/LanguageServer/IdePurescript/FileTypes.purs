module LanguageServer.IdePurescript.FileTypes
  ( RelevantFileType(..)
  , jsUriToMayPsUri
  , uriExtensionIs
  , uriToRelevantFileType
  )
  where

import Prelude
import Data.Maybe (Maybe)
import Data.String as String
import LanguageServer.Protocol.Types (DocumentUri(..))

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

uriToRelevantFileType ∷ DocumentUri → RelevantFileType
uriToRelevantFileType uri = f
  where
  extIs = uriExtensionIs uri
  f
    | extIs "purs" = PureScriptFile
    | extIs "js" = JavaScriptFile
    | otherwise = UnsupportedFile

uriExtensionIs ∷ DocumentUri → String → Boolean
uriExtensionIs (DocumentUri uri) ext = ext' == after
  where
  ext' = "." <> ext
  { after } = String.splitAt (String.length uri - String.length ext') uri

jsUriToMayPsUri ∷ DocumentUri → Maybe DocumentUri
jsUriToMayPsUri (DocumentUri str) =
  (String.stripSuffix (String.Pattern ".js") str)
    <#> (_ <> ".purs")
    <#> DocumentUri