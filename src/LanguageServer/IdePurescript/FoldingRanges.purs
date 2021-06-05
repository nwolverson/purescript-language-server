module LanguageServer.IdePurescript.FoldingRanges where

import Prelude
import Data.Array (findIndex, findLastIndex, fromFoldable)
import Data.Maybe (Maybe)
import Data.Nullable as Nullable
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (startsWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (FoldingRangesParams)
import LanguageServer.IdePurescript.Types (ServerState)
import LanguageServer.TextDocument (getText)
import LanguageServer.Types (DocumentStore, FoldingRange(..), Settings, TextDocumentIdentifier(..))

getFoldingRanges :: DocumentStore -> Settings -> ServerState -> FoldingRangesParams -> Aff (Array FoldingRange)
getFoldingRanges docs _ _ { textDocument: TextDocumentIdentifier textDocId } =
  liftEffect do
    doc <- getDocument docs textDocId.uri
    text <- getText doc
    pure <<< fromFoldable
      $ getImportsSection text

newlineRegex :: Regex
newlineRegex = unsafeRegex "[\n\r]" noFlags

getImportsSection :: String -> Maybe FoldingRange
getImportsSection text = ado
  firstImport <- findIndex isImportLine lines
  lastImport <- findLastIndex isImportLine lines
  in FoldingRange
    { startLine: firstImport
    , endLine: lastImport
    , startCharacter: Nullable.notNull 0
    , endCharacter:
      Nullable.notNull
        $ CodeUnits.length importPrefix - 1
    , kind: Nullable.notNull "imports"
    }
  where
  importPrefix = "import "
  lines = Regex.split newlineRegex text
  isImportLine = startsWith importPrefix
