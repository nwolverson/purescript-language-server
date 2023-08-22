module LanguageServer.Protocol.Text where

import Prelude

import Data.Array (findIndex, last, length, null, reverse, slice, zip)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.String (joinWith)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (uncurry)
import LanguageServer.Protocol.Types (ClientCapabilities, DocumentUri, OptionalVersionedTextDocumentIdentifier(..), Position(..), Range(..), TextDocumentEdit(..), TextEdit(..), WorkspaceEdit, workspaceEdit)

makeWorkspaceEdit ::
  Maybe ClientCapabilities ->
  DocumentUri ->
  Number ->
  Range ->
  String ->
  WorkspaceEdit
makeWorkspaceEdit capabilities uri version range newText = workspaceEdit
  capabilities
  [ edit ]
  where
  textEdit = TextEdit { range, newText }
  docid = OptionalVersionedTextDocumentIdentifier
    { uri, version: Nullable.notNull version }
  edit = TextDocumentEdit { textDocument: docid, edits: [ textEdit ] }

type EditParams =
  { range :: Range
  , newText :: String
  }

type MultiEdit =
  { uri :: DocumentUri
  , version :: Maybe Number
  , edits :: Array { range :: Range, newText :: String }
  }

makeMultiWorkspaceEdit ::
  Maybe ClientCapabilities -> Array (MultiEdit) -> WorkspaceEdit
makeMultiWorkspaceEdit capabilities multiEdits =
  workspaceEdit capabilities
    $ multiEdits
        <#> \{ uri, version, edits } ->
          TextDocumentEdit
            { textDocument: OptionalVersionedTextDocumentIdentifier
                { uri, version: Nullable.toNullable version }
            , edits: edits <#> \{ range, newText } -> TextEdit
                { range, newText }
            }

-- | Make a full-text workspace edit via a minimal diff under the assumption that at most one change is required
-- | In particular the scenario of inserting text in the middle AC -> ABC becomes an edit of B only.
makeMinimalWorkspaceEdit ::
  Maybe ClientCapabilities ->
  DocumentUri ->
  Number ->
  String ->
  String ->
  Maybe WorkspaceEdit
makeMinimalWorkspaceEdit clientCapabilities uri version oldText newText =
  let
    splitLines t = either (const [ t ]) (\r -> Regex.split r t) $ regex "\r?\n"
      noFlags
    newLines = splitLines newText
    oldLines = case splitLines oldText of
      -- Add imports adds a newline to the end of the file always, giving bad diffs
      xs | last xs /= Just "" && last newLines == Just "" -> xs <> [ "" ]
      xs -> xs

    range text l1 l2 = Range
      { start: Position { line: l1, character: 0 }
      , end: Position { line: length text - l2, character: 0 }
      }
    lines text l1 l2 = slice l1 (length text - l2) text

    firstDiff = findIndex (uncurry (/=)) (zip oldLines newLines)
    lastDiff = findIndex (uncurry (/=))
      (zip (reverse oldLines) (reverse newLines))

    e a b = Just $ makeWorkspaceEdit clientCapabilities uri version a b
    oldLen = length oldLines
    newLen = length newLines

  in
    case firstDiff, lastDiff of
      Just n, Just m
        | newLen - m >= n ->
            let
              m' = min m (oldLen - n)
            in
              e (range oldLines n m')
                ( joinWith "\n" (lines newLines n m') <>
                    if null newLines then "" else "\n"
                )
      Nothing, Nothing
        | oldLen == newLen -> Nothing
      _, _ -> e (range oldLines 0 0) newText
