module LanguageServer.Text where

import Prelude

import Data.Array (findIndex, last, length, null, reverse, slice, zip)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Tuple (uncurry)
import LanguageServer.Types (DocumentUri, Position(..), Range(..), TextDocumentEdit(..), TextDocumentIdentifier(..), TextEdit(..), WorkspaceEdit, workspaceEdit)

makeWorkspaceEdit :: DocumentUri -> Number -> Range -> String -> WorkspaceEdit
makeWorkspaceEdit uri version range newText = workspaceEdit [ edit ]
  where 
      textEdit = TextEdit { range, newText }
      docid = TextDocumentIdentifier { uri, version }
      edit = TextDocumentEdit { textDocument: docid, edits: [ textEdit ] }

-- | Make a full-text workspace edit via a minimal diff under the assumption that at most one change is required
-- | In particular the scenario of inserting text in the middle AC -> ABC becomes an edit of B only.
makeMinimalWorkspaceEdit :: DocumentUri -> Number -> String -> String -> Maybe WorkspaceEdit
makeMinimalWorkspaceEdit uri version oldText newText =
  let newLines = split (Pattern "\n") newText
      oldLines = case split (Pattern "\n") oldText of
        -- Add imports adds a newline to the end of the file always, giving bad diffs
        xs | last xs /= Just "" && last newLines == Just "" -> xs <> [""]
        xs -> xs

      range text l1 l2 = Range
        { start: Position { line: l1, character: 0 },
          end: Position { line: length text - l2 + 1, character: 0 }
        }
      lines text l1 l2 = slice l1 (length text - l2 + 1) text

      firstDiff = findIndex (uncurry (/=)) (zip oldLines newLines)
      lastDiff = findIndex (uncurry (/=)) (zip (reverse oldLines) (reverse newLines))

      e a b = Just $ makeWorkspaceEdit uri version a b
      oldLen = length oldLines
      newLen = length newLines

  in case firstDiff, lastDiff of
      Just n, Just m
        | newLen - m >= n
          -> 
          let m' = min m (oldLen - n) in
          e (range oldLines n m') (joinWith "\n" (lines newLines n m') <> if null newLines then "" else "\n")
      Nothing, Nothing
        | oldLen == newLen -> Nothing
      _, _ -> e (range oldLines 0 0) newText

