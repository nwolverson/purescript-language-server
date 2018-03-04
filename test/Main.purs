module Test.Main where

import Prelude

import Data.Array (concat)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import LanguageServer.Text (makeMinimalWorkspaceEdit)
import LanguageServer.Types (DocumentUri(..), Position(..), Range(..), TextDocumentEdit(..), TextEdit(..), WorkspaceEdit(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

getEdit :: WorkspaceEdit -> Array TextEdit
getEdit (WorkspaceEdit {documentChanges}) = concat $ map go $ changes
  where
  go (TextDocumentEdit {edits}) = edits
  changes = fromMaybe [] $ toMaybe documentChanges

mkEdit :: Int -> Int -> String -> TextEdit
mkEdit n m t = TextEdit 
  { range: Range 
    { start: Position 
      { line: n, character: 0 }
    , end: Position
      { line: m, character: 0 }
    }
  , newText: t } 

main :: _
main = runTest do
  suite "workspace edit" do
    test "update line" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\nB\nC\n" "A\nXX\nC\n"
      Assert.equal (Just [ mkEdit 1 2 "XX\n"]) (getEdit <$> edit)
    test "insert line" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\nC\n" "A\nB\nC\n"
      Assert.equal (Just [ mkEdit 1 1 "B\n"]) (getEdit <$> edit)
    test "insert line with more context" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\n1\n2\n3\n4\n5\nC\n" "A\n1\n2\n3\nB\n4\n5\nC\n"
      Assert.equal (Just [ mkEdit 4 4 "B\n"]) (getEdit <$> edit)
    test "no difference" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\nC\n" "A\nC\n"
      Assert.equal (Nothing) (getEdit <$> edit)
    test "first line changed" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\nB\nC\n" "X\nB\nC\n"
      Assert.equal (Just [ mkEdit 0 1 "X\n"]) (getEdit <$> edit)
    test "last line changed" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\nB\nC\n" "A\nB\nX\n"
      Assert.equal (Just [ mkEdit 2 3 "X\n"]) (getEdit <$> edit)

    test "CRLF" do
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\r\nC\r\n" "A\r\nB\r\nC\r\n"
      Assert.equal (Just [ mkEdit 1 1 "B\n"]) (getEdit <$> edit)

    test "CRLF old-only" do
      -- If I have a CRLF file for some reason but IDE server gives me back LF
      let edit = makeMinimalWorkspaceEdit (DocumentUri "uri") 1.0 "A\r\nC\r\n" "A\nB\nC\n"
      Assert.equal (Just [ mkEdit 1 1 "B\n"]) (getEdit <$> edit)