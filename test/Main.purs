module Test.Main where

import Prelude

import Data.Array (concat)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (null, toMaybe)
import Data.Nullable as Nullable
import Effect (Effect)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.FileTypes (RelevantFileType(..), jsUriToMayPsUri, uriToRelevantFileType)
import LanguageServer.Protocol.Text (makeMinimalWorkspaceEdit)
import LanguageServer.Protocol.Types (ClientCapabilities, DocumentUri(..), Position(..), Range(..), TextDocumentEdit(..), TextEdit(..), WorkspaceEdit(..))
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

getEdit :: WorkspaceEdit -> Array TextEdit
getEdit (WorkspaceEdit { documentChanges }) = concat $ map go $ changes
  where
  go (TextDocumentEdit { edits }) = edits
  changes = fromMaybe [] $ toMaybe documentChanges

mkEdit :: Int -> Int -> String -> TextEdit
mkEdit n m t =
  TextEdit
    { range:
        Range
          { start: Position { line: n, character: 0 }
          , end: Position { line: m, character: 0 }
          }
    , newText: t
    }

capabilities :: ClientCapabilities
capabilities =
  { workspace:
      Nullable.notNull
        { applyEdit: Nullable.notNull true
        , workspaceEdit: Nullable.notNull
            { documentChanges: Nullable.notNull true }
        , codeLens: Nullable.null
        }
  , textDocument: null
  }

makeEdit :: String -> String -> Maybe WorkspaceEdit
makeEdit = makeMinimalWorkspaceEdit (Just capabilities) (DocumentUri "uri") 1.0

main :: Effect Unit
main =
  runTest do
    suite "workspace edit" do
      test "update line" do
        let edit = makeEdit "A\nB\nC\n" "A\nXX\nC\n"
        Assert.equal (Just [ mkEdit 1 2 "XX\n" ]) (getEdit <$> edit)
      test "insert line" do
        let edit = makeEdit "A\nC\n" "A\nB\nC\n"
        Assert.equal (Just [ mkEdit 1 1 "B\n" ]) (getEdit <$> edit)
      test "insert line with more context" do
        let edit = makeEdit "A\n1\n2\n3\n4\n5\nC\n" "A\n1\n2\n3\nB\n4\n5\nC\n"
        Assert.equal (Just [ mkEdit 4 4 "B\n" ]) (getEdit <$> edit)
      test "no difference" do
        let edit = makeEdit "A\nC\n" "A\nC\n"
        Assert.equal (Nothing) (getEdit <$> edit)
      test "first line changed" do
        let edit = makeEdit "A\nB\nC\n" "X\nB\nC\n"
        Assert.equal (Just [ mkEdit 0 1 "X\n" ]) (getEdit <$> edit)
      test "last line changed" do
        let edit = makeEdit "A\nB\nC\n" "A\nB\nX\n"
        Assert.equal (Just [ mkEdit 2 3 "X\n" ]) (getEdit <$> edit)
      test "CRLF" do
        let edit = makeEdit "A\r\nC\r\n" "A\r\nB\r\nC\r\n"
        Assert.equal (Just [ mkEdit 1 1 "B\n" ]) (getEdit <$> edit)
      test "CRLF old-only" do
        -- If I have a CRLF file for some reason but IDE server gives me back LF
        let edit = makeEdit "A\r\nC\r\n" "A\nB\nC\n"
        Assert.equal (Just [ mkEdit 1 1 "B\n" ]) (getEdit <$> edit)
      -- Type at point
      test "identifierAtPoint: identifies $" do
        let str = """$"""
        let result = identifierAtPoint str 0
        Assert.equal (result <#> _.word) (Just "$")
      test """identifierAtPoint: identifies <>""" do
        let str = """ 4 <> 3 """
        let result = identifierAtPoint str 3
        Assert.equal (result <#> _.word) (Just """<>""")
        Assert.equal (result <#> _.range) (Just { left: 3, right: 5 })
      test """identifierAtPoint: identifies /\""" do
        let str = """ 4 /\ 3 """
        let result = identifierAtPoint str 3
        Assert.equal (result <#> _.word) (Just """/\""")
        Assert.equal (result <#> _.range) (Just { left: 3, right: 5 })
    suite "file handling" do
      test "Determine file type" do
        let f x y = Assert.equal x $ uriToRelevantFileType $ DocumentUri y
        f PureScriptFile "foo/bar/baz.purs"
        f PureScriptFile "./foo.purs"
        f PureScriptFile "foo.purs"
        f JavaScriptFile "foo.js"
        f UnsupportedFile "foo.xyz"
      test "convert js uri to ps uri" do
        let mmUnwrap (DocumentUri x) = x
        let
          f x y = Assert.equal (Just x) $ mmUnwrap <$> jsUriToMayPsUri ( DocumentUri y
            )
        f "foo/bar/baz.purs" "foo/bar/baz.js"
        f "./foo.purs" "./foo.js"
        f "foo.purs" "foo.js"
