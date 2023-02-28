module LanguageServer.IdePurescript.Util.CST where

import Data.Lens (Iso', iso)
import LanguageServer.Protocol.Types as LSP
import PureScript.CST.Types as CST

_sourcePosition :: Iso' CST.SourcePos LSP.Position
_sourcePosition = iso sourcePosToPosition sourcePosFromPosition

_sourceRange :: Iso' CST.SourceRange LSP.Range
_sourceRange = iso sourceRangeToRange sourceRangeFromRange

sourceRangeToRange :: CST.SourceRange -> LSP.Range
sourceRangeToRange sr =
  LSP.Range
    { start: sourcePosToPosition sr.start
    , end: sourcePosToPosition sr.end
    }

sourceRangeFromRange :: LSP.Range -> CST.SourceRange
sourceRangeFromRange (LSP.Range lspRange) =
  { start: sourcePosFromPosition lspRange.start
  , end: sourcePosFromPosition lspRange.end
  }

-- SourcePos are 0 based 
sourcePosToPosition :: CST.SourcePos -> LSP.Position
sourcePosToPosition { line, column } = do
  LSP.Position { line: line, character: column }

-- SourcePos are 0 based 
sourcePosFromPosition :: LSP.Position -> CST.SourcePos
sourcePosFromPosition (LSP.Position { line, character }) = do
  { line: line, column: character }
