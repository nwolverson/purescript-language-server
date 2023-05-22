module LanguageServer.IdePurescript.Util.Position where

import Prelude

import Data.Lens (Iso', Lens', iso, over)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import LanguageServer.Protocol.Types as LSP
import PscIde.Command as PscIde
import Type.Proxy (Proxy(..))

-- Some Lenses
_line :: Lens' LSP.Position Int
_line = _Newtype <<< prop (Proxy :: Proxy "line")

_character :: Lens' LSP.Position Int
_character = _Newtype <<< prop (Proxy :: Proxy "character")

_startPosition :: Lens' LSP.Range LSP.Position
_startPosition = _Newtype <<< prop (Proxy :: Proxy "start")

_endPosition :: Lens' LSP.Range LSP.Position
_endPosition = _Newtype <<< prop (Proxy :: Proxy "end")

shiftRangeDown :: Int -> LSP.Range -> LSP.Range
shiftRangeDown by =
  over (_startPosition <<< _line) (_ + by)
    <<< over (_endPosition <<< _line) (_ + by)

shiftRangeUp :: Int -> LSP.Range -> LSP.Range
shiftRangeUp = negate >>> shiftRangeDown

isoPscIdeLspPosition :: Iso' PscIde.Position LSP.Position
isoPscIdeLspPosition = iso convertPosition lspPositionToPscIdePosition

shiftPositionDown :: Int -> LSP.Position -> LSP.Position
shiftPositionDown by = over _line (_ + by)

shiftPositionUp :: Int -> LSP.Position -> LSP.Position
shiftPositionUp = negate >>> shiftPositionDown

shiftPositionRight :: Int -> LSP.Position -> LSP.Position
shiftPositionRight by = over _character (_ + by)

shiftPositionLeft :: Int -> LSP.Position -> LSP.Position
shiftPositionLeft by = over _character (_ + by)

convertPosition :: PscIde.Position -> LSP.Position
convertPosition { line, column } =
  LSP.Position
    { line: line - 1
    , character: column - 1
    }

lspPositionToPscIdePosition :: LSP.Position -> PscIde.Position
lspPositionToPscIdePosition (LSP.Position { line, character }) =
  { line: line + 1
  , column: character + 1
  }

convertTypePosition :: PscIde.TypePosition -> LSP.Range
convertTypePosition (PscIde.TypePosition { start, end }) =
  LSP.Range
    { start: convertPosition start
    , end: convertPosition end
    }

convertRangePosition :: PscIde.RangePosition -> LSP.Range
convertRangePosition { startLine, startColumn, endLine, endColumn } =
  LSP.Range
    { start: convertPosition { line: startLine, column: startColumn }
    , end: convertPosition { line: endLine, column: endColumn }
    }
