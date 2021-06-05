module LanguageServer.IdePurescript.Tooltips where

import Prelude

import Data.Array (uncons)
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Data.String (drop, length, take)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getTypeInfo)
import IdePurescript.Tokens (WordRange, identPart, identifierAtPoint)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (TextDocumentPositionParams)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (DocumentStore, Hover(Hover), Position(Position), Range(Range), Settings, TextDocumentIdentifier(TextDocumentIdentifier), markupContent)
import PscIde.Command as C

moduleBeforePart :: String
moduleBeforePart = """(?:^|[^A-Za-z_.])((?:[A-Z][A-Za-z0-9]*(?:\.(?:[A-Z][A-Za-z0-9]*)?)*)?)"""

moduleAfterPart :: String
moduleAfterPart = """([A-Za-z0-9]*(?:\.[A-Za-z0-9]*)*)\.""" 

afterPart :: String
afterPart = moduleAfterPart <> identPart <> "(?:[^A-Za-z_'.]|$)"-- identPart captures 1

moduleAtPoint :: String -> Int -> Maybe { word :: String, range :: WordRange }
moduleAtPoint line column =
  let textBefore = take column line
      textAfter = drop column line
      beforeRegex = regex (moduleBeforePart <> "$") noFlags
      afterRegex = regex ("^" <> afterPart) noFlags
      wordRange left right = { left: column - left, right: column + right }
      match' r t = either (const Nothing) (\r' -> match r' t) r
  in
  case NEA.toArray <$> match' beforeRegex textBefore, NEA.toArray <$> match' afterRegex textAfter of
    Just [_, Just m1], Just [_, Just m2, _] ->
      Just { word : m1 <> m2, range : wordRange (length m1) (length m2) }
    _, _ -> Nothing

getTooltips :: DocumentStore -> Settings -> ServerState -> TextDocumentPositionParams -> Aff (Nullable Hover)
getTooltips docs _ state ({ textDocument, position }) = do
  doc <- liftEffect $ getDocument docs (_.uri $ un TextDocumentIdentifier textDocument)
  text <- liftEffect $ getTextAtRange doc $ lineRange position
  let { port, modules } = un ServerState state
      char = _.character $ un Position $ position
  case port, identifierAtPoint text char, moduleAtPoint text char of
    Just _, _, Just { word, range } -> do
      let mod = getQualModule word (un ServerState state).modules
      pure $ toNullable $ case uncons mod of 
        Just { head } -> 
          Just $ Hover {
            contents: markupContent head
          , range: toNullable $ Just $ wordRange position range
          }
        _ -> Nothing
    Just port', Just { word, qualifier }, _ -> do
      ty <- getTypeInfo port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)
      pure $ toNullable $ map (convertInfo word) ty
    _, _, _-> pure $ toNullable Nothing

  where
 
  convertInfo word (C.TypeInfo { type', expandedType, documentation }) = Hover 
    {
      contents: 
        markupContent $ typeStr <> "\n" <> (fromMaybe "" documentation)
    , range: toNullable $ Nothing
    }
    where
      typeStr = "```purescript\n" <> compactTypeStr <> (if showExpanded then "\n" <> expandedTypeStr else "") <> "\n```"
      showExpanded = isJust expandedType && (expandedType /= Just type')
      compactTypeStr = word <> " :: " <> type'
      expandedTypeStr = word <> " :: " <> (fromMaybe "" expandedType)

  wordRange (Position { line }) { left, right } = 
    Range
      { start: Position
        { line
        , character: left
        }
      , end: Position
        { line
        , character: right
        }
      }

  lineRange (Position { line, character }) =
    Range
      { start: Position
        { line
        , character: 0
        }
      , end: Position
        { line
        , character: character + 100
        }
      }