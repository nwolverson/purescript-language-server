module LanguageServer.IdePurescript.Tooltips
  ( getTooltips
  ) where

import Prelude

import Data.Array (uncons)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule)
import IdePurescript.PscIdeServer (Notify)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.TypeInfo (getTypeInfoMaybeNew)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (TextDocumentPositionParams)
import LanguageServer.Protocol.TextDocument (getTextAtRange)
import LanguageServer.Protocol.Types (DocumentStore, Hover(Hover), Position(Position), Range(Range), Settings, TextDocumentIdentifier(TextDocumentIdentifier), markupContent)
import Literals.Undefined (undefined)
import PscIde.Command as C
import Untagged.Union (asOneOf)

getTooltips :: Notify -> DocumentStore -> Settings -> ServerState -> TextDocumentPositionParams -> Aff (Nullable Hover)
getTooltips notify docs _ state ({ textDocument, position }) = toNullable <$> do
  let uri = (_.uri $ un TextDocumentIdentifier textDocument)
  maybeDoc <- liftEffect $ getDocument docs uri
  case Nullable.toMaybe maybeDoc of
    Nothing -> pure Nothing
    Just doc -> do
      text <- liftEffect $ getTextAtRange doc $ lineRange position
      let
        char = _.character $ un Position $ position
      case identifierAtPoint text char of
        Just { word, qualifier, range: range@{ left } } -> do
          case qualifier of
            Just q
              | char < left + String.length q -> do
                  let mod = getQualModule q (un ServerState state).modules
                  pure $ (\{ head } -> Hover
                            { contents: markupContent head
                            , range: asOneOf $ wordRange position range
                                { right = left + String.length q }
                            }) <$> uncons mod
            _ ->
              map (convertInfo word) <$> getTypeInfoMaybeNew notify state uri word qualifier

        _ -> pure Nothing
  where
  
  convertInfo word (C.TypeInfo { type', expandedType, documentation }) =
    Hover
      { contents:
          markupContent $ typeStr <> "\n" <> (fromMaybe "" documentation)
      , range: asOneOf undefined
      }
    where
    typeStr = "```purescript\n" <> compactTypeStr
      <> (if showExpanded then "\n" <> expandedTypeStr else "")
      <> "\n```"
    showExpanded = isJust expandedType && (expandedType /= Just type')
    compactTypeStr = word <> " :: " <> type'
    expandedTypeStr = word <> " :: " <> (fromMaybe "" expandedType)

  wordRange (Position { line }) { left, right } =
    Range
      { start:
          Position { line, character: left }
      , end: Position { line, character: right }
      }

  lineRange (Position { line, character }) =
    Range
      { start: Position { line, character: 0 }
      , end: Position { line, character: character + 100 }
      }
