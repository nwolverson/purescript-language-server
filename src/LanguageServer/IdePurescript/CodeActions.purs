module LanguageServer.IdePurescript.CodeActions where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, mapMaybe)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, readInt, readString)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.StrMap (lookup)
import Data.String (null, trim)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (global, noFlags)
import Data.Traversable (traverse)
import IdePurescript.QuickFix (getTitle, isUnknownToken)
import IdePurescript.Regex (replace', test')
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (CodeActionParams, applyEdit)
import LanguageServer.IdePurescript.Build (positionToRange)
import LanguageServer.IdePurescript.Commands (build, fixTypo, replaceSuggestion)
import LanguageServer.IdePurescript.Types (ServerState(..), MainEff)
import LanguageServer.Text (makeWorkspaceEdit)
import LanguageServer.TextDocument (getTextAtRange, getVersion)
import LanguageServer.Types (Command, DocumentStore, DocumentUri(..), Position(..), Range(..), Settings, TextDocumentIdentifier(..))
import PscIde.Command (PscSuggestion(..), RebuildError(..))

getActions :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> CodeActionParams -> Aff (MainEff eff) (Array Command)
getActions documents settings (ServerState { diagnostics, conn }) { textDocument, range } =  
  case lookup (un DocumentUri $ docUri) diagnostics of
    Just errs -> do 
      replacements <- catMaybes <$> traverse asCommand errs
      pure $ replacements <> mapMaybe commandForCode errs
    _ -> pure []
  where
    docUri = _.uri $ un TextDocumentIdentifier textDocument

    asCommand (RebuildError { position: Just position, suggestion: Just (PscSuggestion { replacement, replaceRange }), errorCode })
      | contains range (positionToRange position) = do
      let range' = positionToRange $ fromMaybe position replaceRange
      pure $ Just $ replaceSuggestion (getTitle errorCode) (_.uri $ un TextDocumentIdentifier textDocument) replacement range'
    asCommand _ = pure Nothing

    commandForCode (RebuildError { position: Just position, errorCode }) | contains range (positionToRange position) =
      case errorCode of
        "ModuleNotFound" -> Just build
        x | isUnknownToken x
          , { startLine, startColumn } <- position -> Just $ fixTypo docUri startLine startColumn
        _ -> Nothing
    commandForCode _ = Nothing

    -- TODO if isUnknownToken errorCode -> then add quick fix action

    contains (Range { start, end }) (Range { start: start', end: end' }) = start <= start' && end >= end'

readRange :: Foreign -> F Range
readRange r = do
  start <- r ! "start" >>= readPosition
  end <- r ! "end" >>= readPosition
  pure $ Range { start, end }
  where
  readPosition p = do
    line <- p ! "line" >>= readInt
    character <- p ! "character" >>= readInt
    pure $ Position { line, character }

afterEnd :: Range -> Range
afterEnd (Range { end: end@(Position { line, character }) }) = 
  Range
    { start: end
    , end: Position { line, character: character + 10 }
    }

toNextLine :: Range -> Range
toNextLine (Range { start, end: end@(Position { line, character }) }) = 
  Range
    { start
    , end: Position { line: line+1, character: 0 }
    }

onReplaceSuggestion :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Unit
onReplaceSuggestion docs config (ServerState { conn }) args =
  case conn, args of 
    Just conn', [ uri', replacement', range' ]
      | Right uri <- runExcept $ readString uri'
      , Right replacement <- runExcept $ readString replacement'
      , Right range <- runExcept $ readRange range'
      -> liftEff do
        doc <- getDocument docs (DocumentUri uri)
        version <- getVersion doc
        origText <- getTextAtRange doc range
        afterText <- replace' (regex "\n$" noFlags) "" <$> getTextAtRange doc (afterEnd range)

        let newText = getReplacement replacement afterText
        
        let range' = if newText == "" && afterText == "" then
                      toNextLine range
                     else
                      range
        let edit = makeWorkspaceEdit (DocumentUri uri) version range' newText

        -- TODO: Check original & expected text ?
        applyEdit conn' edit
    _, _ -> pure unit
  where
    -- | Modify suggestion replacement text, removing extraneous newlines
    getReplacement :: String -> String -> String
    getReplacement replacement extraText =
      (trim $ replace' (regex "\\s+\n" global) "\n" replacement)
      <> if addNewline then "\n" else ""
      where
      trailingNewline = test' (regex "\n\\s+$" noFlags) replacement
      addNewline = trailingNewline && (not $ null extraText)
