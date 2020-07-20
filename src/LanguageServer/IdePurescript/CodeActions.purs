module LanguageServer.IdePurescript.CodeActions where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, filter, length, mapMaybe, nubByEq)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.String (null, trim)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (global, noFlags)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (F, Foreign, readArray, readInt, readString)
import Foreign.Index ((!))
import Foreign.Object as Object
import IdePurescript.QuickFix (getTitle, isImport, isUnknownToken)
import IdePurescript.Regex (replace', test')
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (CodeActionParams, applyEdit)
import LanguageServer.IdePurescript.Build (positionToRange)
import LanguageServer.IdePurescript.Commands (Replacement, build, fixTypo, replaceAllSuggestions, replaceSuggestion, typedHole)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Text (makeWorkspaceEdit)
import LanguageServer.TextDocument (TextDocument, getTextAtRange, getVersion)
import LanguageServer.Types (Command, DocumentStore, DocumentUri(DocumentUri), Position(Position), Range(Range), Settings, TextDocumentEdit(..), TextDocumentIdentifier(TextDocumentIdentifier), TextEdit(..), workspaceEdit)
import PscIde.Command (PscSuggestion(..), PursIdeInfo(..), RebuildError(..))

getActions :: DocumentStore -> Settings -> ServerState -> CodeActionParams -> Aff (Array Command)
getActions documents settings (ServerState { diagnostics, conn: Just conn }) { textDocument, range } =
  case Object.lookup (un DocumentUri $ docUri) diagnostics of
    Just errs -> pure $
        (catMaybes $ map asCommand errs)
        <> fixAllCommand "Apply all suggestions" errs
        <> fixAllCommand "Apply all import suggestions" (filter (\(RebuildError { errorCode }) -> isImport errorCode) errs)
        <> mapMaybe commandForCode errs
    _ -> pure []
  where
    docUri = _.uri $ un TextDocumentIdentifier textDocument

    asCommand error@(RebuildError { position: Just position, errorCode })
      | Just { replacement, range: replaceRange } <- getReplacementRange error
      , contains (positionToRange position) range = do
      Just $ replaceSuggestion (getTitle errorCode) docUri replacement replaceRange
    asCommand _ = Nothing

    getReplacementRange (RebuildError { position: Just position, suggestion: Just (PscSuggestion { replacement, replaceRange }) }) =
      Just $ { replacement, range: range' }
      where
      range' = positionToRange $ fromMaybe position replaceRange
    getReplacementRange _ = Nothing

    fixAllCommand text rebuildErrors = if length replacements > 0 then [ replaceAllSuggestions text docUri replacements ] else [ ]
      where
      replacements = nubByEq eq $ mapMaybe getReplacementRange rebuildErrors

    commandForCode err@(RebuildError { position: Just position, errorCode }) | contains range (positionToRange position) =
      case errorCode of
        "ModuleNotFound" -> Just build
        "HoleInferredType" -> case err of
          RebuildError { pursIde: Just (PursIdeInfo { name, completions }) } ->
            Just $ typedHole name docUri (positionToRange position) completions
          _ -> Nothing
        x | isUnknownToken x
          , { startLine, startColumn } <- position -> Just $ fixTypo docUri (startLine-1) (startColumn-1)
        _ -> Nothing
    commandForCode _ = Nothing

    contains (Range { start, end }) (Range { start: start', end: end' }) = start <= start' && end >= end'
getActions _ _ _ _ = pure []

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

onReplaceSuggestion :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
onReplaceSuggestion docs config (ServerState { conn }) args =
  case conn, args of
    Just conn', [ uri', replacement', range' ]
      | Right uri <- runExcept $ readString uri'
      , Right replacement <- runExcept $ readString replacement'
      , Right range <- runExcept $ readRange range'
      -> do
        doc <- liftEffect $ getDocument docs (DocumentUri uri)
        version <- liftEffect $ getVersion doc
        TextEdit { range: range'', newText } <- getReplacementEdit doc { replacement, range }
        let edit = makeWorkspaceEdit (DocumentUri uri) version range'' newText

        -- TODO: Check original & expected text ?
        void $ applyEdit conn' edit
    _, _ -> pure unit


getReplacementEdit :: TextDocument -> Replacement -> Aff TextEdit
getReplacementEdit doc { replacement, range } = do
  origText <- liftEffect $ getTextAtRange doc range
  afterText <- liftEffect $ replace' (regex "\n$" noFlags) "" <$> getTextAtRange doc (afterEnd range)

  let newText = getReplacement replacement afterText

  let range' = if newText == "" && afterText == "" then
                toNextLine range
                else
                range
  pure $ TextEdit { range: range', newText }
  where
    -- | Modify suggestion replacement text, removing extraneous newlines
    getReplacement :: String -> String -> String
    getReplacement replacement' extraText =
      (trim $ replace' (regex "\\s+\n" global) "\n" replacement')
      <> if addNewline then "\n" else ""
      where
      trailingNewline = test' (regex "\n\\s+$" noFlags) replacement'
      addNewline = trailingNewline && (not $ null extraText)

onReplaceAllSuggestions :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
onReplaceAllSuggestions docs config (ServerState { conn }) args =
  case conn, args of
    Just conn', [ uri', suggestions' ]
      | Right uri <- runExcept $ readString uri'
      , Right suggestions <- runExcept $ readArray suggestions' >>= traverse readSuggestion
      -> do
          doc <- liftEffect $ getDocument docs (DocumentUri uri)
          version <- liftEffect $ getVersion doc
          edits <- traverse (getReplacementEdit doc) suggestions
          void $ applyEdit conn' $ workspaceEdit
            [ TextDocumentEdit
              { textDocument: TextDocumentIdentifier { uri: DocumentUri uri, version }
              , edits
              }
            ]
    _, _ -> pure unit

readSuggestion :: Foreign -> F Replacement
readSuggestion o = do
  replacement <- o ! "replacement" >>= readString
  range <- o ! "range" >>= readRange
  pure $ { replacement, range }

