module LanguageServer.IdePurescript.CodeActions where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, concat, filter, foldl, head, length, mapMaybe, nubByEq, singleton, sortWith, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.String (null, trim)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (global, noFlags)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (F, Foreign, readArray, readString)
import Foreign.Index ((!))
import Foreign.Object as Object
import IdePurescript.QuickFix (getTitle, isImport, isUnknownToken)
import IdePurescript.Regex (replace', test')
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (CodeActionParams, applyEdit)
import LanguageServer.IdePurescript.Assist (fixTypoActions)
import LanguageServer.IdePurescript.Build (positionToRange)
import LanguageServer.IdePurescript.Commands (Replacement, build, replaceAllSuggestions, replaceSuggestion, typedHole)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Text (makeWorkspaceEdit)
import LanguageServer.TextDocument (TextDocument, getTextAtRange, getVersion)
import LanguageServer.Types (Command, DocumentStore, DocumentUri(DocumentUri), Position(Position), Range(Range), Settings, TextDocumentEdit(..), TextDocumentIdentifier(TextDocumentIdentifier), TextEdit(..), readRange, workspaceEdit)
import PscIde.Command (PscSuggestion(..), PursIdeInfo(..), RebuildError(..))

getActions :: DocumentStore -> Settings -> ServerState -> CodeActionParams -> Aff (Array Command)
getActions documents settings state@(ServerState { diagnostics, conn: Just conn }) { textDocument, range } =
  case Object.lookup (un DocumentUri $ docUri) diagnostics of
    Just errs -> do
      codeActions <- traverse commandForCode errs
      pure $
        (catMaybes $ map asCommand errs)
        <> fixAllCommand "Apply all suggestions" errs
        <> fixAllCommand "Apply all import suggestions" (filter (\(RebuildError { errorCode, position }) -> isImport errorCode && maybe false (\pos -> intersects (positionToRange pos) range) position) errs)
        <> concat codeActions
    _ -> pure []
  where
    docUri = _.uri $ un TextDocumentIdentifier textDocument

    asCommand error@(RebuildError { position: Just position, errorCode })
      | Just { replacement, range: replaceRange } <- getReplacementRange error
      , intersects (positionToRange position) range = do
      Just $ replaceSuggestion (getTitle errorCode) docUri replacement replaceRange
    asCommand _ = Nothing

    getReplacementRange (RebuildError { position: Just position, suggestion: Just (PscSuggestion { replacement, replaceRange }) }) =
      Just $ { replacement, range: range' }
      where
      range' = positionToRange $ fromMaybe position replaceRange
    getReplacementRange _ = Nothing

    fixAllCommand text rebuildErrors = if length replacements > 0 then [ replaceAllSuggestions text docUri replacements ] else [ ]
      where
      replacements :: Array { range :: Range, replacement :: String }
      replacements = removeOverlaps $ sortWith _.range $ nubByEq eq $ mapMaybe getReplacementRange rebuildErrors

    removeOverlaps :: Array { range :: Range, replacement :: String } -> Array { range :: Range, replacement :: String }
    removeOverlaps = foldl go []
      where
      go [] x = [x] 
      go acc x@{range: Range { start }} 
        | Just ({range: Range { end: lastEnd }}) <- head acc
        , lastEnd < start
        = x:acc
      go acc _ = acc

    commandForCode err@(RebuildError { position: Just position, errorCode }) | intersects (positionToRange position) range =
      case errorCode of
        "ModuleNotFound" -> pure [ build ]
        "HoleInferredType" -> case err of
          RebuildError { pursIde: Just (PursIdeInfo { name, completions }) } ->
            pure $ singleton $ typedHole name docUri (positionToRange position) completions
          _ -> pure []
        x | isUnknownToken x
          , { startLine, startColumn } <- position ->
            fixTypoActions documents settings state docUri (startLine-1) (startColumn-1)
            -- singleton $ fixTypo docUri (startLine-1) (startColumn-1)
        _ -> pure []
    commandForCode _ = pure []

    intersects (Range { start, end }) (Range { start: start', end: end' }) = start <= end' && start' <= end

getActions _ _ _ _ = pure []



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
onReplaceSuggestion docs config (ServerState { conn, clientCapabilities }) args =
  case conn, args of
    Just conn', [ uri', replacement', range' ]
      | Right uri <- runExcept $ readString uri'
      , Right replacement <- runExcept $ readString replacement'
      , Right range <- runExcept $ readRange range'
      -> do
        doc <- liftEffect $ getDocument docs (DocumentUri uri)
        version <- liftEffect $ getVersion doc
        TextEdit { range: range'', newText } <- getReplacementEdit doc { replacement, range }
        let edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version range'' newText

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
onReplaceAllSuggestions docs config (ServerState { conn, clientCapabilities }) args =
  case conn, args of
    Just conn', [ uri', suggestions' ]
      | Right uri <- runExcept $ readString uri'
      , Right suggestions <- runExcept $ readArray suggestions' >>= traverse readSuggestion
      -> do
          doc <- liftEffect $ getDocument docs (DocumentUri uri)
          version <- liftEffect $ getVersion doc
          edits <- traverse (getReplacementEdit doc) suggestions
          void $ applyEdit conn' $ workspaceEdit clientCapabilities
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

