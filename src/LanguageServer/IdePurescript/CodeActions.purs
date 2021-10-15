module LanguageServer.IdePurescript.CodeActions where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, concat, filter, foldl, head, length, mapMaybe, nubByEq, singleton, sortWith, (:))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (F, Foreign, readArray, readString)
import Foreign.Index ((!))
import Foreign.Object as Object
import IdePurescript.QuickFix (getReplacement, getTitle, isImport, isUnknownToken)
import IdePurescript.Regex (replace')
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (CodeActionParams, applyEdit)
import LanguageServer.IdePurescript.Assist (fixTypoActions)
import LanguageServer.IdePurescript.Build (positionToRange)
import LanguageServer.IdePurescript.Commands (Replacement, build, replaceAllSuggestions, replaceSuggestion, typedHole)
import LanguageServer.IdePurescript.Commands as Commands
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Text (makeWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (TextDocument, getTextAtRange, getVersion)
import LanguageServer.Protocol.Types (ClientCapabilities, CodeAction(..), CodeActionKind, CodeActionResult, Command(..), DocumentStore, DocumentUri(DocumentUri), Position(Position), Range(Range), Settings, TextDocumentEdit(..), TextDocumentIdentifier(TextDocumentIdentifier), TextEdit(..), codeActionResult, codeActionSourceOrganizeImports, codeActionSourceSortImports, readRange, workspaceEdit)
import PscIde.Command (PscSuggestion(..), PursIdeInfo(..), RebuildError(..))

m :: forall a. Nullable a -> Maybe a
m = Nullable.toMaybe

codeActionLiteralsSupported :: ClientCapabilities -> Boolean
codeActionLiteralsSupported c =
  c
    # (_.textDocument >>> m)
    >>= (_.codeAction >>> m)
    >>= (_.codeActionLiteralSupport >>> m)
    # isJust

codeActionToCommand :: Maybe ClientCapabilities -> Either CodeAction Command -> Maybe CodeActionResult
codeActionToCommand capabilities action =
  codeActionResult
    <$> if supportsLiteral then
        Just action
      else
        either convert (Just <<< Right) action
  where
  supportsLiteral = maybe true codeActionLiteralsSupported capabilities
  convert (CodeAction { command }) | Just c <- m command = Just $ Right c
  convert _ = Nothing

getActions :: DocumentStore -> Settings -> ServerState -> CodeActionParams -> Aff (Array CodeActionResult)
getActions documents settings state@(ServerState { diagnostics, conn: Just conn, clientCapabilities }) { textDocument, range } =
  case Object.lookup (un DocumentUri $ docUri) diagnostics of
    Just errs ->
      mapMaybe (codeActionToCommand clientCapabilities)
        <$> do
            codeActions <- traverse commandForCode errs
            pure
              $ (map Right $ catMaybes $ map asCommand errs)
              <> (map Right $ fixAllCommand "Apply all suggestions" $ notImplicitPrelude errs)
              <> (organizeImports $ notImplicitPrelude errs)
              <> (map Right $ concat codeActions)
              <> sortImports
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

  notImplicitPrelude = filter (\(RebuildError { errorCode, message }) -> not (errorCode == "ImplicitImport" && String.contains (Pattern "Module Prelude") message))

  -- Sort/format imports via purs ide
  sortImports = [ Left $ commandAction codeActionSourceSortImports (Commands.sortImports docUri) ]

  -- Apply all import suggestions from the compiler
  -- TODO this should probably sort too!
  organizeImports errs =
    map (Left <<< commandAction codeActionSourceOrganizeImports)
      $ fixAllCommand "Organize Imports" (filter (\(RebuildError { errorCode }) -> isImport errorCode) errs)

  fixAllCommand text rebuildErrors = if length replacements > 0 then [ replaceAllSuggestions text docUri replacements ] else []
    where
    replacements :: Array { range :: Range, replacement :: String }
    replacements = removeOverlaps $ sortWith _.range $ nubByEq eq $ mapMaybe getReplacementRange rebuildErrors

  removeOverlaps :: Array { range :: Range, replacement :: String } -> Array { range :: Range, replacement :: String }
  removeOverlaps = foldl go []
    where
    go [] x = [ x ]
    go acc x@{ range: Range { start } }
      | Just ({ range: Range { end: lastEnd } }) <- head acc
      , lastEnd < start = x : acc
    go acc _ = acc

  commandForCode err@(RebuildError { position: Just position, errorCode })
    | intersects (positionToRange position) range =
      case errorCode of
        "ModuleNotFound" -> pure [ build ]
        "HoleInferredType" -> case err of
          RebuildError { pursIde: Just (PursIdeInfo { name, completions }) } ->
            pure $ singleton $ typedHole name docUri (positionToRange position) completions
          _ -> pure []
        x
          | isUnknownToken x
          , { startLine, startColumn } <- position ->
            fixTypoActions documents settings state docUri (startLine - 1) (startColumn - 1)
        _ -> pure []
  commandForCode _ = pure []

  intersects (Range { start, end }) (Range { start: start', end: end' }) = start <= end' && start' <= end

getActions _ _ _ _ = pure []

commandAction :: CodeActionKind -> Command -> CodeAction
commandAction kind c@(Command { title }) =
  CodeAction
    { title
    , kind
    , isPreferred: false
    , edit: Nullable.toNullable Nothing
    , command: Nullable.toNullable $ Just c
    }

-- codeActionSourceOrganizeImports
-- codeActionEmpty
afterEnd :: Range -> Range
afterEnd (Range { end: end@(Position { line, character }) }) =
  Range
    { start: end
    , end: Position { line, character: character + 10 }
    }

toNextLine :: Range -> Range
toNextLine (Range { start, end: Position { line } }) =
  Range
    { start
    , end: Position { line: line + 1, character: 0 }
    }

onReplaceSuggestion :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
onReplaceSuggestion docs _ (ServerState { conn, clientCapabilities }) args =
  case conn, args of
    Just conn', [ uri', replacement', range' ]
      | Right uri <- runExcept $ readString uri'
      , Right replacement <- runExcept $ readString replacement'
      , Right range <- runExcept $ readRange range' -> do
        doc <- liftEffect $ getDocument docs (DocumentUri uri)
        version <- liftEffect $ getVersion doc
        TextEdit { range: range'', newText } <- getReplacementEdit doc { replacement, range }
        let edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version range'' newText
        -- TODO: Check original & expected text ?
        void $ applyEdit conn' edit
    _, _ -> pure unit

getReplacementEdit :: TextDocument -> Replacement -> Aff TextEdit
getReplacementEdit doc { replacement, range } = do
  afterText <- liftEffect $ replace' (regex "\n$" noFlags) "" <$> getTextAtRange doc (afterEnd range)
  let newText = getReplacement replacement afterText
  let
    range' =
      if newText == "" && afterText == "" then
        toNextLine range
      else
        range
  pure $ TextEdit { range: range', newText }

onReplaceAllSuggestions :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
onReplaceAllSuggestions docs _ (ServerState { conn, clientCapabilities }) args =
  case conn, args of
    Just conn', [ uri', suggestions' ]
      | Right uri <- runExcept $ readString uri'
      , Right suggestions <- runExcept $ readArray suggestions' >>= traverse readSuggestion -> do
        doc <- liftEffect $ getDocument docs (DocumentUri uri)
        version <- liftEffect $ getVersion doc
        edits <- traverse (getReplacementEdit doc) suggestions
        void
          $ applyEdit conn'
          $ workspaceEdit clientCapabilities
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
