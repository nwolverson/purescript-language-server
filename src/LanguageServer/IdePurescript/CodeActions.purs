module LanguageServer.IdePurescript.CodeActions
  ( getActions
  , onReplaceAllSuggestions
  , onReplaceSuggestion
  )
  where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, concat, filter, foldl, head, length, mapMaybe, nubByEq, singleton, sortWith, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (un)
import Data.Nullable as Nullable
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (any, for_, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (F, Foreign, readArray, readString)
import Foreign.Index ((!))
import Foreign.Object as Object
import IdePurescript.QuickFix (getTitle, isImport, isUnknownToken, wildcardInferredType)
import IdePurescript.Regex (replace')
import LanguageServer.IdePurescript.Assist (fixTypoActions)
import LanguageServer.IdePurescript.Build (positionToRange)
import LanguageServer.IdePurescript.Commands (Replacement, build, replaceAllSuggestions, replaceSuggestion, typedHole)
import LanguageServer.IdePurescript.Commands as Commands
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (CodeActionParams, applyEdit)
import LanguageServer.Protocol.Text (makeWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (TextDocument, getTextAtRange, getVersion)
import LanguageServer.Protocol.TextDocument (TextDocument, getTextAtRange, getVersion)
import LanguageServer.Protocol.Types (ClientCapabilities, CodeAction(..), CodeActionKind(..), CodeActionResult, Command(..), DocumentStore, DocumentUri(DocumentUri), OptionalVersionedTextDocumentIdentifier(..), Position(Position), Range(Range), Settings, TextDocumentEdit(..), TextDocumentIdentifier(TextDocumentIdentifier), TextEdit(..), codeActionEmpty, codeActionResult, codeActionSourceOrganizeImports, codeActionSourceSortImports, readRange, workspaceEdit)
import LanguageServer.Protocol.Types (ClientCapabilities, CodeAction(..), CodeActionKind(..), CodeActionResult, Command(..), DocumentStore, DocumentUri(DocumentUri), OptionalVersionedTextDocumentIdentifier(..), Position(Position), Range(Range), Settings, TextDocumentEdit(..), TextDocumentIdentifier(TextDocumentIdentifier), TextEdit(..), codeActionEmpty, codeActionResult, codeActionSourceOrganizeImports, codeActionSourceSortImports, readRange, workspaceEdit)
import PscIde.Command (PscSuggestion(..), PursIdeInfo(..), RebuildError(..))
import PscIde.Command (PscSuggestion(..), PursIdeInfo(..), RebuildError(..))

getActions :: DocumentStore -> Settings -> ServerState -> CodeActionParams -> Aff (Array CodeActionResult)
getActions documents settings state@(ServerState { diagnostics, conn: Just _conn, clientCapabilities }) { textDocument, range, context: { only }  } =
  case Object.lookup (un DocumentUri $ docUri) diagnostics of
    Just errs ->
      mapMaybe (codeActionToCommand clientCapabilities)
        <$> do
            codeActions :: Array (Array CodeAction) <- traverse commandForCode errs
            let actions =
                    (catMaybes $ map asCommand errs)
                    <> (commandAction_ <$> (fixAllCommand "Apply all suggestions" $ notImplicitPrelude errs))
                    <> (organizeImports $ notImplicitPrelude errs)
                    <> concat codeActions
                    <> sortImports
            pure $ filterKind actions
    _ -> pure []
  where
  docUri = _.uri $ un TextDocumentIdentifier textDocument

  filterKind :: Array CodeAction -> Array CodeAction
  filterKind actions =
    case Nullable.toMaybe only of
      Nothing -> actions
      Just kinds -> Array.filter (\(CodeAction { kind }) -> any (kind `isSubKindOf` _) kinds) actions

    where
    -- Sub kind, eg source.organizeImports is a sub-kind of source
    isSubKindOf (CodeActionKind k1) (CodeActionKind k2) = String.indexOf (Pattern k2) k1 == Just 0

  asCommand :: _ -> Maybe CodeAction
  asCommand error@(RebuildError { position: Just position, errorCode })
    | Just { replacement, range: replaceRange } <- getReplacementRange error
    , intersects (positionToRange position) range = do
      let replacement' = replace' (regex "\\s*\\n\\s*$" RegexFlags.global) "\n" replacement
          replacement'' = if errorCode == wildcardInferredType then replace' (regex "\\n\\s*$" RegexFlags.noFlags) "" replacement' else replacement'
      Just $ commandAction_ $ replaceSuggestion (getTitle errorCode) docUri replacement'' replaceRange
  asCommand _ = Nothing

  getReplacementRange (RebuildError { position: Just position, suggestion: Just (PscSuggestion { replacement, replaceRange }) }) =
    Just $ { replacement, range: range' }
    where
    range' = positionToRange $ fromMaybe position replaceRange
  getReplacementRange _ = Nothing

  notImplicitPrelude = filter (\(RebuildError { errorCode, message }) -> not (errorCode == "ImplicitImport" && String.contains (Pattern "Module Prelude") message))

  -- Sort/format imports via purs ide
  sortImports = [ commandAction codeActionSourceSortImports (Commands.sortImports docUri) ]

  -- Apply all import suggestions from the compiler
  -- TODO this should probably sort too!
  organizeImports :: _ -> Array CodeAction
  organizeImports errs =
    map (commandAction codeActionSourceOrganizeImports)
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

  commandForCode :: _ -> Aff (Array CodeAction)
  commandForCode err@(RebuildError { position: Just position, errorCode })
    | intersects (positionToRange position) range =
      case errorCode of
        "ModuleNotFound" -> pure [ commandAction_  build ]
        "HoleInferredType" -> case err of
          RebuildError { pursIde: Just (PursIdeInfo { name, completions }) } ->
            pure $ singleton $ commandAction_ $ typedHole name docUri (positionToRange position) completions
          _ -> pure []
        x
          | isUnknownToken x
          , { startLine, startColumn } <- position ->
            map commandAction_ <$> fixTypoActions documents settings state docUri (startLine - 1) (startColumn - 1)
        _ -> pure []
  commandForCode _ = pure []

  intersects (Range { start, end }) (Range { start: start', end: end' }) = start <= end' && start' <= end

getActions _ _ _ _ = pure []

codeActionLiteralsSupported :: ClientCapabilities -> Boolean
codeActionLiteralsSupported capabilities =
  capabilities
    # (_.textDocument >>> Nullable.toMaybe)
    >>= (_.codeAction >>> Nullable.toMaybe)
    >>= (_.codeActionLiteralSupport >>> Nullable.toMaybe)
    # isJust

codeActionToCommand :: Maybe ClientCapabilities -> CodeAction -> Maybe CodeActionResult
codeActionToCommand capabilities action =
  codeActionResult
    <$> if supportsLiteral then
        Just $ Left action
      else
        convert action
  where
  supportsLiteral = maybe true codeActionLiteralsSupported capabilities
  convert (CodeAction { command }) | Just c <- Nullable.toMaybe command = Just $ Right c
  convert _ = Nothing


commandAction :: CodeActionKind -> Command -> CodeAction
commandAction kind c@(Command { title }) =
  CodeAction
    { title
    , kind
    , isPreferred: false
    , edit: Nullable.toNullable Nothing
    , command: Nullable.toNullable $ Just c
    }

commandAction_ :: Command -> CodeAction
commandAction_ = commandAction codeActionEmpty

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
        for_ (Nullable.toMaybe doc) \doc -> do 
          version <- liftEffect $ getVersion doc
          TextEdit { range: range'', newText } <- getReplacementEdit doc { replacement, range }
          let edit = makeWorkspaceEdit clientCapabilities (DocumentUri uri) version range'' newText
          -- TODO: Check original & expected text ?
          void $ applyEdit conn' edit
    _, _ -> pure unit

getReplacementEdit :: TextDocument -> Replacement -> Aff TextEdit
getReplacementEdit doc { replacement, range } = do
  afterText <- liftEffect $ replace' (regex "\n$" noFlags) "" <$> getTextAtRange doc (afterEnd range)
  let 
  -- trim spaces at the end of lines
    replacementTrimSpaces =  replace' (regex "\\s+\n" RegexFlags.global) "\n" replacement
  -- remove trailing newline (inserting changes midway through line - eg _ fix)
    removeTrailingNewline = not $ String.null afterText
    newText = if removeTrailingNewline then replace' (regex "\\s+\n$" RegexFlags.noFlags) "" replacementTrimSpaces else replacementTrimSpaces
    range' =
      -- deletion (eg import line)
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
        for_ (Nullable.toMaybe doc) \doc -> do 
          version <- liftEffect $ getVersion doc
          edits <- traverse (getReplacementEdit doc) suggestions
          void
            $ applyEdit conn'
            $ workspaceEdit clientCapabilities
                [ TextDocumentEdit
                    { textDocument: OptionalVersionedTextDocumentIdentifier { uri: DocumentUri uri, version: Nullable.notNull version }
                    , edits
                    }
                ]
    _, _ -> pure unit

readSuggestion :: Foreign -> F Replacement
readSuggestion o = do
  replacement <- o ! "replacement" >>= readString
  range <- o ! "range" >>= readRange
  pure $ { replacement, range }
