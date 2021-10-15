module LanguageServer.IdePurescript.Symbols where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (catMaybes, singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (over, un)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..), contains)
import Data.String as Str
import Data.String as String
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getCompletion, getLoadedModules, getModuleInfo, getTypeInfo)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (TextDocumentPositionParams, WorkspaceSymbolParams, DocumentSymbolParams)
import LanguageServer.Protocol.TextDocument (getTextAtRange)
import LanguageServer.Protocol.Types (ClientCapabilities, DocumentStore, GotoDefinitionResult, Location(..), LocationLink(..), Position(..), Range(..), Settings, SymbolInformation(..), SymbolKind(..), TextDocumentIdentifier(..), gotoDefinitionResult, symbolKindToInt)
import LanguageServer.Protocol.Uri (filenameToUri)
import Node.Path (resolve)
import PscIde.Command (CompletionOptions(..))
import PscIde.Command as Command

convPosition :: Command.Position -> Position
convPosition { line, column } = Position { line: line - 1, character: column - 1 }

convTypePosition :: Command.TypePosition -> Range
convTypePosition (Command.TypePosition { start, end }) = Range { start: convPosition start, end: convPosition end }

getDefinition ::
  DocumentStore ->
  Settings ->
  ServerState ->
  TextDocumentPositionParams ->
  Aff (Nullable GotoDefinitionResult)
getDefinition docs _ state@(ServerState { conn: Just conn, clientCapabilities: Just clientCapabilities }) ({ textDocument, position }) = do
  doc <- liftEffect $ getDocument docs (_.uri $ un TextDocumentIdentifier textDocument)
  text <- liftEffect $ getTextAtRange doc (mkLineRange position)
  let { port, modules, root } = un ServerState $ state
  case port, root, identifierAtPoint text (_.character $ un Position position) of
    Just port', Just root', Just identRes@{ word, qualifier, range } -> do
      info <- lift2 (<|>) (moduleInfo port' identRes range) (typeInfo port' modules identRes)
      liftEffect
        $ toNullable
        <$> case info of
            Just { typePos: Command.TypePosition { name, start }, originRange } -> do
              uri <- filenameToUri =<< resolve [ root' ] name
              let startRange = Range { start: convPosition start, end: convPosition start }
              pure $ Just $ mkResult uri originRange startRange
            Nothing -> pure Nothing
    _, _, _ -> pure $ toNullable Nothing
  where

  moduleInfo port' { word, qualifier, range } { right } = do
    let
      fullModule = case qualifier of
        Just q -> q <> "." <> word
        Nothing -> word
    let left = right - (String.length fullModule)
    info <- getModuleInfo port' fullModule
    pure
      $ case info of
          Just (Command.TypeInfo { definedAt: Just typePos }) ->
            Just { typePos, originRange: Just $ mkNewRange position left right }
          _ -> Nothing
  typeInfo port' modules { word, qualifier, range } = do
    info <- getTypeInfo port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)
    pure
      $ case info of
          Just (Command.TypeInfo { definedAt: Just typePos }) -> Just { typePos, originRange: Nothing }
          _ -> Nothing

  mkLineRange pos =
    Range
      { start: pos # over Position (_ { character = 0 })
      , end: pos # over Position (\c -> c { character = c.character + 100 })
      }
  mkNewRange pos left right =
    Range { start: over Position (_ { character = left }) pos, end: over Position (_ { character = right }) pos }
  mkResult uri (Just sourceRange) range =
    if locationLinkSupported clientCapabilities then
      gotoDefinitionResult
        $ Right
        $ LocationLink
            { originSelectionRange: toNullable $ Just sourceRange
            , targetRange:
                over Range
                  ( \rr ->
                      rr
                        { start = over Position (\pp -> pp { character = 0 }) rr.start
                        , end = over Position (\pp -> pp { line = pp.line + 1 }) rr.end
                        }
                  )
                  range
            , targetSelectionRange:
                over Range
                  ( \rr ->
                      rr
                        { start = over Position (\pp -> pp { character = 0 }) rr.start
                        , end = over Position (\pp -> pp { line = pp.line + 1 }) rr.end
                        }
                  )
                  range
            , targetUri: uri
            }
    else
      gotoDefinitionResult $ Left $ Location { uri, range }
  mkResult uri Nothing range = gotoDefinitionResult $ Left $ Location { uri, range }

  locationLinkSupported :: ClientCapabilities -> Boolean
  locationLinkSupported c = c # (_.textDocument >>> m) >>= (_.definition >>> m) >>= (_.linkSupport >>> m) # isJust

  m :: forall a. Nullable a -> Maybe a
  m = Nullable.toMaybe

getDefinition _ _ _ _ = pure $ toNullable Nothing

getDocumentSymbols ::
  Settings ->
  ServerState ->
  DocumentSymbolParams ->
  Aff (Array SymbolInformation)
getDocumentSymbols _ state _ = do
  let { port, root, modules } = un ServerState state
  case port, root of
    Just port', Just root' -> getSymbols root' port' "" (maybe [] singleton modules.main)
    _, _ -> pure []

getWorkspaceSymbols ::
  Settings ->
  ServerState ->
  WorkspaceSymbolParams ->
  Aff (Array SymbolInformation)
getWorkspaceSymbols _ state { query } = do
  let { port, root } = un ServerState state
  case port, root of
    Just port', Just root' -> do
      allModules <- getLoadedModules port'
      getSymbols root' port' query allModules
    _, _ -> pure []

getSymbols :: String -> Int -> String -> Array String -> Aff (Array SymbolInformation)
getSymbols root port prefix modules = do
  let opts = CompletionOptions { maxResults: Nothing, groupReexports: true }
  completions <- getCompletion port prefix Nothing Nothing modules (const []) opts
  res <- liftEffect $ traverse getInfo completions
  pure $ catMaybes res

  where
  getInfo (Command.TypeInfo { identifier, definedAt: Just typePos, module' }) = do
    fileName <- getName typePos
    let
      kind =
        if Str.take 1 identifier == (Str.toUpper $ Str.take 1 identifier) then
          ClassSymbolKind
        else if contains (Pattern "->") identifier then
          FunctionSymbolKind
        else
          PropertySymbolKind
    uri <- filenameToUri fileName
    pure
      $ Just
      $ SymbolInformation
          { name: identifier
          , kind: symbolKindToInt kind
          , location:
              Location
                { uri
                , range: convTypePosition typePos
                }
          , containerName: toNullable $ Just $ module'
          }
  getInfo _ = pure Nothing

  getName (Command.TypePosition { name }) = resolve [ root ] name
