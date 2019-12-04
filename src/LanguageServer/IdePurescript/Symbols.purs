module LanguageServer.IdePurescript.Symbols where

import Prelude

import Data.Array (catMaybes, singleton)
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (over, un)
import Data.Nullable (toNullable, Nullable)
import Data.String (Pattern(..), contains)
import Data.String as Str
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getCompletion, getLoadedModules, getTypeInfo)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (TextDocumentPositionParams, WorkspaceSymbolParams, DocumentSymbolParams)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (DocumentStore, Location(..), Position(..), Range(..), Settings, SymbolInformation(..), SymbolKind(..), TextDocumentIdentifier(..), symbolKindToInt)
import LanguageServer.Uri (filenameToUri)
import Node.Path (resolve)
import PscIde.Command (CompletionOptions(..))
import PscIde.Command as Command

convPosition :: Command.Position -> Position
convPosition { line, column } = Position { line: line - 1, character: column - 1 }

convTypePosition :: Command.TypePosition -> Range
convTypePosition (Command.TypePosition {start, end}) = Range { start: convPosition start, end: convPosition end }

getDefinition :: DocumentStore -> Settings -> ServerState -> TextDocumentPositionParams
  -> Aff (Nullable Location)
getDefinition docs settings state ({ textDocument, position }) = do
    doc <- liftEffect $ getDocument docs (_.uri $ un TextDocumentIdentifier textDocument)
    text <- liftEffect $ getTextAtRange doc (mkRange position)
    let { port, modules, root } = un ServerState $ state
    case port, root, identifierAtPoint text (_.character $ un Position position) of
      Just port', Just root', Just { word, qualifier } -> do
        info <- getTypeInfo port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)
        liftEffect $ toNullable <$> case info of
          Just (Command.TypeInfo { definedAt: Just (Command.TypePosition { name, start }) }) -> do
            uri <- filenameToUri =<< resolve [ root' ] name
            let range = Range { start: convPosition start, end: convPosition start }
            pure $ Just $ Location { uri, range }
          _ -> pure $ Nothing
      _, _, _ -> pure $ toNullable Nothing
    where
    mkRange pos@(Position { line, character }) = Range
        { start: pos # over Position (_ { character = 0 })
        , end: pos # over Position (\c -> c { character = c.character + 100 })
        }

getDocumentSymbols :: Settings -> ServerState -> DocumentSymbolParams
  -> Aff (Array SymbolInformation)
getDocumentSymbols _ state _ = do 
  let { port, root, modules } = un ServerState state
  case port, root of
    Just port', Just root' -> getSymbols root' port' "" (maybe [] singleton modules.main)
    _, _ -> pure []

getWorkspaceSymbols :: Settings -> ServerState -> WorkspaceSymbolParams
  -> Aff (Array SymbolInformation)
getWorkspaceSymbols _ state { query } = do
  let { port, root } = un ServerState state
  case port, root of
    Just port', Just root'  -> do
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
  getInfo (Command.TypeInfo { identifier, definedAt: Just typePos, module', type' }) = do
    fileName <- getName typePos
    let kind = if Str.take 1 identifier == (Str.toUpper $ Str.take 1 identifier)
               then ClassSymbolKind
               else if contains (Pattern "->") identifier then
                  FunctionSymbolKind
               else PropertySymbolKind
    uri <- filenameToUri fileName
    pure $ Just $ SymbolInformation
      { name: identifier
      , kind: symbolKindToInt kind
      , location: Location
        { uri 
        , range: convTypePosition typePos
        }
      , containerName: toNullable $ Just $ module'
      }
  getInfo _ = pure Nothing

  getName (Command.TypePosition { name }) = resolve [ root ] name
