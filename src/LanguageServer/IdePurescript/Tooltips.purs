module LanguageServer.IdePurescript.Tooltips
  ( getTooltips
  ) where

import Prelude

import Data.Array (uncons)
import Data.Foldable (foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getTypeInfo, getTypeInfoWithImportFilter)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify, Version(..), parseVersion)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util (maybeParseResult)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (TextDocumentPositionParams)
import LanguageServer.Protocol.TextDocument (getTextAtRange)
import LanguageServer.Protocol.Types (DocumentStore, Hover(Hover), Position(Position), Range(Range), Settings, TextDocumentIdentifier(TextDocumentIdentifier), markupContent)
import Literals.Undefined (undefined)
import PscIde.Command as C
import PscIde.Server (Executable(..))
import PureScript.CST.Print as CST.Print
import PureScript.CST.Range (class TokensOf) as CST
import PureScript.CST.Range (tokensOf)
import PureScript.CST.Range.TokenList as TokenList
import PureScript.CST.Types (Module(..), ModuleHeader(..)) as CST
import Untagged.Union (asOneOf)

dependencyFilterAvailable :: ServerState -> Boolean
dependencyFilterAvailable (ServerState { purs }) = case purs of
  Just (Executable _bin (Just vStr)) | Just version <- parseVersion vStr ->
    version >= Version 0 15 7
  _ -> false

getTooltips :: Notify -> DocumentStore -> Settings -> ServerState -> TextDocumentPositionParams -> Aff (Nullable Hover)
getTooltips notify docs _ state ({ textDocument, position }) = toNullable <$> do
  let uri = (_.uri $ un TextDocumentIdentifier textDocument)
  maybeDoc <- liftEffect $ getDocument docs uri
  case Nullable.toMaybe maybeDoc of
    Nothing -> pure Nothing
    Just doc -> do
      text <- liftEffect $ getTextAtRange doc $ lineRange position
      let
        { port, modules, parsedModules } = un ServerState state
        char = _.character $ un Position $ position
      case port, identifierAtPoint text char of
        Just port', Just { word, qualifier, range: range@{ left } } -> do
          case qualifier of
            Just q
              | char < left + String.length q -> do
                  let mod = getQualModule q (un ServerState state).modules
                  pure
                    $ case uncons mod of
                        Just { head } ->
                          Just
                            $ Hover
                                { contents: markupContent head
                                , range: asOneOf $ wordRange position range
                                    { right = left + String.length q }
                                }
                        _ -> Nothing
            _ -> do
              map (convertInfo word) <$>
                if dependencyFilterAvailable state then do
                  imports <- getImports uri parsedModules
                  getTypeInfoWithImportFilter port' word modules.main qualifier imports
                else
                  getTypeInfo port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)

        _, _ -> pure Nothing
  where
  
  getImports uri parsedModules = 
    case Map.lookup uri parsedModules of
      Just { parsed } -> 
        pure $ maybeParseResult [] parseImports parsed
      Nothing -> do
        liftEffect $ notify Warning $ "tooltips - no parsed CST for " <> show uri
        pure []

  parseImports :: forall a. CST.TokensOf a => CST.Module a -> Array String
  parseImports (CST.Module { header: CST.ModuleHeader { imports } }) =
    let
      printImport imp =
        String.trim $ foldMap CST.Print.printSourceToken (TokenList.toArray (tokensOf imp))
    in
      printImport <$> imports

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
