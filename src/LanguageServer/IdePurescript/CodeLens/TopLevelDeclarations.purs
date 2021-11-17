module LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations( getDecls
  , topLevelDeclarationLenses
  )
where

import Prelude

import Data.Array (mapMaybe, mapWithIndex)
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.Set as Set
import Data.String (joinWith, trim)
import Data.String.Utils as String
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import IdePurescript.PscIde (typesInModule)
import LanguageServer.IdePurescript.Commands (cmdName, replaceSuggestionCmd)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util (maybeParseResult)
import LanguageServer.Protocol.Handlers (CodeLensResult)
import LanguageServer.Protocol.Types (Command(..), DocumentStore, DocumentUri, Position(..), Range(..), Settings)
import PscIde.Command (TypeInfo(..))
import PureScript.CST.Types (Declaration(..), Ident(..), Labeled(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..))

-- TODO force code lens refresh on server load, full build, consider rebuild even if it is meant to be "global"
topLevelDeclarationLenses ∷ DocumentStore -> Settings -> ServerState -> DocumentUri -> Aff (Array CodeLensResult)
topLevelDeclarationLenses _docs _settings (ServerState { port, parsedModules }) uri = do
  let binds = maybeParseResult Nothing (Just <<< getDecls) =<< _.parsed <$> Map.lookup uri parsedModules
  case port, binds of
    Just port', Just { moduleName, decls } -> do
      types <- typesInModule port' moduleName
      pure $ Array.mapMaybe (mkCodeLensResult types) decls

    _, _ -> pure []
  where

  mkCodeLensResult types { name, range: { start, end } } = do
    let
      lookupType = Array.findMap (\(TypeInfo { identifier, type' }) -> if identifier == name then Just type' else Nothing) types
      range =
        Range
          { start: Position { line: start.line, character: 0 }
          , end: Position { line: end.line, character: 0 }
          }
    ( \ty ->
        { range
        , command: Nullable.notNull (mkReplaceCommand name ty range)
        , data: (Nullable.null # unsafeToForeign)
        }
    )
      <$> lookupType

  mkReplaceCommand name ty range =
    let
      signature = name <> " ∷ " <> trim ty
    in
      Command
        { command: cmdName replaceSuggestionCmd
        , title: signature
        , arguments:
            Nullable.notNull
              [ unsafeToForeign uri
              , unsafeToForeign (ensureSpaceAfterFirstLine signature <> "\n")
              , unsafeToForeign range
              ]
        }


getDecls :: forall a. Module a -> { moduleName :: String, decls :: Array _ }
getDecls (Module { header: ModuleHeader { name: Name { name: ModuleName moduleName } }, body: ModuleBody { decls } }) =
  { moduleName, decls: mapMaybe go decls }
  where
  signatures = Set.fromFoldable $ Array.mapMaybe sig decls
  sig = case _ of
    DeclSignature (Labeled { label: Name { name: Ident name } }) -> Just name
    _ -> Nothing
  go = case _ of
    DeclValue { name: Name { name: Ident name, token } } | not (name `Set.member` signatures) -> Just { name, range: token.range }
    _ -> Nothing

ensureSpaceAfterFirstLine ∷ String -> String
ensureSpaceAfterFirstLine = String.lines >>> mapWithIndex prependSpaceIfNecessary >>> joinWith "\n"
  where
  prependSpaceIfNecessary i s = if i == 0 || String.startsWith " " s then s else " " <> s
