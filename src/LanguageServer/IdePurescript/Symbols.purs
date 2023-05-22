module LanguageServer.IdePurescript.Symbols
  ( convPosition
  , getDefinition
  , getDocumentSymbols
  , getSymbols
  , getWorkspaceSymbols
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Array (catMaybes, foldr, singleton, (:))
import Data.Array as Array
import Data.Array.NonEmpty (foldl1)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Newtype (over, un)
import Data.Nullable (Nullable, toNullable)
import Data.Nullable as Nullable
import Data.Set (Set)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.String as Str
import Data.String as String
import Data.Traversable (foldMap, for, traverse)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getCompletion, getLoadedModules, getModuleInfo, getTypeInfo)
import IdePurescript.PscIdeServer (Notify)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.CST (sourceRangeToRange)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (DocumentSymbolParams, TextDocumentPositionParams, WorkspaceSymbolParams)
import LanguageServer.Protocol.TextDocument (getTextAtRange)
import LanguageServer.Protocol.Types (ClientCapabilities, DocumentStore, DocumentUri, GotoDefinitionResult, Location(..), LocationLink(..), Position(..), Range(..), Settings, SymbolInformation(..), SymbolKind(..), TextDocumentIdentifier(..), gotoDefinitionResult, symbolKindToInt)
import LanguageServer.Protocol.Uri (filenameToUri)
import Node.Path (resolve)
import PscIde.Command (CompletionOptions(..))
import PscIde.Command as Command
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST.Range (class RangeOf, rangeOf)
import PureScript.CST.Traversal as CSTTraversals
import PureScript.CST.Types (Declaration(..), DoStatement(..), Expr(..), LetBinding, Separated(..), SourceRange)
import PureScript.CST.Types as CST

convPosition :: Command.Position -> Position
convPosition { line, column } = Position
  { line: line - 1, character: column - 1 }

convTypePosition :: Command.TypePosition -> Range
convTypePosition (Command.TypePosition { start, end }) = Range
  { start: convPosition start, end: convPosition end }

getDefinition ::
  Notify ->
  DocumentStore ->
  Settings ->
  ServerState ->
  TextDocumentPositionParams ->
  Aff (Nullable GotoDefinitionResult)
getDefinition
  _notify
  docs
  _
  state@
    (ServerState { clientCapabilities: Just clientCapabilities, parsedModules })
  ({ textDocument, position }) =
  toNullable
    <$> do
      let uri = _.uri $ un TextDocumentIdentifier textDocument
      maybeDoc <- liftEffect $ getDocument docs uri
      case Nullable.toMaybe maybeDoc of
        Nothing -> pure Nothing
        Just doc -> do
          text <- liftEffect $ getTextAtRange doc (mkLineRange position)
          let { port, modules, root } = un ServerState $ state
          case
            port,
            root,
            identifierAtPoint text (_.character $ un Position position)
            of
            Just port', Just root', Just identRes@{ range, qualifier, word } ->
              do
                localDefn uri word
                  >>= case _ of
                    Just res | isNothing qualifier -> pure $ Just res
                    _ -> do
                      info <- lift2 (<|>) (moduleInfo port' identRes range)
                        (typeInfo port' modules identRes)
                      for info
                        \{ typePos: Command.TypePosition { name, start }
                         , originRange
                         } -> do
                          defnUri <- liftEffect $ filenameToUri =<< resolve
                            [ root' ]
                            name
                          let
                            startRange = Range
                              { start: convPosition start
                              , end: convPosition start
                              }
                          pure $ mkResult defnUri originRange startRange
            _, _, _ -> pure Nothing
  where
  localDefn uri ident = case _.parsed <$> Map.lookup uri parsedModules of
    Just (ParseSucceeded parsedModule) -> getLocalDefinitions uri position ident
      parsedModule
    Just (ParseSucceededWithErrors parsedModule _) -> getLocalDefinitions uri
      position
      ident
      parsedModule
    _ -> pure Nothing

  moduleInfo port' { word, qualifier } { right } = do
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
  typeInfo port' modules { word, qualifier } = do
    info <- getTypeInfo port' word modules.main qualifier
      (getUnqualActiveModules modules $ Just word)
      (flip getQualModule modules)
    pure
      $ case info of
          Just (Command.TypeInfo { definedAt: Just typePos }) -> Just
            { typePos, originRange: Nothing }
          _ -> Nothing

  mkLineRange pos =
    Range
      { start: pos # over Position (_ { character = 0 })
      , end: pos # over Position (\c -> c { character = c.character + 100 })
      }
  mkNewRange pos left right =
    Range
      { start: over Position (_ { character = left }) pos
      , end: over Position (_ { character = right }) pos
      }
  mkResult uri (Just sourceRange) range =
    if locationLinkSupported clientCapabilities then
      let
        targetRange =
          over Range
            ( \rr ->
                rr
                  { start = over Position (\pp -> pp { character = 0 }) rr.start
                  , end = over Position (\pp -> pp { line = pp.line + 1 })
                      rr.end
                  }
            )
            range
      in
        gotoDefinitionResult
          $ Right
          $ LocationLink
              { originSelectionRange: toNullable $ Just sourceRange
              , targetRange
              , targetSelectionRange: targetRange
              , targetUri: uri
              }
    else
      gotoDefinitionResult $ Left $ Location { uri, range }
  mkResult uri Nothing range = gotoDefinitionResult $ Left $ Location
    { uri, range }

  locationLinkSupported :: ClientCapabilities -> Boolean
  locationLinkSupported c = c # (_.textDocument >>> m) >>= (_.definition >>> m)
    >>= (_.linkSupport >>> m)
    # isJust

  m :: forall a. Nullable a -> Maybe a
  m = Nullable.toMaybe
getDefinition _ _ _ _ _ = pure $ toNullable Nothing

guard' :: forall t. Monoid t => Boolean -> (Unit -> t) -> t
guard' cond f = if cond then f unit else mempty

type IdentRange = { ident :: SourceRange, scope :: SourceRange }

newtype DoStatementsScope err = DoStatementsScope
  (NEA.NonEmptyArray (Either (Expr err) (DoStatement err)))

instance rangeOfDoStatementsScope ::
  RangeOf err =>
  RangeOf (DoStatementsScope err) where
  rangeOf (DoStatementsScope stmts) =
    foldl1 (\a b -> { start: a.start, end: b.end }) $ either rangeOf rangeOf <$>
      stmts

getLocalDefinitions ::
  forall err.
  RangeOf err =>
  DocumentUri ->
  Position ->
  String ->
  CST.Module err ->
  Aff (Maybe GotoDefinitionResult)
getLocalDefinitions uri position ident mod =
  toRes
    $
      ( CSTTraversals.foldMapModule
          $ CSTTraversals.defaultMonoidalVisitor
              { onExpr =
                  \e -> case e of
                    ExprLet { bindings } -> foldMap (onLetBind e) bindings
                    ExprLambda { binders, body } -> foldMap (onBinder body)
                      binders
                    ExprCase { branches } -> foldMap
                      ( \(Tuple (Separated { head, tail }) guarded) -> foldMap
                          (onBinder guarded)
                          (head : map snd tail)
                      )
                      branches
                    ExprDo { statements } -> _.res
                      $ foldr onStmt { scope: [], res: Set.empty }
                      $ NEA.toArray statements
                    ExprAdo { statements, result } -> _.res $ foldr onStmt
                      { scope: [ Left result ], res: Set.empty }
                      statements
                    _ -> Set.empty
              , onDecl =
                  \d ->
                    let
                      onMod x token = guard' (x == ident)
                        ( \_ -> Set.singleton
                            { ident: token.range, scope: rangeOf mod }
                        )
                    in
                      case d of
                        DeclValue
                          { name: CST.Name { name: CST.Ident x, token }
                          , binders
                          } ->
                          onMod x token <> foldMap (onBinder d) binders
                        DeclData
                          { name: CST.Name { token, name: CST.Proper name } }
                          _ -> onMod name token
                        DeclNewtype
                          { name: CST.Name { token, name: CST.Proper name } }
                          _
                          _
                          _ -> onMod name token
                        DeclClass
                          { name: CST.Name { token, name: CST.Proper name } }
                          _ -> onMod name token
                        DeclType
                          { name: CST.Name { token, name: CST.Proper name } }
                          _
                          _ -> onMod name token
                        DeclFixity
                          { operator: CST.FixityValue _ _
                              (CST.Name { token, name: CST.Operator name })
                          } -> onMod name token
                        DeclFixity
                          { operator: CST.FixityType _ _ _
                              (CST.Name { token, name: CST.Operator name })
                          } -> onMod name token
                        DeclForeign _ _
                          ( CST.ForeignValue
                              ( CST.Labeled
                                  { label: CST.Name
                                      { token, name: CST.Ident name }
                                  }
                              )
                          ) -> onMod name token
                        DeclForeign _ _
                          ( CST.ForeignData _
                              ( CST.Labeled
                                  { label: CST.Name
                                      { token, name: CST.Proper name }
                                  }
                              )
                          ) -> onMod name token
                        DeclForeign _ _
                          ( CST.ForeignKind _
                              (CST.Name { token, name: CST.Proper name })
                          ) -> onMod name token
                        _ -> Set.empty
              }
      )
        mod
  where

  onLetBind :: forall e. RangeOf e => e -> LetBinding err -> Set IdentRange
  onLetBind eScope = case _ of
    CST.LetBindingName
      { name: CST.Name { name: CST.Ident x, token }, binders, guarded } ->
      -- TODO scope is not fully e if patterns binds split the block
      guard' (x == ident)
        (\_ -> Set.singleton { ident: token.range, scope: rangeOf eScope })
        <> foldMap (onBinder guarded) binders
    CST.LetBindingPattern binder _ _ewhere -> onBinder eScope binder -- TODO should be bound in rest of bindings only

    _ -> Set.empty

  onStmt ::
    DoStatement err ->
    { res :: Set IdentRange
    , scope :: Array (Either (Expr err) (DoStatement err))
    } ->
    { res :: Set IdentRange
    , scope :: Array (Either (Expr err) (DoStatement err))
    }
  onStmt stmt acc = case stmt of
    DoLet _ letBinds ->
      let
        res' =
          maybe Set.empty
            ( \scopeStmts -> foldMap (onLetBind (DoStatementsScope scopeStmts))
                letBinds
            ) $ NEA.fromArray acc.scope
      in
        { res: acc.res <> res', scope: (Right stmt) : acc.scope }
    DoBind binder _ _ ->
      let
        res' =
          maybe Set.empty
            (\scopeStmts -> onBinder (DoStatementsScope scopeStmts) binder) $
            NEA.fromArray acc.scope
      in
        { res: acc.res <> res', scope: (Right stmt) : acc.scope }
    _ -> acc { scope = (Right stmt) : acc.scope }

  onBinder :: forall e. RangeOf e => e -> CST.Binder err -> Set IdentRange
  onBinder eScope =
    CSTTraversals.foldMapBinder
      $ CSTTraversals.defaultMonoidalVisitor
          { onBinder =
              case _ of
                CST.BinderVar (CST.Name { name: CST.Ident x, token })
                  | x == ident -> Set.singleton
                      { ident: token.range, scope: rangeOf eScope }
                CST.BinderNamed (CST.Name { name: CST.Ident x, token }) _ _
                  | x == ident -> Set.singleton
                      { ident: token.range, scope: rangeOf eScope }

                _ -> Set.empty
          }

  (Position { line: targetLine, character: targetChar }) = position
  containsPosition
    { start: { line: startLine, column: startCol }
    , end: { line: endLine, column: endCol }
    } =
    let
      target = Tuple targetLine targetChar
    in
      target >= Tuple startLine startCol && target <= Tuple endLine endCol

  toRes :: Set IdentRange -> Aff (Maybe GotoDefinitionResult)
  toRes ranges = do
    let
      validRanges = Array.filter (containsPosition <<< _.scope) $
        Array.fromFoldable ranges
    pure $ (res <<< _.ident) <$> Array.last validRanges

  res :: SourceRange -> GotoDefinitionResult
  res range = gotoDefinitionResult $ Left $ Location
    { uri, range: sourceRangeToRange range }

getDocumentSymbols ::
  Settings ->
  ServerState ->
  DocumentSymbolParams ->
  Aff (Array SymbolInformation)
getDocumentSymbols _ state _ = do
  let { port, root, modules } = un ServerState state
  case port, root of
    Just port', Just root' -> getSymbols root' port' ""
      (maybe [] singleton modules.main)
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

getSymbols ::
  String -> Int -> String -> Array String -> Aff (Array SymbolInformation)
getSymbols root port prefix modules = do
  let opts = CompletionOptions { maxResults: Nothing, groupReexports: true }
  completions <- getCompletion port prefix Nothing Nothing modules (const [])
    opts
  res <- liftEffect $ traverse getInfo completions
  pure $ catMaybes res

  where
  getInfo (Command.TypeInfo { identifier, definedAt: Just typePos, module' }) =
    do
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
