module LanguageServer.IdePurescript.FoldingRanges where

import Prelude
import Control.Alt ((<|>))
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable as Nullable
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.PscIdeServer (Notify)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (FoldingRangesParams)
import LanguageServer.IdePurescript.Types (ServerState)
import LanguageServer.TextDocument (getText)
import LanguageServer.Types (DocumentStore, FoldingRange(..), Settings, TextDocumentIdentifier(..))
import PureScript.CST (RecoveredParserResult(..))
import PureScript.CST as CST
import PureScript.CST.Types (DataCtor(..), Declaration(..), DoStatement(..), Expr(..), Foreign(..), Guarded(..), GuardedExpr(..), Ident, ImportDecl(..), Instance(..), InstanceBinding(..), Labeled(..), LetBinding(..), Module(..), ModuleBody(..), ModuleHeader(..), Name(..), QualifiedName(..), Separated(..), SourceToken, Where(..), Wrapped(..), SourceRange)
import PureScript.CST.Types as CSTTypes

getFoldingRanges :: Notify -> DocumentStore -> Settings -> ServerState -> FoldingRangesParams -> Aff (Array FoldingRange)
getFoldingRanges _log docs _ _ { textDocument: TextDocumentIdentifier textDocId } =
  liftEffect do
    doc <- getDocument docs textDocId.uri
    text <- getText doc
    let res = CST.parseModule text
    let
      ranges = case res of
        ParseSucceeded x -> getRanges x
        ParseSucceededWithErrors x _errs -> getRanges x
        ParseFailed _err -> []
    pure ranges

getRanges :: forall a. Module a -> Array FoldingRange
getRanges (Module { header, body: ModuleBody { decls } }) =
  let
    ModuleHeader { imports } = header
    importRanges = case Array.head imports, Array.last imports of
      Just (ImportDecl { keyword: { range: { start } } }), Just lastImport ->
        let
          last = Array.last $ Array.sortWith _.line (_.range.start <$> lastToks lastImport)
        in
          maybe [] (\end -> [ makeRange (Nullable.notNull "imports") start end ]) last
      _, _ -> []
    bodyRanges = Array.mapMaybe declRange decls
  in
    importRanges <> bodyRanges
  where
  lastToks (ImportDecl { keyword, names, qualified }) =
    keyword
      : (Array.fromFoldable $ Tuple.fst <$> qualified)
      <> (Array.fromFoldable $ unwrap <$> Tuple.snd <$> names)
  unwrap (Wrapped { close }) = close

declRange :: forall a. Declaration a -> Maybe FoldingRange
declRange = case _ of
  DeclData { keyword } dctors -> r keyword.range $ fromMaybe keyword.range $ dctorsEndRange <$> dctors
  DeclType { keyword } _ ty -> r keyword.range (fromMaybe keyword.range $ tyEndRange ty)
  DeclNewtype { keyword } _ _ ty -> r keyword.range (fromMaybe keyword.range $ tyEndRange ty)
  DeclClass { keyword } cls -> r keyword.range $ fromMaybe keyword.range $ classEndRange =<< cls
  DeclInstanceChain (Separated { head, tail }) ->
    let
      ranges = Array.concatMap instanceRanges (head : (Tuple.snd <$> tail))
    in
      case Array.head ranges, Array.last ranges of
        Just r1, Just r2 -> r r1 r2
        _, _ -> Nothing
  DeclDerive token _ { types } -> r token.range $ fromMaybe token.range $ tyEndRange =<< Array.last types
  DeclKindSignature token (Labeled { value }) -> r token.range $ fromMaybe token.range $ tyEndRange value
  DeclSignature (Labeled { label: Name { token }, value }) -> r token.range $ fromMaybe token.range $ tyEndRange value
  DeclValue { name: Name { token }, guarded } -> r token.range $ fromMaybe token.range $ guardedRange guarded
  DeclFixity _ -> Nothing
  DeclForeign token _ forn -> r token.range $ fromMaybe token.range $ foreignEndRange forn
  DeclRole token _tok _name roles -> r token.range $ (_.range <<< Tuple.fst) $ NEA.last roles
  DeclError _ -> Nothing

  where
  r startRange endRange = Just $ makeRange Nullable.null startRange.start endRange.end

guardedRange :: forall a. Guarded a -> Maybe SourceRange
guardedRange = case _ of
  Unconditional _tok where' -> where_ where'
  Guarded exprs ->
    case NEA.last exprs of
      (GuardedExpr { where: where' }) -> where_ where'

where_ :: forall a. Where a -> Maybe SourceRange
where_ (Where { expr, bindings }) =
  let
    r1 :: Maybe SourceRange
    r1 = Array.last $ exprRanges expr
    rb :: Maybe SourceRange
    rb = b =<< bindings
  in
    rb <|> r1
  where
  -- b :: _ -> Maybe SourceRange
  b (Tuple _ bindings) = letBindRange $ NEA.last $ bindings

letBindRange :: forall a. LetBinding a -> Maybe SourceRange
letBindRange = case _ of
  LetBindingSignature (Labeled { value }) -> tyEndRange value
  LetBindingName { name: _, binders: _, guarded } -> guardedRange guarded
  LetBindingPattern _bind _tok wh -> where_ wh
  LetBindingError _ -> Nothing

exprRanges :: forall a. Expr a -> Array SourceRange
exprRanges = case _ of
  ExprHole (Name { token }) -> [ token.range ]
  ExprSection token -> [ token.range ]
  ExprIdent (QualifiedName { token }) -> [ token.range ]
  ExprConstructor (QualifiedName { token }) -> [ token.range ]
  ExprBoolean token _ -> [ token.range ]
  ExprChar token _ -> [ token.range ]
  ExprString token _ -> [ token.range ]
  ExprInt token _ -> [ token.range ]
  ExprNumber token _ -> [ token.range ]
  ExprArray (Wrapped { open, close }) -> [ open.range, close.range ]
  ExprRecord (Wrapped { open, close }) -> [ open.range, close.range ]
  ExprParens (Wrapped { open, close }) -> [ open.range, close.range ]
  ExprTyped expr _token ty -> exprRanges expr <> Array.fromFoldable (tyEndRange ty)
  ExprInfix expr rest -> exprRanges expr <> (exprRanges $ Tuple.snd $ NEA.last rest)
  ExprOp expr rest -> exprRanges expr <> (exprRanges $ Tuple.snd $ NEA.last rest)
  ExprOpName (QualifiedName { token }) -> [ token.range ]
  ExprNegate token expr -> token.range : exprRanges expr
  ExprRecordAccessor { expr, dot, path: Separated { head, tail } } ->
    exprRanges expr
      <> [ fromMaybe (n head) $ ((n <<< Tuple.snd) <$> Array.last tail) ]
  ExprRecordUpdate e (Wrapped { close }) -> exprRanges e <> [ close.range ]
  ExprApp e es -> exprRanges e <> exprRanges (NEA.last es)
  ExprLambda { symbol, body } -> symbol.range : exprRanges body
  ExprIf { keyword, false: expr } -> keyword.range : exprRanges expr
  ExprCase { keyword, of: k2, branches } -> keyword.range : k2.range : Array.fromFoldable (guardedRange $ Tuple.snd $ NEA.last branches)
  ExprLet { keyword, body } -> keyword.range : exprRanges body
  ExprDo { keyword, statements } -> keyword.range : Array.fromFoldable (stmtEndRange $ NEA.last statements)
  ExprAdo { keyword, result } -> keyword.range : exprRanges result
  ExprError _ -> []
  where
  n :: forall a. Name a -> SourceRange
  n (Name { token }) = token.range

instanceRanges :: forall a. Instance a -> Array SourceRange
instanceRanges (Instance { head: { keyword }, body }) =
  keyword.range : maybe [] go body
  where
  go (Tuple _ bindings) = b $ NEA.last bindings
  b (InstanceBindingSignature (Labeled { value })) = Array.fromFoldable $ tyEndRange value
  b (InstanceBindingName ({ name: Name { token }, binders, guarded })) = [ token.range ]

stmtEndRange :: forall a. DoStatement a -> Maybe SourceRange
stmtEndRange = case _ of
  DoLet _ binds -> letBindRange $ NEA.last binds
  DoDiscard expr -> Array.last $ exprRanges expr
  DoBind _ _ expr -> Array.last $ exprRanges expr
  DoError _ -> Nothing

classEndRange :: (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (CSTTypes.Type _)))) -> _
classEndRange (Tuple _ xs) = go $ NEA.last xs
  where
  go (Labeled { value }) = tyEndRange value

dctorsEndRange :: forall a. (Tuple SourceToken (Separated (DataCtor a))) -> SourceRange
dctorsEndRange (Tuple head (Separated { tail })) =
  fromMaybe (head.range) $ (dctorRange <<< Tuple.snd) <$> Array.head tail
  where
  dctorRange (DataCtor { name: Name { token }, fields }) = fromMaybe token.range (Array.last fields >>= tyEndRange)

foreignEndRange :: _
foreignEndRange = case _ of
  ForeignValue (Labeled { value }) -> tyEndRange value
  ForeignData _tok (Labeled { value }) -> tyEndRange value
  ForeignKind _tok (Name { token }) -> Just token.range

tyEndRange :: forall a. CSTTypes.Type a -> Maybe SourceRange
tyEndRange = case _ of
  CSTTypes.TypeVar (Name { token }) -> Just token.range
  CSTTypes.TypeConstructor (QualifiedName { token }) -> Just token.range
  CSTTypes.TypeWildcard token -> Just token.range
  CSTTypes.TypeHole (Name { token }) -> Just token.range
  CSTTypes.TypeString token _ -> Just token.range
  CSTTypes.TypeRow (Wrapped { close }) -> Just close.range
  CSTTypes.TypeRecord (Wrapped { close }) -> Just close.range
  CSTTypes.TypeForall _ _ _ ty -> tyEndRange ty
  CSTTypes.TypeKinded _ty _tok ty -> tyEndRange ty
  CSTTypes.TypeApp _ tys -> tyEndRange $ NEA.last tys
  CSTTypes.TypeOp _ tys -> tyEndRange $ Tuple.snd $ NEA.last tys
  CSTTypes.TypeOpName (QualifiedName { token }) -> Just token.range
  CSTTypes.TypeArrow _ty _tok ty -> tyEndRange ty
  CSTTypes.TypeArrowName token -> Just token.range
  CSTTypes.TypeConstrained _ty _tok ty -> tyEndRange ty
  CSTTypes.TypeParens (Wrapped { close }) -> Just close.range
  CSTTypes.TypeUnaryRow _tok ty -> tyEndRange ty
  CSTTypes.TypeError _ -> Nothing

makeRange :: _
makeRange kind startPos endPos =
  FoldingRange
    { startLine: startPos.line
    , startCharacter: Nullable.notNull startPos.column
    , endLine: endPos.line
    , endCharacter: Nullable.notNull endPos.column
    , kind
    }
