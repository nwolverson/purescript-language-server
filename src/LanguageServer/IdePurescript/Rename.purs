module LanguageServer.IdePurescript.Rename
  ( prepareRename
  , renameIdentifier
  -- below methods and types exported for tests
  , getTypeInfoWithUsages
  , getTextAtRangeInLines
  , getTextEdits
  , DocToEdit
  , TextEditsMap
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over, un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Regex (regex, search, test) as Regex
import Data.String.Regex.Flags (ignoreCase, noFlags) as Regex
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.Modules as Modules
import IdePurescript.PscIdeServer (Port)
import IdePurescript.Tokens (identifierAtPoint, startsWithCapitalLetter)
import LanguageServer.IdePurescript.Rename.CST (getDeclSignatureName, getExportedRanges, getImportedRanges)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util (maybeParseResult)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (PrepareRenameParams, RenameParams, TextDocumentPositionParams)
import LanguageServer.Protocol.Text (makeMultiWorkspaceEdit)
import LanguageServer.Protocol.TextDocument (getText, getTextAtRange, getVersion)
import LanguageServer.Protocol.Types (DocumentStore, DocumentUri, Position(..), Range(..), Settings, TextDocumentIdentifier(..), WorkspaceEdit(..))
import LanguageServer.Protocol.Uri (filenameToUri, uriToFilename)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import PscIde as P
import PscIde.Command (DeclarationType(..), Namespace(..), TypeInfo(..), TypePosition(..))
import PscIde.Command as C
import PureScript.CST (RecoveredParserResult, parseModule)
import PureScript.CST.Types (Module, ModuleName(..))

-- | Handler for prepare rename operation.
prepareRename ::
  DocumentStore ->
  Settings ->
  ServerState ->
  PrepareRenameParams ->
  Aff (Nullable Range)
prepareRename docs settings state ({ textDocument, position }) = do
  identInfo <- getIdentInfo docs settings state { textDocument, position }
  pure $ Nullable.toNullable $ _.range <$> identInfo

-- | Handler for rename operation.
renameIdentifier ::
  DocumentStore ->
  Settings ->
  ServerState ->
  RenameParams ->
  Aff (WorkspaceEdit)
renameIdentifier docs settings state ({ textDocument, position, newName }) = do
  identInfo <- getIdentInfo docs settings state { textDocument, position }
  case identInfo of
    Just ({ word, found: Right { typeInfo, usages } }) -> do
      docsToEdit <- getDocsToEdit typeInfo usages
      liftEffect $ do
        let usgEdits = getTextEdits typeInfo usages docsToEdit newName word
        pure
          $ makeMultiWorkspaceEdit clientCapabilities (toMultiEdits usgEdits)
    -- Means that we deal with a local identifier.
    -- Not implemented.
    Just ({ found: Left { } }) -> do
      pure wsEditEmpty

    Nothing -> do
      pure wsEditEmpty
  where
  wsEditEmpty = WorkspaceEdit
    { documentChanges: Nullable.null
    , changes: Nullable.null
    }

  { clientCapabilities } = un ServerState $ state

  toMultiEdits m =
    Map.toUnfoldable m
      <#> (\((uri /\ version) /\ edits) -> { uri, version, edits })

  getDocsToEdit (TypeInfo ti) usages =
    Array.foldM goFile Map.empty files
    where
    getName (TypePosition { name }) = name
    files = Array.nub
      $ (usages <#> getName)
          <> maybe [] Array.singleton (ti.definedAt <#> getName)

  goFile dcs path = do
    uri <- liftEffect $ filenameToUri path
    mbDoc <- liftEffect $ getDocument docs uri
    docText /\ version <-
      case (Nullable.toMaybe mbDoc) of
        -- For docs that were not yet opened in the editor we read the file
        -- from disk to search its content.
        --
        -- Alternatively may try this:
        -- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#window_showDocument
        Nothing -> do
          docText <- FS.readTextFile (UTF8) path
          pure (docText /\ Nothing)
        Just doc -> liftEffect $ do
          version <- getVersion doc
          docText <- getText doc
          pure (docText /\ Just version)

    pure $ Map.insert path
      { uri
      , docTextAtRange:
          getTextAtRangeInLines (splitLines docText)
      , version
      , parsed: parseModule docText
      }
      dcs

type DocToEdit =
  { docTextAtRange :: Range -> String
  , uri :: DocumentUri
  , version :: Maybe Number
  , parsed :: RecoveredParserResult Module
  }

type TextEditsMap =
  Map (DocumentUri /\ (Maybe Number))
    (Array { newText :: String, range :: Range })

-- | Maps found typeInfo and usages to plain text edits.
getTextEdits ::
  TypeInfo ->
  Array (TypePosition) ->
  Map String DocToEdit ->
  String ->
  String ->
  TextEditsMap
getTextEdits typeInfo usages docsToEdit newText oldName =
  -- We use Maybe
  ( Just Map.empty
      # addUsagesEdits
      # addTypeDefEdit
      # addImportsEdits
  )
    # fromMaybe Map.empty
    -- We remove possible ranges duplication due to combination of purs ide
    -- usage results and local logic based on CST parsing.
    # removeDups
  where
  removeDups = map Array.nub

  (TypeInfo ty) = typeInfo
  moduleDefinedIn = ModuleName ty.module'
  addUsagesEdits edits =
    Array.foldl
      ( \editsMap tp@(TypePosition { name }) ->
          case Map.lookup name docsToEdit of
            Nothing ->
              editsMap
            Just { docTextAtRange, uri, version } ->
              let
                useRange = rangeFromTypePos $ tp
                rangeText = docTextAtRange useRange
                -- Get qualifier index.
                qualIdx = maybe 0 ((+) 1) $ String.indexOf (Pattern ".") rangeText

                -- Make new text from old because found range may contain
                -- other symbols beside the name (in case of constructor unwrapping).

                -- This is a bit tricky case and workaround. Purs ide returns
                -- for usage of types/constructors in imports whole type with
                -- ctors span: Name(A, Name), where the name of type can be
                -- the same as ctor name. So is we are looking for constructor
                -- replace it with "_ame(A, Name)", to find correct position.
                handleCtorImport t
                  | isCtor, Just idx <- String.lastIndexOf (Pattern oldName) t, idx > 0 =
                      ((<>) "_" <<< String.drop 1) t
                handleCtorImport t = t

                mbRange = useRange
                  # rangeShiftLeft qualIdx
                  # findWordInRange (Pattern oldName)
                      (rangeText # String.drop qualIdx # handleCtorImport)
                addEdit' range = addEdit (uri /\ version) { range, newText }

              in
                editsMap # maybe cancelEdit addEdit' mbRange
      )
      edits
      usages

  -- Adds defined info position and exports
  addTypeDefEdit edits =
    case getDefined typeInfo of
      Nothing ->
        edits
      Just (tp /\ { docTextAtRange, version, uri, parsed }) ->
        let
          defRange = rangeFromTypePos tp
          addEdit' range = addEdit (uri /\ version) { range, newText }
          fixRange range =
            let
              rangeText = docTextAtRange range
              mbRange = findWordInRange (Pattern oldName) rangeText range
            in
              maybe cancelEdit addEdit' mbRange

        in
          edits
            # fixRange defRange
            -- Add export and declaration edit. Actual if purs ide does not return
            -- positions in exports. CST parsed results to may too provide ranges
            -- that should be fixed.
            # maybeParseResult identity
                ( \m ->
                    ( case Array.head $ getExportedRanges m isType oldName of
                        Just range -> do
                          fixRange range
                        Nothing ->
                          identity
                    )
                      -- Add declaration signature.
                      # (<<<)
                          case getDeclSignatureName m isType oldName of
                            Just range ->
                              addEdit' range
                            Nothing ->
                              identity
                )
                parsed

    where
    getDefined (TypeInfo { definedAt }) =
      definedAt
        >>= \tp@(TypePosition { name }) ->
          Map.lookup name docsToEdit
            <#> \doc -> tp /\ doc

  -- Finds usages parsed module in imports. Actual if purs ide does not return
  -- positions in imports. CST parsed results to may too provide ranges that
  -- should be fixed.
  addImportsEdits edits =
    Array.foldl
      ( \editsMap fileName ->
          case Map.lookup fileName docsToEdit of
            Just { uri, version, parsed, docTextAtRange } ->
              let
                addEdit' range = addEdit (uri /\ version) { range, newText }
                fixRange range =
                  let
                    rangeText = docTextAtRange range
                    mbRange = findWordInRange (Pattern oldName) rangeText range
                  in
                    maybe cancelEdit addEdit' mbRange
              in
                editsMap #
                  ( maybeParseResult identity
                      ( \m ->
                          case
                            Array.head $ getImportedRanges m isType oldName moduleDefinedIn
                            of
                            Just range ->
                              fixRange range
                            Nothing ->
                              identity
                      )
                      parsed
                  )
            Nothing ->
              editsMap
      )
      edits
      files
    where
    (TypeInfo ti) = typeInfo
    getName (TypePosition { name }) = name
    defFileName = ti.definedAt <#> getName
    -- All files to edit without the file with type definition.
    files =
      ( maybe identity (\defFile -> Array.filter (\name -> name /= defFile))
          defFileName
      )
        (Array.nub $ usages <#> getName)

  isType = isNSType $ typeInfoToNs typeInfo
  isCtor = startsWithCapitalLetter oldName && not isType

  toRangePos { line, column } =
    Position { line: line - 1, character: column - 1 }

  rangeFromTypePos (TypePosition { start, end }) =
    Range
      { start: toRangePos start
      , end: toRangePos end
      }

  addEdit key edit m =
    m <#> Map.alter (Just <<< maybe [ edit ] (Array.cons edit)) key

  cancelEdit = const Nothing

  rangeShiftLeft charNum (Range ({ start: Position p, end })) =
    Range
      ( { start: Position { line: p.line, character: p.character + charNum }
        , end
        }
      )

--| Uses purs ide to find information about definition and usages of rename target given its
-- symbolic name and its type position.
--
-- TypePosition in .definedAt (start, end):
--
--  - for fns/types position of the WHOLE body of defined value.
--  - for fns defines line of value, the same for kind (line of its type
--    definition)
-- - for type ctors: start.column is a position of "=" or "|" symbol before it
getTypeInfoWithUsages ::
  Port ->
  String ->
  TypePosition ->
  Maybe String ->
  Modules.State ->
  Aff (Maybe (TypeInfo /\ Array TypePosition))
getTypeInfoWithUsages port word wordPos qualifier moduleState = do
  -- Find where the target is defined.
  typeInfos <-
    (eitherToErr $ P.type' port word moduleFilters moduleState.main)
  -- Find its usages. First, try to check if if the target is in the defined
  -- positions. Then for each defined position find usages until they contain
  -- the target.

  case findInDefined typeInfos of
    Just info ->
      getUsages info <#> Just <<< (/\) info
    Nothing ->
      -- Find usage corresponding to target.
      f >>= maybe findTy (pure <<< Just)
      where
      f = Array.foldM
        (\res info -> maybe (getCheckedUsages info) (pure <<< Just) res)
        Nothing
        typeInfos
      findTy = case findTypeInDefined typeInfos of
        Just info -> getUsages info <#> Just <<< (/\) info
        Nothing -> pure Nothing
  where

  checkPosition =
    comparePositions wordPos

  getCheckedUsages info = getUsages info
    <#> \usages ->
      (Array.find checkPosition usages) <#>
        (const $ info /\ usages)

  isMaybeTypeCtor = startsWithCapitalLetter word

  comparePositionFile (TypePosition p1) (TypePosition p2) =
    String.toLower p1.name == String.toLower p2.name

  isTypeOrFnDecl (TypeInfo { declarationType }) =
    case declarationType of
      Just DeclValue -> true
      Just DeclType -> true
      Just DeclTypeSynonym -> true
      Just DeclTypeClass -> true
      _ -> false

  -- typeInfo.definedAt points to: to value level definition (not a signature)
  -- or Type definition (not a kind signature) or ADT ctor.
  --
  -- ADT ctor symbolic  name can be the same as Type name: in this case
  -- PscIde.usages returns two typeInfos in the same file (Type first then Ctor
  -- in the list).
  findInDefined typeInfos =
    Array.findMap
      ( \ti@(TypeInfo { definedAt }) ->
          definedAt
            <#>
              ( \tp ->
                  -- Check if renaming ident position is inside defined.
                  checkPositionNested wordPos tp
              -- If target rename ident is an exported ref or type
              -- signature name they are not in the source position that
              -- comes in definedAt. So we just check if this is the same
              -- module.
              --|| (isTypeOrFnDecl ti && comparePositionFile wordPos tp)
              )
            >>= case _ of
              true -> Just ti
              false -> Nothing
      )
      -- Reverse order of found defs, because first we want to check ctor def
      -- which comes after type def in the result.
      (Array.reverse typeInfos)

  findTypeInDefined typeInfos =
    Array.findMap
      ( \ti@(TypeInfo { definedAt }) ->
          definedAt
            <#>
              ( \tp ->
                  (isTypeOrFnDecl ti && comparePositionFile wordPos tp)
              )
            >>= case _ of
              true -> Just ti
              false -> Nothing
      )
      -- Reverse order of found defs, because first we want to check ctor def
      -- which comes after type def in the result.
      (Array.reverse typeInfos)

  -- For not exported identifiers ide usages returns the error "Declaration not
  -- found".
  getUsages ti@(TypeInfo { module' }) =
    -- Need to nub because currently (until fixed) purs ide may return
    -- duplicated positions, in case if imports are duplicated.
    Array.nubByEq comparePositions <$>
      ( eitherToErr $ P.usages port module'
          (typeInfoToNs ti)
          word
      )

  unqualModules =
    -- if it is possibly constructor then we search in every possible module
    if isMaybeTypeCtor then
      Modules.getAllUnqualActiveModules moduleState
    else
      getUnqualActiveModules moduleState (Just word)

  getQualifiedModule = flip getQualModule moduleState

  moduleFilters =
    [ C.ModuleFilter $ maybe unqualModules getQualifiedModule qualifier ]

type LocalUsage = { ranges :: Array Range }
type ExternUsage = { typeInfo :: TypeInfo, usages :: Array (TypePosition) }

type IdentInfo =
  { word :: String
  , found :: Either LocalUsage ExternUsage
  , range :: Range
  }

-- | Finds information about definition and usages of rename target given
-- original position and LSP stuff.
getIdentInfo ::
  DocumentStore ->
  Settings ->
  ServerState ->
  TextDocumentPositionParams ->
  Aff (Maybe IdentInfo)
getIdentInfo docs _ state { textDocument, position } = do
  let { port, modules } = un ServerState $ state
  let uri = _.uri $ un TextDocumentIdentifier textDocument
  mbDoc <- liftEffect $
    Nullable.toMaybe <$> getDocument docs uri
  mbDoc
    # maybe (pure empty) \doc -> do
        lineText <- liftEffect $ getTextAtRange doc (mkRange position)
        case port, getIdentFromLine lineText of
          Just port', Just { word, qualifier, range } -> do
            modulePath <- liftEffect $ uriToFilename uri
            infoUsage <-
              getTypeInfoWithUsages port' word
                ( mkTypePosition modulePath
                    (mkWordRange position range Nothing)
                )
                qualifier
                modules
            pure case infoUsage of
              Just (typeInfo /\ usages)
                -- Forbid refactoring in libraries (for now).
                | isLibType typeInfo -> Nothing
                | otherwise ->
                    -- Currently PscIde (purs ide) returns no usage for any type
                    -- namespace. So for now in this case it is better to disable
                    -- rename for type level if it returns empty usages.
                    --
                    -- We should remove this check when it is fixed.
                    if isNSType (typeInfoToNs typeInfo) && Array.null usages then
                      Nothing
                    else
                      Just
                        { word
                        , range: mkWordRange position range qualifier
                        , found: Right { typeInfo, usages: usages }
                        }
              _ ->
                empty
          _, _ -> pure empty
  where
  empty = Nothing

  isLibType (TypeInfo { definedAt: Just (TypePosition p) }) =
    String.contains (String.Pattern ".spago") p.name
  isLibType _ = false

  getIdentFromLine lineText =
    (identifierAtPoint lineText (_.character $ un Position position))

  -- make line range with target identifier
  mkRange pos =
    Range
      { start: pos # over Position (_ { character = 0 })
      , end: pos # over Position
          (\c -> c { character = c.character + maxIdentLen })
      }
    where
    maxIdentLen = 100
  mkWordRange pos { left, right } qualifier =
    Range
      { start: pos # over Position mkStart
      , end: pos # over Position (\c -> c { character = right })
      }
    where
    mkStart = _
      { character = left +
          -- adjust start if qualified
          (maybe 0 (\s -> String.length s + 1) qualifier)
      }

  rangePosToTypePos (Position { character, line }) =
    { column: character + 1, line: line + 1 } -- differs by 1

  mkTypePosition filePath (Range { start, end }) =
    TypePosition
      { name: filePath
      , start: rangePosToTypePos start
      , end: rangePosToTypePos end
      }

splitLines :: String -> Array String
splitLines =
  String.split (Pattern "\n")

eitherToErr :: forall a. Aff (Either String a) -> (Aff a)
eitherToErr c = do
  r <- c
  case r of
    Left s -> throwError (error s)
    Right res -> pure res

isNSType :: C.Namespace -> Boolean
isNSType =
  case _ of
    NSType -> true
    _ -> false

typeInfoToNs :: TypeInfo -> C.Namespace
typeInfoToNs (TypeInfo { declarationType }) =
  maybe NSValue
    case _ of
      DeclValue -> NSValue
      DeclType -> NSType
      DeclTypeSynonym -> NSType
      DeclDataConstructor -> NSValue
      DeclTypeClass -> NSType
      DeclValueOperator -> NSValue
      DeclTypeOperator -> NSType
      DeclModule -> NSType
    declarationType

-- Check if positions are  the same we compare only start position.
comparePositions :: TypePosition -> TypePosition -> Boolean
comparePositions (TypePosition pos1) (TypePosition pos2) =
  comparePaths pos1.name pos2.name
    && (pos1.start == pos2.start)
  --&& (pos1.end == pos2.end)
  where
  -- Paths may differ on windows because of drive letter.
  comparePaths p1 p2 =
    String.toLower p1 == String.toLower p2

-- Check if the first position is nested inside the second one.
checkPositionNested :: TypePosition -> TypePosition -> Boolean
checkPositionNested (TypePosition inner) (TypePosition outer) =
  comparePaths inner.name outer.name
    && toTup outer.start <= toTup inner.start
    && toTup inner.end <= toTup outer.end
  where
  toTup ({ line, column }) = line /\ column
  -- "normalize" paths that may differ on windows (drive letter case)
  comparePaths p1 p2 =
    String.toLower p1 == String.toLower p2

findWordInRange :: Pattern -> String -> Range -> Maybe Range
findWordInRange (Pattern word) text range =
  Array.findMap
    ( \(lineNum /\ lineText) ->
        searchIndex lineText
          <#> \startChar ->
            let
              (Range { start: Position p }) = range
              line = p.line + lineNum
              character = p.character + startChar
            in
              Range
                { start: Position { line, character }
                , end:
                    Position
                      { line
                      , character: character + String.length word
                      }
                }
    )
    $ Array.mapWithIndex
        (\idx s -> idx /\ s)
        (String.split (Pattern "\n") text)
  where
  isOp = maybe false (not <<< flip Regex.test word) $ hush
    $ Regex.regex ("[a-z]") Regex.ignoreCase
  searchIndex line
    -- For operators use just search in string, for identifiers - regexp to
    -- match the whole word
    | isOp = String.indexOf (Pattern word) line
    | otherwise =
        (hush $ Regex.regex ("\\b" <> word <> "\\b") Regex.noFlags) >>=
          flip Regex.search line

getTextAtRangeInLines :: Array String -> Range -> String
getTextAtRangeInLines lines (Range { start: Position start, end: Position end }) =
  String.joinWith "\n"
    ( Array.slice start.line (end.line + 1) lines
        # cutEnd
        # cutStart
    )
  where
  cutStart ln =
    case Array.uncons ln of
      Just { head, tail } -> Array.cons (String.drop start.character head) tail
      _ -> ln
  cutEnd ln =
    case Array.unsnoc ln of
      Just { init, last } -> Array.snoc init
        (String.take (end.character) last)
      _ -> ln
