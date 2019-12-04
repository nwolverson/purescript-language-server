module LanguageServer.IdePurescript.Assist where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (fold, intercalate, (!!))
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Data.String (trim)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (F, Foreign, readInt, readString, unsafeFromForeign, unsafeToForeign)
import Foreign.Index ((!))
import IdePurescript.PscIde (eitherToErr)
import IdePurescript.PscIdeServer (Notify)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.Console (log)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (applyEdit)
import LanguageServer.IdePurescript.CodeActions (readRange)
import LanguageServer.IdePurescript.Imports (addCompletionImport, addCompletionImportEdit)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Text (makeWorkspaceEdit)
import LanguageServer.TextDocument (getText, getTextAtRange, getVersion)
import LanguageServer.Types (DocumentStore, DocumentUri(..), Position(..), Range(..), Settings)
import PscIde (defaultCompletionOptions, suggestTypos)
import PscIde as P
import PscIde.Command (TypeInfo(..))

lineRange' :: Int -> Int -> Range
lineRange' line character = lineRange $ Position { line, character }

lineRange :: Position -> Range
lineRange pos@(Position { line, character }) = Range 
    { start: pos # over Position (_ { character = 0 })
    , end: pos # over Position (_ { character = (top :: Int) })
    }

caseSplit :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
caseSplit docs settings state args = do
  let ServerState { port, conn } = state
  case port, conn, args of
    Just port', Just conn', [ argUri, argLine, argChar, argType ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        , Right tyStr <- runExcept $ readString argType
        -> do
            doc <- liftEffect $ getDocument docs (DocumentUri uri)
            lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
            version <- liftEffect $ getVersion doc
            case identifierAtPoint lineText char of
                Just { range: { left, right } } -> do
                    lines <- eitherToErr $ P.caseSplit port' lineText left right false tyStr
                    let edit = makeWorkspaceEdit (DocumentUri uri) version (lineRange' line char) $ intercalate "\n" $ map trim lines
                    void $ applyEdit conn' edit
                _ -> do liftEffect $ log conn' "fail identifier"
                        pure unit
            pure unit
    _, Just conn', [ argUri, argLine, argChar, argType ] ->
        liftEffect $ log conn' $ show [ show $ runExcept $ readString argUri, show $ runExcept $ readInt argLine , show $ runExcept $ readInt argChar, show $ runExcept $ readString argType ]
    _, _, _ -> do 
        liftEffect $ maybe (pure unit) (flip log "fial match") conn
        pure unit

addClause :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
addClause docs settings state args = do
  let ServerState { port, conn } = state
  case port, conn, args of
    Just port', Just conn', [ argUri, argLine, argChar ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        -> do
            doc <- liftEffect $ getDocument docs (DocumentUri uri)
            lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
            version <- liftEffect $ getVersion doc
            case identifierAtPoint lineText char of
                Just { range: { left, right } } -> do
                    lines <- eitherToErr $ P.addClause port' lineText false
                    let edit = makeWorkspaceEdit (DocumentUri uri) version (lineRange' line char) $ intercalate "\n" $ map trim lines
                    void $ applyEdit conn' edit
                _ -> pure unit
            pure unit
    _, _, _ -> pure unit

newtype TypoResult = TypoResult { identifier :: String, mod :: String }

encodeTypoResult :: TypoResult -> Foreign
encodeTypoResult = unsafeToForeign

decodeTypoResult :: Foreign -> F TypoResult
decodeTypoResult obj = do
  identifier <- obj ! "identifier" >>= readString
  mod <- obj ! "mod" >>= readString
  pure $ TypoResult { identifier, mod }

fixTypo :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
fixTypo log docs settings state args = do
  let ServerState { port, conn, modules } = state
  (unsafeToForeign <<< map encodeTypoResult) <$> case port, conn, args !! 0, args !! 1, args !! 2 of
    Just port', Just conn', Just argUri, Just argLine, Just argChar
      | Right uri <- runExcept $ readString argUri
      , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
      , Right char <- runExcept $ readInt argChar -> do
        doc <- liftEffect $ getDocument docs (DocumentUri uri)
        lineText <- liftEffect $ getTextAtRange doc (lineRange' line char)
        version <- liftEffect $ getVersion doc
        case identifierAtPoint lineText char, (runExcept <<< decodeTypoResult) <$> args !! 3 of
            Just { range }, Just (Right (TypoResult { identifier, mod })) -> [] <$ replace conn' uri version line range identifier mod
            -- TODO add import?
            Just { word, range }, _ -> do
                res <- suggestTypos port' word 2 modules.main defaultCompletionOptions
                case res of
                    Right [ TypeInfo { identifier, module' } ] -> [] <$ replace conn' uri version line range identifier module'
                    _ -> pure $ map convertRes $ either (pure []) identity res
            _, _ -> pure $ []
    _, _, _, _, _ -> pure  []

  where
    emptyRes = unsafeToForeign []
    convertRes (TypeInfo { identifier, module' }) = TypoResult { identifier, mod: module' }
    replace conn uri version line {left, right} word mod = do 
      let range = Range { start: Position { line, character: right }
                        , end: Position { line, character: left }
                        }
          edit = makeWorkspaceEdit (DocumentUri uri) version range word
      void $ applyEdit conn edit
      addCompletionImport log docs settings state [ unsafeToForeign word, unsafeToForeign mod, unsafeToForeign Nothing, unsafeToForeign uri ]

fillTypedHole :: Notify -> DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Unit
fillTypedHole logFn docs settings state args = do
  let ServerState { port, conn } = state
  case port, conn, args of
    Just port', Just conn', [ _, argUri, range', argChoice ]
      | Right range <- runExcept $ readRange range'
      , Right uri <- runExcept $ readString argUri
      , TypeInfo { identifier, module': mod } <- readTypeInfo argChoice
     -> do
      doc <- liftEffect $ getDocument docs (DocumentUri uri)
      version <- liftEffect $ getVersion doc
      text <- liftEffect $ getText doc
      let edit = makeWorkspaceEdit (DocumentUri uri) version range identifier
      edit' <- either (const []) identity <$> addCompletionImportEdit logFn docs settings state 
         { identifier, mod: Just mod , qual: Nothing, uri: DocumentUri uri }
        doc version text
      let edit2 = edit <> fold edit'
      applyRes <- applyEdit conn' $ edit2 -- edit <> fold edit'
      liftEffect $ log conn' $ "Applied: " <> show applyRes
      -- -- Seems that even after waiting for the edit response, changes will be lost 
      -- delay $ Milliseconds 300.0
      _ <- addCompletionImport logFn docs settings state [ unsafeToForeign identifier, unsafeToForeign mod, unsafeToForeign Nothing, unsafeToForeign uri ]
      pure unit
    _, _, _ -> do 
      liftEffect $ maybe (pure unit) (flip log "fail match") conn
      pure unit
  where
    readTypeInfo :: Foreign -> TypeInfo
    readTypeInfo obj = unsafeFromForeign obj