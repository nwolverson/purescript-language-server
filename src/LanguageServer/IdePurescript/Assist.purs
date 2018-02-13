module LanguageServer.IdePurescript.Assist where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate, (!!))
import Data.Either (Either(..), either)
import Data.Foreign (Foreign, F, readInt, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import Data.String (trim)
import IdePurescript.PscIde (eitherToErr)
import IdePurescript.PscIdeServer (Notify)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.Console (log)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (applyEdit)
import LanguageServer.IdePurescript.Imports (addCompletionImport)
import LanguageServer.IdePurescript.Types (MainEff, ServerState(..))
import LanguageServer.Text (makeWorkspaceEdit)
import LanguageServer.TextDocument (getTextAtRange, getVersion)
import LanguageServer.Types (DocumentStore, DocumentUri(..), Position(..), Range(..), Settings)
import PscIde (defaultCompletionOptions, suggestTypos)
import PscIde as P
import PscIde.Command (TypeInfo(..))

lineRange' :: Int -> Int -> Range
lineRange' line character = lineRange $ Position { line, character }

lineRange :: Position -> Range
lineRange (pos@ Position { line, character }) = Range 
    { start: pos # over Position (_ { character = 0 })
    , end: pos # over Position (_ { character = (top :: Int) })
    }

caseSplit :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Unit
caseSplit docs settings state args = do
  let ServerState { port, conn } = state
  case port, conn, args of
    Just port', Just conn', [ argUri, argLine, argChar, argType ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        , Right tyStr <- runExcept $ readString argType
        -> do
            doc <- liftEff $ getDocument docs (DocumentUri uri)
            lineText <- liftEff $ getTextAtRange doc (lineRange' line char)
            version <- liftEff $ getVersion doc
            case identifierAtPoint lineText char of
                Just { range: { left, right } } -> do
                    lines <- eitherToErr $ P.caseSplit port' lineText left right false tyStr
                    let edit = makeWorkspaceEdit (DocumentUri uri) version (lineRange' line char) $ intercalate "\n" $ map trim lines
                    liftEff $ applyEdit conn' edit
                _ -> do liftEff $ log conn' "fail identifier"
                        pure unit
            pure unit
    _, Just conn', [ argUri, argLine, argChar, argType ] ->
        liftEff $ log conn' $ show [ show $ runExcept $ readString argUri, show $ runExcept $ readInt argLine , show $ runExcept $ readInt argChar, show $ runExcept $ readString argType ]
    _, _, _ -> do 
        liftEff $ maybe (pure unit) (flip log "fial match") conn
        pure unit

addClause :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Unit
addClause docs settings state args = do
  let ServerState { port, conn } = state
  case port, conn, args of
    Just port', Just conn', [ argUri, argLine, argChar ]
        | Right uri <- runExcept $ readString argUri
        , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
        , Right char <- runExcept $ readInt argChar
        -> do
            doc <- liftEff $ getDocument docs (DocumentUri uri)
            lineText <- liftEff $ getTextAtRange doc (lineRange' line char)
            version <- liftEff $ getVersion doc
            case identifierAtPoint lineText char of
                Just { range: { left, right } } -> do
                    lines <- eitherToErr $ P.addClause port' lineText false
                    let edit = makeWorkspaceEdit (DocumentUri uri) version (lineRange' line char) $ intercalate "\n" $ map trim lines
                    liftEff $ applyEdit conn' edit
                _ -> pure unit
            pure unit
    _, _, _ -> pure unit

newtype TypoResult = TypoResult { identifier :: String, mod :: String }

encodeTypoResult :: TypoResult -> Foreign
encodeTypoResult = toForeign

decodeTypoResult :: Foreign -> F TypoResult
decodeTypoResult obj = do
  identifier <- obj ! "identifier" >>= readString
  mod <- obj ! "mod" >>= readString
  pure $ TypoResult { identifier, mod }

fixTypo :: forall eff. Notify (MainEff eff) -> DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Foreign
fixTypo log docs settings state args = do
  let ServerState { port, conn, modules } = state
  (toForeign <<< map encodeTypoResult) <$> case port, conn, args !! 0, args !! 1, args !! 2 of
    Just port', Just conn', Just argUri, Just argLine, Just argChar
      | Right uri <- runExcept $ readString argUri
      , Right line <- runExcept $ readInt argLine -- TODO: Can this be a Position?
      , Right char <- runExcept $ readInt argChar -> do
        doc <- liftEff $ getDocument docs (DocumentUri uri)
        lineText <- liftEff $ getTextAtRange doc (lineRange' line char)
        version <- liftEff $ getVersion doc
        case identifierAtPoint lineText char, (runExcept <<< decodeTypoResult) <$> args !! 3 of
            Just { range }, Just (Right (TypoResult { identifier, mod })) -> [] <$ replace conn' uri version line range identifier mod
            -- TODO add import?
            Just { word, range }, _ -> do
                res <- suggestTypos port' word 2 modules.main defaultCompletionOptions
                case res of
                    Right [ TypeInfo { identifier, module' } ] -> [] <$ replace conn' uri version line range identifier module'
                    _ -> pure $ map convertRes $ either (pure []) id res
            _, _ -> pure $ []
    _, _, _, _, _ -> pure  []

  where
    emptyRes = toForeign []
    convertRes (TypeInfo { identifier, module' }) = TypoResult { identifier, mod: module' }
    replace conn uri version line {left, right} word mod = do 
      let range = Range { start: Position { line, character: right }
                        , end: Position { line, character: left }
                        }
          edit = makeWorkspaceEdit (DocumentUri uri) version range word
      liftEff $ applyEdit conn edit
      addCompletionImport log docs settings state [ toForeign word, toForeign mod, toForeign Nothing, toForeign uri ]
