module LanguageServer.IdePurescript.Assist where

import Prelude
import PscIde as P
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Array (intercalate, length)
import Data.Either (Either(..))
import Data.Foreign (Foreign, readInt, readString)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over)
import IdePurescript.PscIde (eitherToErr)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.Console (log)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (applyEdit)
import LanguageServer.IdePurescript.Types (MainEff, ServerState(..))
import LanguageServer.TextDocument (getTextAtRange, getVersion)
import LanguageServer.Types (DocumentStore, DocumentUri(..), Position(..), Range(..), Settings)
import LanguageServer.Text (makeWorkspaceEdit)

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
  liftEff $ maybe (pure unit) (\c -> log c $ show (length args) ) conn
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
                    liftEff $ log conn' $ "Case split: " <> lineText <> " / " <> show left <> " / " <> show right <> " / " <> tyStr
                    lines <- eitherToErr $ P.caseSplit port' lineText left right true tyStr
                    let edit = makeWorkspaceEdit (DocumentUri uri) version (lineRange' line char) (intercalate "\n" lines)
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
                    lines <- eitherToErr $ P.addClause port' lineText true
                    let edit = makeWorkspaceEdit (DocumentUri uri) version (lineRange' line char) (intercalate "\n" lines)
                    liftEff $ applyEdit conn' edit
                _ -> pure unit
            pure unit
    _, _, _ -> pure unit
