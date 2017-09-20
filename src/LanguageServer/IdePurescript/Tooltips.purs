module LanguageServer.IdePurescript.Tooltips where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Data.String (null)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getType)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (TextDocumentPositionParams)
import LanguageServer.IdePurescript.Types (ServerState(..), MainEff)
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (DocumentStore, Hover(..), Position(..), Range(..), Settings, TextDocumentIdentifier(..), markedString)

getTooltips :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> TextDocumentPositionParams -> Aff (MainEff eff) (Nullable Hover)
getTooltips docs settings state ({ textDocument, position }) = do
  doc <- liftEff $ getDocument docs (_.uri $ un TextDocumentIdentifier textDocument)
  text <- liftEff $ getTextAtRange doc $ lineRange position
  let { port, modules, conn } = un ServerState state
      char = _.character $ un Position $ position
  case port, identifierAtPoint text char of
    Just port', Just { word, qualifier } -> do
      ty <- getType port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)
      pure $ toNullable $
        if null ty then Nothing
        else Just $ Hover
            {
              contents: markedString $ word <> " :: " <> ty
            , range: toNullable $ Nothing -- Just $ Range { start: position, end: position }
            }
    _, _ -> pure $ toNullable Nothing

  where
  lineRange (Position { line, character }) =
    Range
      { start: Position
        { line
        , character: 0
        }
      , end: Position
        { line
        , character: character + 100
        }
      }