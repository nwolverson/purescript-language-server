module LanguageServer.IdePurescript.Tooltips where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.List.Trans (scanl)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (un)
import Data.Nullable (Nullable, toNullable)
import Data.String (null)
import IdePurescript.Modules (getQualModule, getUnqualActiveModules)
import IdePurescript.PscIde (getType, getTypeInfo)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.DocumentStore (getDocument)
import LanguageServer.Handlers (TextDocumentPositionParams)
import LanguageServer.IdePurescript.Types (ServerState(..), MainEff)
import LanguageServer.TextDocument (getTextAtRange)
import LanguageServer.Types (DocumentStore, Hover(..), Position(..), Range(..), Settings, TextDocumentIdentifier(..), markedString)
import PscIde.Command as C

getTooltips :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> TextDocumentPositionParams -> Aff (MainEff eff) (Nullable Hover)
getTooltips docs settings state ({ textDocument, position }) = do
  doc <- liftEff $ getDocument docs (_.uri $ un TextDocumentIdentifier textDocument)
  text <- liftEff $ getTextAtRange doc $ lineRange position
  let { port, modules, conn } = un ServerState state
      char = _.character $ un Position $ position
  case port, identifierAtPoint text char of
    Just port', Just { word, qualifier } -> do
      ty <- getTypeInfo port' word modules.main qualifier (getUnqualActiveModules modules $ Just word) (flip getQualModule modules)
      pure $ toNullable $ map (convertInfo word) ty
    _, _ -> pure $ toNullable Nothing

  where

  convertInfo word (C.TypeInfo { type', expandedType }) = Hover
    {
      contents: markedString $ compactTypeStr <> 
        if showExpanded then "\n" <> expandedTypeStr else ""
    , range: toNullable $ Nothing -- Just $ Range { start: position, end: position }
    }
    where
      showExpanded = isJust expandedType && (expandedType /= Just type')
      compactTypeStr = word <> " :: " <> type'
      expandedTypeStr = word <> " :: " <> (fromMaybe "" expandedType)

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