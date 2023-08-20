module LanguageServer.IdePurescript.References
  ( getReferences
  ) where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (over, un)
import Data.Nullable as Nullable
import Data.String.Utils (endsWith)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import IdePurescript.PscIdeServer (Notify)
import IdePurescript.Tokens (identifierAtPoint)
import LanguageServer.IdePurescript.Symbols (convPosition)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.IdePurescript.Util.TypeInfo (getTypeInfoMaybeNew)
import LanguageServer.Protocol.DocumentStore (getDocument)
import LanguageServer.Protocol.Handlers (ReferenceParams)
import LanguageServer.Protocol.TextDocument (getTextAtRange)
import LanguageServer.Protocol.Types (DocumentStore, Location(..), Position(..), Range(..), Settings, TextDocumentIdentifier(..))
import LanguageServer.Protocol.Uri (filenameToUri)
import Node.Path (resolve)
import PscIde (usages)
import PscIde.Command (Namespace(..))
import PscIde.Command as Command

getReferences ::  Notify ->
  DocumentStore ->
  Settings ->
  ServerState ->
  ReferenceParams ->
  Aff (Array Location)
getReferences notify docs _ state ({ textDocument, position }) = do
  let { port, root } = un ServerState $ state
      uri =_.uri $ un TextDocumentIdentifier textDocument
  doc <- liftEffect $ getDocument docs uri
    
  Nullable.toMaybe doc # maybe (pure []) \doc -> do
    text <- liftEffect $ getTextAtRange doc (mkRange position)
    case
      port,
      root,
      identifierAtPoint text (_.character $ un Position position)
      of
      Just port', Just root', Just { word, qualifier } -> do
        info <- getTypeInfoMaybeNew notify state uri word qualifier
        case info of
          Just (Command.TypeInfo { module', type' }) -> do
            let
              ns = case type' of
                "Type" -> NSType
                _ | endsWith "-> Type" type' -> NSType
                _ -> NSValue
            usg <- usages port' module' ns word
            liftEffect $ either (pure $ pure []) (traverse $ convLocation root')
              usg
          _ -> pure $ []
      _, _, _ -> pure $ []
  where

  convLocation :: String -> Command.TypePosition -> Effect Location
  convLocation root (Command.TypePosition { start, end, name }) = do
    uri <- filenameToUri =<< resolve [ root ] name
    pure
      $ Location
          { uri
          , range: Range { start: convPosition start, end: convPosition end }
          }

  mkRange pos =
    Range
      { start: pos # over Position (_ { character = 0 })
      , end: pos # over Position (\c -> c { character = c.character + 100 })
      }
