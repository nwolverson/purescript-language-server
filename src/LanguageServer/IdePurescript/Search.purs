module LanguageServer.IdePurescript.Search
  ( SearchResult
  , decodeSearchResult
  , search
  )
  where

import Prelude
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign (Foreign, ForeignError, readString, unsafeToForeign)
import Foreign.Index ((!))
import IdePurescript.Modules (getQualModule)
import IdePurescript.PscIde (getCompletion', getLoadedModules)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Types (DocumentStore, Settings)
import PscIde as P
import PscIde.Command (TypeInfo(..))
import PscIde.Command as C

newtype SearchResult
  = SearchResult { identifier :: String, typ :: String, mod :: String }

encodeSearchResult :: SearchResult -> Foreign
encodeSearchResult = unsafeToForeign

decodeSearchResult :: Foreign -> Except (NonEmptyList ForeignError) SearchResult
decodeSearchResult obj = do
  identifier <- obj ! "identifier" >>= readString
  typ <- obj ! "typ" >>= readString
  mod <- obj ! "mod" >>= readString
  pure $ SearchResult { identifier, typ, mod }

search :: DocumentStore -> Settings -> ServerState -> Array Foreign -> Aff Foreign
search _ _ state args = case state, runExcept $ traverse readString args of
  ServerState { port: Just port, modules }, Right [ text ] -> do
    loadedModules <- getLoadedModules port
    let getQualifiedModule = (flip getQualModule) modules
    results <- getCompletion' (Just $ C.Flex text) [] port modules.main Nothing loadedModules getQualifiedModule P.defaultCompletionOptions
    pure $ unsafeToForeign $ toResult <$> results
  _, _ -> pure $ unsafeToForeign []

  where
  toResult (TypeInfo { type', identifier, module' }) = encodeSearchResult $ SearchResult { typ: type', identifier, mod: module' }
