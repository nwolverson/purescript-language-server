module LanguageServer.IdePurescript.Search where
  
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError, readString, toForeign)
import Data.Foreign.Index ((!))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import IdePurescript.Modules (getQualModule)
import IdePurescript.PscIde (getCompletion', getLoadedModules)
import LanguageServer.IdePurescript.Types (MainEff, ServerState(..))
import LanguageServer.Types (DocumentStore, Settings)
import PscIde as P
import PscIde.Command (TypeInfo(..))
import PscIde.Command as C

newtype SearchResult = SearchResult { identifier :: String, typ :: String, mod :: String }

encodeSearchResult :: SearchResult -> Foreign
encodeSearchResult = toForeign

decodeSearchResult :: Foreign -> Except (NonEmptyList ForeignError) SearchResult
decodeSearchResult obj = do
  identifier <- obj ! "identifier" >>= readString
  typ <- obj ! "typ"  >>= readString
  mod <- obj ! "mod" >>= readString
  pure $ SearchResult { identifier, typ, mod }

search :: forall eff. DocumentStore -> Settings -> ServerState (MainEff eff) -> Array Foreign -> Aff (MainEff eff) Foreign
search docs config state args = case state, runExcept $ traverse readString args of 
  ServerState { port: Just port, modules }, Right [ text ] -> do
    loadedModules <- getLoadedModules port
    let getQualifiedModule = (flip getQualModule) modules
    results <- getCompletion' (Just $ C.Flex text) [] port modules.main Nothing loadedModules getQualifiedModule P.defaultCompletionOptions
    pure $ toForeign $ toResult <$> results
  _, _ -> pure $ toForeign []

  where
  toResult (TypeInfo { type', identifier, module' }) = encodeSearchResult $ SearchResult { typ: type', identifier, mod: module' }
