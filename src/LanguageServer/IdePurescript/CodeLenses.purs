module LanguageServer.IdePurescript.CodeLenses
  ( getCodeLenses
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, joinFiber)
import Effect.Ref (Ref)
import IdePurescript.PscIdeServer (Notify)
import LanguageServer.IdePurescript.CodeLens.ExportManagement (exportManagementCodeLenses)
import LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations (topLevelDeclarationLenses)
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Handlers (CodeLensParams, CodeLensResult)
import LanguageServer.Protocol.Types (DocumentStore, Settings, TextDocumentIdentifier(..))

getCodeLenses ∷ Notify -> Ref ServerState -> DocumentStore -> Settings -> ServerState -> CodeLensParams -> Aff (Array CodeLensResult)
getCodeLenses _notify _stateRef documentStore settings state { textDocument: TextDocumentIdentifier { uri } } = do
  let ServerState { runningRebuild } = state
  case runningRebuild of
    Just { fiber } -> do
      joinFiber fiber
    Nothing -> do
      pure unit
  
  let guard b m = if b then m else pure []

  topLevelDeclarations <- topLevelDeclarationLenses documentStore settings state uri # guard (Config.declarationTypeCodeLens settings)
  exportManagement <- exportManagementCodeLenses documentStore settings state uri # guard (Config.exportsCodeLens settings)
  pure $ topLevelDeclarations <> exportManagement 
