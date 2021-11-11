module LanguageServer.IdePurescript.CodeLenses where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, joinFiber)
import Effect.Ref (Ref)
import LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations (topLevelDeclarationLenses)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Handlers (CodeLensParams, CodeLensResult)
import LanguageServer.Protocol.Types (DocumentStore, Settings, TextDocumentIdentifier(..))

getCodeLenses ∷ Ref ServerState -> DocumentStore -> Settings -> ServerState -> CodeLensParams -> Aff (Array CodeLensResult)
getCodeLenses _stateRef documentStore settings state { textDocument: TextDocumentIdentifier { uri } } = do
  let ServerState { runningRebuild } = state
  case runningRebuild of
    Just { fiber } -> do
      joinFiber fiber
    Nothing -> do
      pure unit

  topLevelDeclarationLenses documentStore settings state uri
