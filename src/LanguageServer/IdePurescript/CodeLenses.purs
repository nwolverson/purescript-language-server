module LanguageServer.IdePurescript.CodeLenses where

import Prelude

import Data.Foldable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import LanguageServer.Protocol.Console (log)
import LanguageServer.Protocol.Handlers (CodeLensParams, CodeLensResult)
import LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations (topLevelDeclarationCodeLenses)
import LanguageServer.IdePurescript.Types (ServerState(..))
import LanguageServer.Protocol.Types (DocumentStore, Settings, TextDocumentIdentifier(..))

getCodeLenses ∷ Ref ServerState -> DocumentStore -> Settings -> ServerState -> CodeLensParams -> Aff (Array CodeLensResult)
getCodeLenses _stateRef documentStore _ state { textDocument: TextDocumentIdentifier { uri } } = do
  let ServerState { conn, diagnostics } = state
  liftEffect $ for_ conn \c -> log c "code lenses"

  topLevelDeclarations <- topLevelDeclarationCodeLenses diagnostics uri
  pure topLevelDeclarations
