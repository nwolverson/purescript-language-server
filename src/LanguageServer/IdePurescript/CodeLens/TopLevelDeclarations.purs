module LanguageServer.IdePurescript.CodeLens.TopLevelDeclarations where

import Prelude

import Data.Array (mapMaybe, mapWithIndex)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable as Nullable
import Data.String (joinWith)
import Data.String.Utils as String
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import LanguageServer.Protocol.Handlers (CodeLensResult)
import LanguageServer.IdePurescript.Build (positionToRange)
import LanguageServer.IdePurescript.Commands (cmdName, replaceSuggestionCmd)
import LanguageServer.Protocol.Types (Command(..), DocumentUri(..))
import PscIde.Command (PscSuggestion(..), RebuildError(..))

topLevelDeclarationCodeLenses ∷
  Object (Array RebuildError) -> DocumentUri -> Aff (Array CodeLensResult)
topLevelDeclarationCodeLenses diagnostics uri = ado
  let fileDiagnostics = Object.lookup (un DocumentUri uri) diagnostics # fold
  let addTypeDefinitions = addTypeDefinitionCodeLenses uri fileDiagnostics
  in addTypeDefinitions

addTypeDefinitionCodeLenses ∷ DocumentUri -> Array RebuildError -> Array CodeLensResult
addTypeDefinitionCodeLenses docUri =
  mapMaybe case _ of
    RebuildError
      { errorCode: "MissingTypeDeclaration"
    , suggestion:
      Just
      ( PscSuggestion
        { replacement: signature, replaceRange: Just range }
    )
    } -> Just (mkCodeLensResult range signature)
    _ -> Nothing
  where
  mkCodeLensResult rangePosition signature = do
    let range = positionToRange rangePosition
    { range
    , command: Nullable.notNull (mkReplaceCommand signature range)
    , data: (Nullable.null # unsafeToForeign)
    }

  mkReplaceCommand signature range =
    Command
      { command: cmdName replaceSuggestionCmd
      , title: signature
      , arguments:
        Nullable.notNull
          [ unsafeToForeign docUri
          , unsafeToForeign (ensureSpaceAfterFirstLine signature)
          , unsafeToForeign range
          ]
      }

ensureSpaceAfterFirstLine ∷ String -> String
ensureSpaceAfterFirstLine = String.lines >>> mapWithIndex prependSpaceIfNecessary >>> joinWith "\n"
  where
  prependSpaceIfNecessary i s = if i == 0 || String.startsWith " " s then s else " " <> s