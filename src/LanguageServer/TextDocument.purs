module LanguageServer.TextDocument where

import Prelude
import Effect (Effect)
import LanguageServer.Types (DocumentUri, Position, Range)

foreign import data TextDocument :: Type

foreign import offsetAtPosition :: TextDocument -> Position -> Effect Int

foreign import positionAtOffset :: TextDocument -> Int -> Effect Position

foreign import getText :: TextDocument -> Effect String

getTextAtVersion :: TextDocument -> Effect { text :: String, version :: Number }
getTextAtVersion doc = do
  text <- getText doc
  version <- getVersion doc
  pure { text, version }

foreign import getUri :: TextDocument -> DocumentUri
foreign import getLanguageId :: TextDocument -> String
foreign import getVersion :: TextDocument -> Effect Number
foreign import getLineCount :: TextDocument -> Effect Int

foreign import getTextAtRange :: TextDocument -> Range -> Effect String
