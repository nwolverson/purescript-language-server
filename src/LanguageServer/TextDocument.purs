module LanguageServer.TextDocument where

import Prelude
import Control.Monad.Eff (Eff)
import Data.String (drop, take)
import LanguageServer.Types (CONN, DocumentUri, Position, Range(Range))

foreign import data TextDocument :: Type

foreign import offsetAtPosition :: forall eff. TextDocument -> Position -> Eff (conn :: CONN | eff) Int

foreign import positionAtOffset :: forall eff. TextDocument -> Int -> Eff (conn :: CONN | eff) Position

foreign import getText :: forall eff. TextDocument -> Eff (conn :: CONN | eff) String

getTextAtVersion :: forall eff. TextDocument -> Eff (conn :: CONN | eff) { text :: String, version :: Number }
getTextAtVersion doc = do
    text <- getText doc
    version <- getVersion doc
    pure { text, version }

foreign import getUri :: TextDocument -> DocumentUri
foreign import getLanguageId :: TextDocument -> String
foreign import getVersion :: forall eff. TextDocument -> Eff (conn :: CONN | eff) Number
foreign import getLineCount :: forall eff. TextDocument -> Eff (conn :: CONN | eff) Int

getTextAtRange :: forall eff. TextDocument -> Range -> Eff (conn :: CONN | eff) String
getTextAtRange doc (Range { start, end }) = do
    i <- offsetAtPosition doc start
    j <- offsetAtPosition doc end
    text <- getText doc
    pure $ take (j-i) $ drop i text
