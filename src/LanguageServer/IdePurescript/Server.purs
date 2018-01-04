module LanguageServer.IdePurescript.Server where

import Prelude

import Control.Monad.Aff (Aff, attempt, delay)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import IdePurescript.PscIdeServer as P
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.Types (Settings)

retry :: forall eff. (Notify eff) -> Int -> Aff eff Unit -> Aff eff Unit
retry logError n a | n > 0 = do
    res <- attempt a
    case res of
        Right r -> pure r
        Left err -> do
            liftEff $ logError Info $ "Retrying starting server after 500ms: " <> show err
            delay (Milliseconds 500.0)
            retry logError (n - 1) a
retry _ _ a = a

startServer' :: forall eff eff'. Settings -> Maybe String -> Notify (P.ServerEff eff) -> Notify (P.ServerEff eff) -> Aff (P.ServerEff eff) { port:: Maybe Int, quit:: P.QuitCallback eff' }
startServer' settings root cb logCb =
  P.startServer'
    { exe
    , combinedExe: Config.usePurs settings
    , glob: globs
    , logLevel: Config.logLevel settings
    , editorMode: Config.editorMode settings
    , polling: Config.polling settings
    , outputDirectory: Nothing
    } (fromMaybe "" root) (Config.addNpmPath settings) cb logCb
  where
    globs = [Config.srcPath settings <> "/**/*.purs", Config.packagePath settings <> "/**/*.purs"]
    exe = if Config.usePurs settings then Config.pursExe settings else Config.serverExe settings
