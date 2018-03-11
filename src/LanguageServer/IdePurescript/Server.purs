module LanguageServer.IdePurescript.Server where

import Prelude

import Control.Monad.Aff (Aff, apathize, attempt, delay, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (filter, head)
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds(..))
import IdePurescript.Exec (findBins, getPathVar)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import IdePurescript.PscIdeServer as P
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.Types (Settings)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import PscIde (NET, load)
import PscIde.Server (Executable(..))

loadAll :: forall e. Int -> Aff (net :: NET | e) Unit
loadAll port = apathize $ load port [] []

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
startServer' settings root cb logCb = do
  pscpGlob <- getPscPackagePaths settings root
  P.startServer'
    { exe
    , combinedExe: Config.usePurs settings
    , glob: filter (not <<< null) $ globs <> pscpGlob
    , logLevel: Config.logLevel settings
    , editorMode: Config.editorMode settings
    , polling: Config.polling settings
    , outputDirectory: Config.outputDirectory settings
    } (fromMaybe "" root) (Config.addNpmPath settings) cb logCb
  where
    globs = getGlob Config.srcPath <> getGlob Config.packagePath
    getGlob fn = fn settings # case _ of 
      glob | not (null glob) -> [ glob <> "/**/*.purs" ]
      _ -> []
    exe = if Config.usePurs settings then Config.pursExe settings else Config.serverExe settings

getPscPackagePaths :: forall eff. Foreign -> Maybe String -> Aff (P.ServerEff eff) (Array String)
getPscPackagePaths settings root = if not $ Config.addPscPackageSources settings then pure [] else do
  pathVar <- liftEff $ getPathVar (Config.addNpmPath settings) (fromMaybe "" root)
  serverBins <- findBins pathVar "psc-package"
  case head serverBins of
    Just (Executable bin _) -> makeAff \err succ ->
      execFile bin [ "sources" ] defaultExecOptions (\{stdout} -> do
        text <- toString UTF8 stdout
        succ $ lines text)
    _ -> pure []