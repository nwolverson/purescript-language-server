module LanguageServer.IdePurescript.Server where

import Prelude

import Data.Array (filter, head)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Data.String.Utils (lines)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, apathize, attempt, delay, error, makeAff, throwError)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import IdePurescript.Exec (findBins, getPathVar)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import IdePurescript.PscIdeServer as P
import LanguageServer.IdePurescript.Config as Config
import LanguageServer.Types (Settings)
import Node.Buffer (toString)
import Node.ChildProcess (defaultExecOptions, execFile)
import Node.Encoding (Encoding(..))
import PscIde (load)
import PscIde.Server (Executable(..))

loadAll :: Int -> Aff Unit
loadAll port = load port [] [] >>= either (throwError <<< error) (const $ pure unit)

retry :: Notify-> Int -> Aff Unit -> Aff Unit
retry logError n a | n > 0 = do
    res <- attempt a
    case res of
        Right r -> pure r
        Left err -> do
            liftEffect $ logError Info $ "Retrying starting server after 500ms: " <> show err
            delay (Milliseconds 500.0)
            retry logError (n - 1) a
retry _ _ a = a

startServer' :: Settings -> Maybe String -> Notify -> Notify -> Aff { port :: Maybe Int, quit :: Aff Unit }
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
    , port: Config.pscIdePort settings
    } (fromMaybe "" root) (Config.addNpmPath settings) cb logCb
  where
    globs = getGlob Config.srcPath <> getGlob Config.packagePath
    getGlob fn = fn settings # case _ of 
      glob | not (null glob) -> [ glob <> "/**/*.purs" ]
      _ -> []
    exe = if Config.usePurs settings then Config.pursExe settings else Config.serverExe settings

getPscPackagePaths :: Foreign -> Maybe String -> Aff (Array String)
getPscPackagePaths settings root = if not $ Config.addPscPackageSources settings then pure [] else do
  pathVar <- liftEffect $ getPathVar (Config.addNpmPath settings) (fromMaybe "" root)
  serverBins <- findBins pathVar "psc-package"
  case head serverBins of
    Just (Executable bin _) -> makeAff \cb -> do
      void $ execFile bin [ "sources" ] defaultExecOptions (\{stdout} -> do
        text <- toString UTF8 stdout
        cb $ pure $ lines text)
      pure mempty
    _ -> pure []