module IdePurescript.Build where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (uncons)
import Data.Bifunctor (bimap)
import Data.Either (either, Either(..))
import Data.Foldable (find)
import Data.List as List
import Data.Maybe (maybe, Maybe(..))
import Data.String (Pattern(Pattern), split, indexOf)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (catchException)
import Effect.Ref as Ref
import Foreign.Object as Object
import IdePurescript.Exec (findBins, getPathVar)
import IdePurescript.PscErrors (PscResult(..), parsePscOutput)
import IdePurescript.PscIdeServer (ErrorLevel(..), Notify)
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding as Encoding
import Node.Stream as S
import PscIde as P
import PscIde.Command (RebuildResult(..), CodegenTarget(..))
import PscIde.Server (Executable(Executable))

type BuildOptions =
  { command :: Command
  , directory :: String
  , useNpmDir :: Boolean
  }

data Command = Command String (Array String)

type BuildResult =
  { errors :: PscResult
  , success :: Boolean
  }

spawn :: BuildOptions -> Aff { cmdBins :: Array Executable, cp :: Maybe ChildProcess }
spawn { command: Command cmd args, directory, useNpmDir } = do
  pathVar <- liftEffect $ getPathVar useNpmDir directory
  cmdBins <- findBins pathVar cmd
  cp <- liftEffect $ case uncons cmdBins of
    Just { head: Executable cmdBin _ } -> Just <$>
      CP.spawn cmdBin args (CP.defaultSpawnOptions { cwd = Just directory, env = Just (Object.fromFoldable $ List.singleton $ Tuple "PATH" $ either identity identity pathVar) })
    _ -> pure Nothing
  pure { cmdBins, cp }

build :: Notify -> BuildOptions -> Aff BuildResult
build logCb buildOptions@{ command: Command cmd args, directory, useNpmDir } = do
  { cmdBins, cp: cp' } <- spawn buildOptions
  makeAff $ \cb -> do
    logCb Info $ "Resolved build command (1st is used): "
    traverse_ (\(Executable x vv) -> do
      logCb Info $ x <> maybe "" (": " <> _) vv) cmdBins
    case cp' of
      Nothing -> cb $ Left $ error $ "Didn't find command in PATH: " <> cmd
      Just cp -> do
        CP.onError cp (cb <<< Left <<< CP.toStandardError)
        let stderr = CP.stderr cp
        result <- Ref.new ""
        let res :: String -> Effect Unit
            res s = Ref.modify_ (\acc -> acc<>s) result

        catchException (cb <<< Left) $ S.onDataString stderr Encoding.UTF8 res
        CP.onClose cp (\exit -> case exit of
          CP.Normally n | n == 0 || n == 1 -> do
            pscOutput <- Ref.read result
            let lines = split (Pattern "\n") pscOutput
                json = find (\s -> indexOf (Pattern "{\"") s == Just 0) lines
            case parsePscOutput <$> json of
              Just (Left e) -> cb $ Left $ error e
              Just (Right r) -> cb $ Right $ { errors: r, success: n == 0 }
              Nothing -> cb $ Left $ error "Didn't find JSON output"
          _ -> cb $ Left $ error "Process exited abnormally")
    pure mempty

rebuild :: Int -> String -> Maybe (Array CodegenTarget) -> Aff BuildResult
rebuild port file targets = do
  res <- P.rebuild port file (Just file) targets
  either
    (throwError <<< error)
    (pure <<< onResult)
    res
  where

  onResult :: Either RebuildResult RebuildResult -> BuildResult
  onResult =
    either (\errors -> { errors: PscResult { errors, warnings: [] }, success: true })
           (\warnings -> { errors: PscResult { errors: [], warnings }, success: true  })
    <<<
    bimap unwrap unwrap
    where
    unwrap (RebuildResult r) = r
