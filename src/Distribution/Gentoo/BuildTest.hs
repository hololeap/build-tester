module Distribution.Gentoo.BuildTest
    ( someFunc
    , allRepoPackages
    , installedRepoPackages
    , toRepoPackages
    , checkRepoName
    , findEix
    , findPortageq
    ) where

import Control.Monad.Reader
import qualified Data.HashMap.Monoidal as M
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.Text as T
import System.Exit (ExitCode(..), die)

import Distribution.Gentoo.BuildTest.Cmd
import Distribution.Gentoo.BuildTest.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

allRepoPackages :: MonadIO m
    => RepoName
    -> EixT m RepoPackages
allRepoPackages (RepoName n) = runCmd args $ \ec out err -> do
    case ec of
        ExitFailure _ -> ask >>= \exe -> defaultDie exe args ec out err
        ExitSuccess -> pure $ toRepoPackages out
  where
    args = ["--only-names", "--in-overlay", n]

installedRepoPackages :: MonadIO m
    => RepoName
    -> EixT m RepoPackages
installedRepoPackages (RepoName n) = runCmd args $ \ec out err -> do
    case ec of
        ExitFailure _ -> do
            exe <- ask
            defaultDie (toString exe) args ec out err
        ExitSuccess -> pure $ toRepoPackages out
  where
    args = ["--only-names", "--installed-from-overlay", n]

toRepoPackages :: String -> RepoPackages
toRepoPackages = foldMap splitLine . lines
  where
    splitLine l =
        case L.span (/= '/') l of
            (c,_:p) -> if null c || null p
                then mempty
                else M.singleton (Category (T.pack c)) (S.singleton (Package (T.pack p)))
            _ -> mempty

checkRepoName :: MonadIO m
    => RepoName
    -> PortageqT m ()
checkRepoName (RepoName n) = do
    let args = ["get_repo_path", "/", n]
    exe <- ask
    runCmd args $ \ec out err -> case ec of
        ExitSuccess -> pure ()
        ExitFailure 1 -> liftIO $ die $ unwords
            [ "Could not find a repository called"
            , show n
            ]
        ExitFailure _ -> defaultDie exe args ec out err

