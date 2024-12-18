module Distribution.Gentoo.BuildTest
    ( someFunc
    , allRepoPackages
    , installedRepoPackages
    , toRepoPackages
    , checkRepoName
    ) where

import Control.Monad.Reader
import qualified Data.HashMap.Monoidal as M
import qualified Data.HashSet as S
import qualified Data.List as L
import qualified Data.Text as T
import System.Exit (ExitCode(..), die)
import System.Process (readProcessWithExitCode)

import Distribution.Gentoo.BuildTest.Cmd
import Distribution.Gentoo.BuildTest.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

allRepoPackages :: MonadIO m
    => RepoName
    -> EnvT m RepoPackages
allRepoPackages (RepoName n) = do
    out <- runEix args
    pure $ toRepoPackages out
  where
    args = ["--only-names", "--in-overlay", n]

installedRepoPackages :: MonadIO m
    => RepoName
    -> EnvT m RepoPackages
installedRepoPackages (RepoName n) = do
    out <- runEix args
    pure $ toRepoPackages out
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
    -> EnvT m ()
checkRepoName (RepoName n) = do
    portageq <- askPortageqPath
    (ec, out, err) <- liftIO $ readProcessWithExitCode (toString portageq) args ""
    case ec of
        ExitSuccess -> pure ()
        ExitFailure 1 -> liftIO $ die $ unwords
            [ "Could not find a repository called"
            , show n
            ]
        ExitFailure _ -> defaultDie portageq args ec out err
  where
    args = ["get_repo_path", "/", n]

