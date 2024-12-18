{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.BuildTest
    ( someFunc
    ) where

import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Hashable (Hashable)
import Data.HashMap.Monoidal (MonoidalHashMap)
import qualified Data.HashMap.Monoidal as M
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode(..), die)
import System.Process (readProcessWithExitCode)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type EixPath = FilePath
type PortageqPath = FilePath
newtype RepoName = RepoName String
    deriving (Show, Eq, Ord)
newtype Category = Category Text
    deriving (Show, Eq, Ord, Hashable)
newtype Package = Package Text
    deriving (Show, Eq, Ord, Hashable)
type RepoPackages = MonoidalHashMap Category Package

-- checkRepo :: MonadIO m => RepoName -> m ()
-- checkRepo (RepoName n) = do
--

allRepoPackages :: MonadIO m
    => RepoName
    -> ReaderT EixPath m RepoPackages
allRepoPackages (RepoName n) = runCmd args $ \ec out err -> do
    case ec of
        ExitFailure _ -> do
            exe <- ask
            defaultDie exe args ec out err
        ExitSuccess -> pure $ toRepoPackages out
  where
    args = ["--only-names", "--in-overlay", n]

installedRepoPackages :: MonadIO m
    => RepoName
    -> ReaderT EixPath m RepoPackages
installedRepoPackages (RepoName n) = runCmd args $ \ec out err -> do
    case ec of
        ExitFailure _ -> do
            exe <- ask
            defaultDie exe args ec out err
        ExitSuccess -> pure $ toRepoPackages out
  where
    args = ["--only-names", "--installed-from-overlay", n]

toRepoPackages :: String -> RepoPackages
toRepoPackages = M.fromList . catMaybes . map splitLine . lines
  where
    splitLine l =
        let (c,_:p) = L.span (/= '/') l
        in if null c || null p
            then Nothing
            else Just $ (Category (T.pack c), Package (T.pack p))

checkRepoName :: MonadIO m
    => RepoName
    -> ReaderT PortageqPath m ()
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

runCmd :: MonadIO m
    => [String]
    -> (ExitCode -> String -> String -> ReaderT FilePath m a)
    -> ReaderT FilePath m a
runCmd args f = do
    exe <- ask
    -- Pass empty string to stdin, as it shouldn't be needed
    (ec, out, err) <- liftIO $ readProcessWithExitCode exe args ""
    f ec out err

-- | Finds the @eix@ executable in @PATH@ and runs @eix-update@.
--   Calls 'die' if either are not found.
findEix :: MonadIO m => m EixPath
findEix = liftIO $ findExecutable "eix" >>= \case
    Nothing -> die $ unwords
        [ "Could not find \"eix\" executable."
        , "Please install app-portage/eix."
        ]
    Just exe -> liftIO (findExecutable "eix-update") >>= \case
        Nothing -> die $ unwords
            [ "Could not find \"eix-update\" executable."
            , "Please install app-portage/eix."
            ]
        Just updateExe -> do
            runReaderT (runUpdate exe) updateExe
  where
    runUpdate exe = runCmd [] $ \ec out err -> case ec of
        ExitSuccess -> pure exe
        ExitFailure _ -> defaultDie exe [] ec out err

findPortageq :: MonadIO m => m PortageqPath
findPortageq = liftIO $ do
    mp <- findExecutable "portageq"
    case mp of
        Just p -> pure p
        Nothing -> die $ unwords
            [ "Could not find \"portageq\" executable."
            , "Are you sure you arerunning on a Gentoo system?"
            ]

defaultDie :: MonadIO m
    => FilePath
    -> [String]
    -> ExitCode
    -> String
    -> String
    -> m a
defaultDie exe args ec out err = liftIO $ die $ unlines $ unwords <$>
    [ [ "Unexpected exit code", show ec, "when running:", show exe ]
      ++ (show <$> args)
    , [ "stdout:", show out ]
    , [ "stderr:", show err ]
    ]
