{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.BuildTest.Cmd
    ( MemoPaths(..)
    , EnvT
    , runEix
    , runPortageq
    , runEmerge
    , askEixPath
    , askPortageqPath
    , askEmergePath
    , runCmd
    , defaultDie
    ) where

import Control.Monad.Trans.Accum
import Control.Monad.IO.Class
import Data.Monoid (First(..))
import System.Directory (findExecutable)
import System.Exit (ExitCode(..), die)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import Distribution.Gentoo.BuildTest.Types

-- | Basic memoization for various needed executable paths
data MemoPaths = MemoPaths
    { envEixPath :: First EixPath
    , envPortageqPath :: First PortageqPath
    , envEmergePath :: First EmergePath
    } deriving (Show, Eq, Ord)

instance Semigroup MemoPaths where
    MemoPaths a1 b1 c1 <> MemoPaths a2 b2 c2
        = MemoPaths (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid MemoPaths where
    mempty = MemoPaths mempty mempty mempty

-- | Only allows for reading the memoized paths, or initializing them if they
--   have not been initialized yet
type EnvT = AccumT MemoPaths

runEix :: MonadIO m => [String] -> EnvT m String
runEix args = do
    e <- askEixPath
    runCmd e args

runPortageq :: MonadIO m => [String] -> EnvT m String
runPortageq args = do
    e <- askPortageqPath
    runCmd e args

runEmerge :: MonadIO m => [String] -> EnvT m String
runEmerge args = do
    e <- askEmergePath
    runCmd e args

askEixPath :: MonadIO m => EnvT m EixPath
askEixPath = getFirst . envEixPath <$> look >>= \case
    Just p -> pure p
    _ -> do
        p <- findEix
        p <$ add mempty { envEixPath = pure p }

askPortageqPath :: MonadIO m => EnvT m PortageqPath
askPortageqPath = getFirst . envPortageqPath <$> look >>= \case
    Just p -> pure p
    _ -> do
        p <- findPortageq
        p <$ add mempty { envPortageqPath = pure p }

askEmergePath :: MonadIO m => EnvT m EmergePath
askEmergePath = getFirst . envEmergePath <$> look >>= \case
    Just p -> pure p
    _ -> do
        p <- findEmerge
        p <$ add mempty { envEmergePath = pure p }

-- | Finds the @eix@ executable in @PATH@ and runs @eix-update@.
--   Calls 'die' if either are not found.
findEix :: MonadIO m => m EixPath
findEix = liftIO $ findExecutable "eix" >>= \case
    Nothing -> die $ unwords
        [ "Could not find \"eix\" executable."
        , "Please install app-portage/eix."
        ]
    Just exe -> findExecutable "eix-update" >>= \case
        Nothing -> die $ unwords
            [ "Could not find \"eix-update\" executable."
            , "Please install app-portage/eix."
            ]
        Just updateExe -> do
            hPutStrLn stderr "Running 'eix-update'"
            _ <- runCmd updateExe []
            pure (EixPath exe)

-- | Finds the @portageq@ executable in @PATH@. Calls 'die' if it is not found.
findPortageq :: MonadIO m => m PortageqPath
findPortageq = liftIO $ do
    mp <- findExecutable "portageq"
    case mp of
        Just p -> pure $ PortageqPath p
        Nothing -> die $ unwords
            [ "Could not find \"portageq\" executable."
            , "sys-apps/portage is required."
            ]

-- | Finds the @emerge@ executable in @PATH@. Calls 'die' if it is not found.
findEmerge :: MonadIO m => m EmergePath
findEmerge = liftIO $ do
    mp <- findExecutable "emerge"
    case mp of
        Just p -> pure $ EmergePath p
        Nothing -> die $ unlines
            [ "Could not find \"emerge\" executable."
            , "sys-apps/portage is required."
            ]

-- | Run a command and return stdout, or 'defaultDie' if it fails.
runCmd :: (MonadIO m, ToString r)
    => r
    -> [String]
    -> m String
runCmd exe args = liftIO $ do
    (ec, out, err) <- readProcessWithExitCode (toString exe) args ""
    case ec of
         ExitSuccess -> pure out
         ExitFailure _ -> defaultDie exe args ec out err

-- | Dies with a default message when a program returns an unexpected exit code
defaultDie :: (MonadIO m, ToString r)
    => r
    -> [String]
    -> ExitCode
    -> String
    -> String
    -> m a
defaultDie exe args ec out err = liftIO $ die $ unlines $ unwords <$>
    [ [ "Unexpected exit code", show ec, "when running:", show (toString exe) ]
      ++ (show <$> args)
    , [ "stdout:", show out ]
    , [ "stderr:", show err ]
    ]
