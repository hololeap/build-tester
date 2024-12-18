{-# LANGUAGE LambdaCase #-}

module Distribution.Gentoo.BuildTest.Cmd where

import Control.Monad.Reader
import System.Directory (findExecutable)
import System.Exit (ExitCode(..), die)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import Distribution.Gentoo.BuildTest.Types

-- | Interface to run
runCmd :: (MonadIO m, ToString r)
    => [String]
    -> (ExitCode -> String -> String -> ReaderT r m a)
    -> ReaderT r m a
runCmd args f = do
    exe <- ask
    -- Pass empty string to stdin, as it shouldn't be needed
    (ec, out, err) <- liftIO $ readProcessWithExitCode (toString exe) args ""
    f ec out err

-- | Stuff the 'FilePath's into 'ReaderT' so it's easy to run 'findEix' and
--   'findPortagq' once.
--
--   (Because of the builtin checks, it would be expensive to run those
--   functions more than once.)
type EixT = ReaderT EixPath
type PortageqT = ReaderT PortageqPath
type EmergeT = ReaderT EmergePath

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
    runUpdate exe = do
        liftIO $ hPutStrLn stderr $ "Running 'eix-update'"
        runCmd [] $ \ec out err -> case ec of
            ExitSuccess -> pure $ EixPath exe
            ExitFailure _ -> defaultDie exe [] ec out err

findPortageq :: MonadIO m => m PortageqPath
findPortageq = liftIO $ do
    mp <- findExecutable "portageq"
    case mp of
        Just p -> pure $ PortageqPath p
        Nothing -> die $ unwords
            [ "Could not find \"portageq\" executable."
            , "sys-apps/portage is required."
            ]

findEmerge :: MonadIO m => m EmergePath
findEmerge = liftIO $ do
    mp <- findExecutable "emerge"
    case mp of
        Just p -> pure $ EmergePath p
        Nothing -> die $ unlines
            [ "Could not find \"emerge\" executable."
            , "sys-apps/portage is required."
            ]

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
