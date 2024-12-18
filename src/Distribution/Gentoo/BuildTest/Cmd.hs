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

import Conduit
import Control.Monad.Trans.Accum
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit.Process (sourceProcessWithStreams)
import Data.Monoid (First(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import System.Directory (findExecutable)
import System.Exit (ExitCode(..), die)
import System.IO (hPutStrLn, stdout, stderr)
import System.Process

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

runEix :: MonadIO m => [String] -> EnvT m (Text, Text, ExitCode)
runEix args = do
    e <- askEixPath
    liftIO $ runCmd e args

runPortageq :: MonadIO m => [String] -> EnvT m (Text, Text, ExitCode)
runPortageq args = do
    e <- askPortageqPath
    liftIO $ runCmd e args

runEmerge :: MonadIO m => [String] -> EnvT m (Text, Text, ExitCode)
runEmerge args = do
    e <- askEmergePath
    liftIO $ runCmd e args

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

-- | Run a command, transparently sending stdout and stderr to their
--   respective handles. Returns stdout, stderr, and exit code.
runCmd :: ToString r
    => r
    -> [String]
    -> IO (Text, Text, ExitCode)
runCmd exe args = do
    let cp = (proc (toString exe) args)
            { delegate_ctlc = True }
    (ec, bOut, bErr) <- sourceProcessWithStreams cp cIn cOut cErr
    pure (toText bOut, toText bErr, ec)
  where
    toText :: Builder -> Text
    toText = decodeUtf8Lenient . BSL.toStrict . toLazyByteString

    cIn :: ConduitT () ByteString IO ()
    cIn = pure ()

    cOut :: ConduitT ByteString Void IO Builder
    cOut = iterMC (BS.hPut stdout) .| foldMapC BSB.byteString

    cErr :: ConduitT ByteString Void IO Builder
    cErr = iterMC (BS.hPut stderr) .| foldMapC BSB.byteString

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
