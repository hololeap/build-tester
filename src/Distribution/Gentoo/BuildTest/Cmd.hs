{-# Language LambdaCase #-}

module Distribution.Gentoo.BuildTest.Cmd where

import Conduit
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import Data.Conduit.Process
import qualified Data.List as L
import System.Directory (findExecutable)
import System.Environment (setEnv)
import System.Exit (ExitCode(..), die)
import System.IO (Handle, stdout, stderr)

import Distribution.Gentoo.BuildTest.Types
import Distribution.Gentoo.BuildTest.Util

defaultEmergeArgs :: [String]
defaultEmergeArgs =
    [ "--ignore-default-opts"
    , "--verbose"
    , "--quiet-build"
    , "--deep"
    , "--complete-graph"
    , "--oneshot"
    , "--update"
    , "--color=n" -- Need a ANSI filtering library
    , "--nospinner"
    ]

defaultPqueryArgs :: [String]
defaultPqueryArgs =
    [ "--no-version"
    ]

-- | Find the path to the @emerge@ executable or throw an error. Caches the
--   result in the case of a success. Sets @FEATURES="-getbinpkg"@ to avoid
--   it interfering with this utility.
emergePath :: Env EmergePath
emergePath = lookEmergePath >>= \case
    Just p -> pure p
    Nothing -> liftIO (findExecutable "emerge") >>= \case
        Just p -> do
            liftIO $ setEnv "FEATURES" "-getbinpkg"
            p <$ addEmergePath p
        Nothing -> liftIO $ die
            "Could not find emerge executable. Install sys-apps/portage first."

-- | Find the path to the @pquery@ executable or throw an error. Caches the
--   result in the case of a success.
pqueryPath :: Env PqueryPath
pqueryPath = lookPqueryPath >>= \case
    Just p -> pure p
    Nothing -> liftIO (findExecutable "pquery") >>= \case
        Just p -> p <$ addPqueryPath p
        Nothing -> liftIO $ die
            "Could not find pquery executable. Install sys-apps/pkgcore first."

-- | Find the path to the @haskell-updater@ executable or throw an error.
--   Caches the result in the case of a success. Sets
--   @FEATURES="-getbinpkg"@ to avoid it interfering with this utility.
haskellUpdaterPath :: Env HaskellUpdaterPath
haskellUpdaterPath = lookHaskellUpdaterPath >>= \case
    Just p -> pure p
    Nothing -> liftIO (findExecutable "haskell-updater") >>= \case
        Just p -> do
            liftIO $ setEnv "FEATURES" "-getbinpkg"
            p <$ addHaskellUpdaterPath p
        Nothing -> liftIO $ die
            "Could not find haskell-updater executable. \
            \Install app-admin/haskell-updater first."

-- | Run a command and capture stdout and stderr
runOpaque
    :: FilePath -- ^ executable path
    -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
    -> IO (ExitCode, Stdout, Stderr)
runOpaque exe args
    = sourceProcessWithStreams
        (proc exe args)
        (pure ())
        captureOutput
        captureOutput

-- | Run a command and dump stdout to @stdout@, stderr to @stderr@, also
--   capturing both streams.
runTransparent
    :: FilePath -- ^ executable path
    -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
    -> IO (ExitCode, Stdout, Stderr)
runTransparent exe args = do
    printColor Magenta $ "Running: " ++ showCmd
    sourceProcessWithStreams (proc exe args) -- { delegate_ctlc = True }
            (pure ()) (transSink stdout) (transSink stderr)
  where
    transSink :: Handle -> ConduitT BS.ByteString Void IO TL.Text
    transSink h = iterMC (BS.hPut h) .| captureOutput

    showCmd :: String
    showCmd = unwords $ exe : map showArg args

    showArg :: String -> String
    showArg arg = case words arg of
        [] -> ""
        [w] -> w
        ws -> show $ L.intercalate " " ws

captureOutput :: ConduitT BS.ByteString Void IO TL.Text
captureOutput = decodeUtf8LenientC .| sinkLazy

withCmd
    :: (FilePath -> [String] -> IO (ExitCode, Stdout, Stderr))
    -> FilePath
    -> [String]
    -> (Stdout -> IO a)
    -> IO a
withCmd run exe args act = do
    (ec, o, e) <- run exe args
    case ec of
        ExitFailure i -> liftIO $ die $ unlines $
            [ exe ++ " failed with exit code " ++ show i
            , "stdout: " ++ TL.unpack o
            , "stderr: " ++ TL.unpack e ]
        ExitSuccess -> act o
