{-# Language LambdaCase #-}

module Main where

import Control.Exception (bracket)
import Control.Monad.Reader
import qualified Data.HashSet as HS
import Data.IORef
import qualified Data.Set as S
import Data.Time.Clock

import Distribution.Portage.Types
import Data.Parsable

import Distribution.Gentoo.BuildTest
import Distribution.Gentoo.BuildTest.Types
import Distribution.Gentoo.BuildTest.Util

main :: IO ()
main = bracket acquire dumpSummary $ \(nis, rs, ref) ->
    runEnv $ flip runReaderT ref $ forM_ (zip [(1 :: Int) ..] rs) $ go nis

  where
    repo = Repository "haskell"

    go :: HS.HashSet Package
       -> (Int, Package)
       -> ReaderT (IORef AttemptSets) Env ()
    go nis (i,p) = do
        liftIO $ printColor Cyan $ unwords $
            [ "Trying", toString p
            , "(" ++ show i
            , "of"
            , show (HS.size nis) ++ ")..."
            ]
        liftEnv (runPrelimEmerge p) >>= \case
            ResolveFailed t -> do
                tellResolveFailed t p
                liftIO $ printColor Red
                    $ "*** " ++ toString p ++ ": Resolve failed!"
            TriedToDowngrade t -> do
                tellTriedToDowngrade t p
                liftIO $ printColor Red
                    $ "*** " ++ toString p ++ ": Tried to downgrade!"

            PrelimEmergeSuccess -> do
                liftIO $ printColor Yellow
                    $ "Preliminary emerge run succeeded..."
                liftEnv (runFinalEmerge p) >>= \case
                    BuildFailed t -> do
                        tellBuildFailed t p
                        liftIO $ printColor Red
                            $ "*** " ++ toString p ++ ": Build failed!"
                    EmergeSuccess t -> do
                        tellEmergeSuccess t p
                        liftIO $ printColor Green
                            $ "*** " ++ toString p ++ ": Success!"

                liftIO $ putStrLn ""
                liftEnv runHaskellUpdater

    acquire :: IO (HS.HashSet Package, [Package], IORef AttemptSets)
    acquire = do
        nis <- runEnv $ notInstalledPkgs repo
        rs <- randomize nis
        ref <- newIORef mempty
        pure (nis, rs, ref)

    dumpSummary
        :: (HS.HashSet Package, [Package], IORef AttemptSets)
        -> IO ()
    dumpSummary (_, _, ref) = readIORef ref >>= printColor Green . showSummary

    liftEnv :: Env a -> ReaderT r Env a
    liftEnv = lift


showSummary :: AttemptSets -> String
showSummary (rfa, tda, bfa, esa) = unlines $
    [ "Final summary:"
    , ""
    , show (S.size rfa) ++ " packages failed to resolve:"
    ] ++ showSet rfa ++
    [ ""
    , show (S.size tda) ++ " packages tried to downgrade:"
    ] ++ showSet tda ++
    [ ""
    , show (S.size bfa) ++ " packags failed to build:"
    ] ++ showSet bfa ++
    [ ""
    , show (S.size esa) ++ " packages completed successfully:"
    ] ++ showSet esa

  where
    showSet :: S.Set (UTCTime, Package) -> [String]
    showSet = map showPkg . S.toAscList

    showPkg :: (UTCTime, Package) -> String
    showPkg (t,p) = "     - " ++ toString p ++ " " ++ "(" ++ show t ++ ")"


tellResolveFailed
    :: UTCTime -> Package -> ReaderT (IORef AttemptSets) Env ()
tellResolveFailed t p =
    tellIORef (S.singleton (t,p), mempty, mempty, mempty)

tellTriedToDowngrade
    :: UTCTime -> Package -> ReaderT (IORef AttemptSets) Env ()
tellTriedToDowngrade t p =
    tellIORef (mempty, S.singleton (t,p), mempty, mempty)

tellBuildFailed
    :: UTCTime -> Package -> ReaderT (IORef AttemptSets) Env ()
tellBuildFailed t p =
    tellIORef (mempty, mempty, S.singleton (t,p), mempty)

tellEmergeSuccess
    :: UTCTime -> Package -> ReaderT (IORef AttemptSets) Env ()
tellEmergeSuccess t p =
    tellIORef (mempty, mempty, mempty, S.singleton (t,p))

tellIORef :: AttemptSets -> ReaderT (IORef AttemptSets) Env ()
tellIORef s = ask >>= \ref -> liftIO $ modifyIORef' ref (<> s)
