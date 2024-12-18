{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module RepoPackages.GentooTests where

import Control.Monad.Trans.Accum
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Monoidal as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pretty.Simple

import Distribution.Gentoo.BuildTest
import Distribution.Gentoo.BuildTest.Types

repo :: String
repo = "haskell"

gentooTests :: TestTree
gentooTests = testCase ("live ::" ++ repo ++ " packages") $ flip evalAccumT mempty $ do
    checkRepoName (RepoName repo)
    allPkgs <- allRepoPackages (RepoName repo)
    installedPkgs <- installedRepoPackages (RepoName repo)
    let diffPkgs = repoDifference allPkgs installedPkgs
    liftIO $ putStrLn ""
    forM_
        [ ("All", repoSize allPkgs)
        , ("Installed", repoSize installedPkgs)
        , ("Not-installed", repoSize diffPkgs)
        ]
        $ \(d,m) -> do
            liftIO $ putStr $ d ++ " ::" ++ repo ++ " packages: "
            pPrintForceColor m
