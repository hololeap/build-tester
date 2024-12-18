{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.Trans.Accum
import Control.Monad.IO.Class

import Distribution.Gentoo.BuildTest
import Distribution.Gentoo.BuildTest.Cmd
import Distribution.Gentoo.BuildTest.Types

main :: IO ()
main = flip evalAccumT mempty $ do
    let repo = RepoName "haskell"

    checkRepoName repo

    allPkgs <- allRepoPackages repo
    installedPkgs <- installedRepoPackages repo
    let diffPkgs = repoDifference allPkgs installedPkgs
    liftIO $ putStrLn "doot!"
