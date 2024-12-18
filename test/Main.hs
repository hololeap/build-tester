{-# Language CPP #-}

module Main (main) where

import Test.Tasty

import qualified RepoPackages.ValidityTests as RepoPkgs
#if defined(GENTOO_TESTS)
import qualified RepoPackages.GentooTests as RepoPkgs
#endif

main :: IO ()
main = defaultMain $ testGroup "build-tester tests"
    [ testGroup "RepoPackages" repoPkgsTests ]

repoPkgsTests :: [TestTree]
repoPkgsTests =
    [ RepoPkgs.validityTests
#if defined(GENTOO_TESTS)
    , RepoPkgs.gentooTests
#endif
    ]
