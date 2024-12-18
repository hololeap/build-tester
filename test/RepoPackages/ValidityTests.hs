{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RepoPackages.ValidityTests where

import Control.Applicative
import Data.Hashable
import Data.HashMap.Monoidal (MonoidalHashMap(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.Gentoo.BuildTest.Types
import Debug.Pretty.Simple

instance (Hashable k, Arbitrary k) => Arbitrary1 (HashMap k) where
    liftArbitrary = liftA2 HM.singleton arbitrary
    liftShrink = traverse

instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
    arbitrary = arbitrary1
    shrink = shrink1

deriving instance (Hashable k, Arbitrary k) => Arbitrary1 (MonoidalHashMap k)
deriving instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (MonoidalHashMap k v)

instance (Hashable a, Arbitrary a) => Arbitrary (HashSet a) where
    arbitrary = S.fromList <$> arbitrary
    shrink = map S.fromList . shrink . S.toList

validityTests :: TestTree
validityTests = testGroup "validity tests"
    [
    ]
