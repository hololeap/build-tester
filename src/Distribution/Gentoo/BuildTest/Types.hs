{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Gentoo.BuildTest.Types where

import Data.Foldable
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Monoidal (MonoidalHashMap(..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck.Arbitrary

class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString Text where
    toString = T.unpack

newtype EixPath = EixPath { unEixPath :: FilePath }
    deriving (Show, Eq, Ord, ToString)

newtype PortageqPath = PortageqPath { unPortageqPath :: FilePath }
    deriving (Show, Eq, Ord, ToString)

newtype EmergePath = EmergePath { unEmergePath :: FilePath }
    deriving (Show, Eq, Ord, ToString)

newtype RepoName = RepoName { unRepoName :: String }
    deriving (Show, Eq, Ord, ToString)

newtype Category = Category { unCategory :: Text }
    deriving (Show, Eq, Ord, Hashable, ToString)

instance Arbitrary Category where
    arbitrary = Category . T.pack <$> arbitrary
    shrink (Category t) = map (Category . T.pack) $ shrink $ T.unpack t

newtype Package = Package { unPackage :: Text }
    deriving (Show, Eq, Ord, Hashable, ToString)

instance Arbitrary Package where
    arbitrary = Package . T.pack <$> arbitrary
    shrink (Package t) = map (Package . T.pack) $ shrink $ T.unpack t

type RepoPackages = MonoidalHashMap Category (HashSet Package)

repoDifference :: RepoPackages -> RepoPackages -> RepoPackages
repoDifference (MonoidalHashMap m1) (MonoidalHashMap m2)
    = MonoidalHashMap $ HM.differenceWith
        (\s1 s2 ->
            let s' = S.difference s1 s2
            in if null s' then Nothing else Just s'
        )
        m1 m2

repoSize :: RepoPackages -> Int
repoSize = foldl' (\i s -> i + length s) 0
