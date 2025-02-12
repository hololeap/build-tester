
module Distribution.Gentoo.BuildTest.Types where

import Control.Monad.Trans.Accum
import Data.Monoid
import Data.Set (Set)
import Data.Time.Clock
import qualified Data.Text.Lazy as TL

import Distribution.Portage.Types

type Env = AccumT Paths IO

runEnv :: Env a -> IO a
runEnv = flip evalAccumT mempty

data Paths = Paths
    { envEmergePath :: First EmergePath
    , envPqueryPath :: First PqueryPath
    , envHaskellUpdaterPath :: First HaskellUpdaterPath
    } deriving (Show, Eq, Ord)

instance Semigroup Paths where
    Paths e1 p1 h1 <> Paths e2 p2 h2 = Paths (e1 <> e2) (p1 <> p2) (h1 <> h2)

instance Monoid Paths where
    mempty = Paths mempty mempty mempty

type EmergePath = FilePath
type PqueryPath = FilePath
type HaskellUpdaterPath = FilePath

addEmergePath :: EmergePath -> Env ()
addEmergePath p = add (Paths (pure p) mempty mempty)

addPqueryPath :: PqueryPath -> Env ()
addPqueryPath p = add (Paths mempty (pure p) mempty)

addHaskellUpdaterPath :: HaskellUpdaterPath -> Env ()
addHaskellUpdaterPath p = add (Paths mempty mempty (pure p))

lookEmergePath :: Env (Maybe EmergePath)
lookEmergePath = looks (getFirst . envEmergePath)

lookPqueryPath :: Env (Maybe PqueryPath)
lookPqueryPath = looks (getFirst . envPqueryPath)

lookHaskellUpdaterPath :: Env (Maybe HaskellUpdaterPath)
lookHaskellUpdaterPath = looks (getFirst . envHaskellUpdaterPath)

data PrelimEmergeResult
    = ResolveFailed UTCTime
    | TriedToDowngrade UTCTime
    | PrelimEmergeSuccess
    deriving (Show, Eq, Ord)

data EmergeResult
    = BuildFailed UTCTime
    | EmergeSuccess UTCTime
    deriving (Show, Eq, Ord)

type ResolveFailedAttempts = Set (UTCTime, Package)
type TriedToDowngradeAttempts = Set (UTCTime, Package)
type BuildFailedAttempts = Set (UTCTime, Package)
type EmergeSuccessAttempts = Set (UTCTime, Package)

type AttemptSets =
    ( ResolveFailedAttempts
    , TriedToDowngradeAttempts
    , BuildFailedAttempts
    , EmergeSuccessAttempts )

type Stdout = TL.Text
type Stderr = TL.Text
