
module Distribution.Gentoo.BuildTest
    ( runFinalEmerge
    , runPrelimEmerge
    , runHaskellUpdater
    , notInstalledPkgs
    ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Accum
import Data.HashSet (HashSet)
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.HashSet as S
import qualified Data.Text.Lazy as TL
import Data.Time.Clock
import System.Exit

import Distribution.Portage.Types
import Data.Parsable

import Distribution.Gentoo.BuildTest.Cmd
import Distribution.Gentoo.BuildTest.Types
import Distribution.Gentoo.BuildTest.Util

runFinalEmerge :: Package -> Env EmergeResult
runFinalEmerge pkg = do
    t <- liftIO getCurrentTime
    emerge <- emergePath
    (ec, _, _) <- liftIO $ runTransparent emerge args
    pure $ case ec of
        ExitFailure _ -> BuildFailed t
        ExitSuccess -> EmergeSuccess t
    where args = defaultEmergeArgs ++ ["--keep-going=y", toString pkg]

runPrelimEmerge :: Package -> Env PrelimEmergeResult
runPrelimEmerge pkg = do
    t <- liftIO getCurrentTime
    emerge <- emergePath
    (ec, o, _) <- liftIO $ runTransparent emerge args
    case ec of
        ExitFailure _ -> pure $ ResolveFailed t
        ExitSuccess ->
            liftIO $ withParseResultIO (parseDowngrades o) $ \b -> pure
                $ if b then TriedToDowngrade t else PrelimEmergeSuccess
    where args = defaultEmergeArgs ++ ["--pretend", toString pkg]

runHaskellUpdater :: Env ()
runHaskellUpdater = do
    hu <- haskellUpdaterPath
    liftIO $ withCmd runTransparent hu args $ \_ -> pure ()
  where
    args = "--" :
        [ "--ignore-default-opts"
        , "--verbose"
        , "--quiet-build"
        , "--color=n" -- Need a ANSI filtering library
        , "--nospinner"
        ]

notInstalledPkgs :: Repository -> Env (HashSet Package)
notInstalledPkgs repo = S.difference <$> repoPkgs repo <*> installedPkgs

repoPkgs :: Repository -> Env (HashSet Package)
repoPkgs (Repository repo) = do
    pquery <- pqueryPath
    liftIO $ withCmd runOpaque pquery args $ \o ->
        withParseResultIO (parsePquery o) pure
    where args = defaultPqueryArgs ++ ["--repo", repo]

installedPkgs :: Env (HashSet Package)
installedPkgs = do
    pquery <- pqueryPath
    liftIO $ withCmd runOpaque pquery args $ \o ->
        withParseResultIO (parsePquery o) pure
    where args = defaultPqueryArgs ++ ["--installed"]

parseDowngrades :: TL.Text -> Either ParseError Bool
parseDowngrades t
    = or . catMaybes <$> evalAccumT (traverse parseLine (TL.lines t)) (Sum 1)
  where
    parseLine :: TL.Text -> AccumT (Sum Int) (Either ParseError) (Maybe Bool)
    parseLine l = do
        Sum i <- look
        add (Sum 1)
        lift $ runParser (optionMaybe dgParser) () ("line " ++ show i) l

    dgParser :: Parsec TL.Text () Bool
    dgParser = do
        _ <- char '['
        _ <- try (string "ebuild") <|> string "binary"
        _ <- count 5 anyChar
        b <- (True <$ try (string "UD")) <|> (False <$ count 2 anyChar)
        _ <- anyChar
        _ <- char ']'
        pure b

parsePquery :: TL.Text -> Either ParseError (HashSet Package)
parsePquery t =
    S.fromList <$> evalAccumT (traverse parseLine (TL.lines t)) (Sum 1)
  where
    parseLine :: TL.Text -> AccumT (Sum Int) (Either ParseError) Package
    parseLine l = do
        Sum i <- look
        add (Sum 1)
        lift $ runParsable ("line " ++ show i) (TL.unpack l)
