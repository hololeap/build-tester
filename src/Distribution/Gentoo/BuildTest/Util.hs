
module Distribution.Gentoo.BuildTest.Util
    ( randomize
    , withParseIO
    , withParseResultIO
    , printColor
    , module System.Console.ANSI.Types
    ) where

import Conduit
import Data.Foldable (toList)
import List.Shuffle
import System.Console.ANSI
import System.Console.ANSI.Types (Color(..))
import System.Exit (die)

import Text.Parsec

randomize :: Foldable t => t a -> IO [a]
randomize = shuffleIO . toList

withParseIO :: (Stream s Identity t, Show a)
    => Parsec s () a -> String -> s -> (a -> IO b) -> IO b
withParseIO p s input act = case parse p s input of
    Left err -> die $ "parse error at " ++ show err
    Right x -> act x

withParseResultIO :: Either ParseError a -> (a -> IO b) -> IO b
withParseResultIO r act = case r of
    Left e -> die $ "parse error at " ++ show e
    Right x -> act x

printColor :: Color -> String -> IO ()
printColor c s = do
    setSGR [SetColor Foreground Dull c]
    putStrLn s
    setSGR [Reset]
    putStrLn ""
