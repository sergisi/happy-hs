module Main where

import Parser
import ParserData
import Lexer

main :: IO ()
main =
  do
    s <- getContents
    print $ withSimplify s

withoutSimplify s = runAlex s $ fmap reverse calc

withSimplify s = runAlex s $ fmap (fmap simplify . reverse) calc
