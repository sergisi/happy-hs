module Main where

import Parser
import ParserData
import Lexer

main :: IO ()
main =
  do
    s <- getContents
    let x = reverse <$> runAlex s stateMachine
    case x of
      Left y -> print y
      Right x -> putStrLn . unlines $ map repr x
