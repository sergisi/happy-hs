{
module Lexer
  ( LexerT(..)
  , scanner
  , Alex(..)
  , lexWrapper
  , alexError
  , runAlex
  , alexGetUserState
  , alexSetUserState
  , getLineAndColumn
  ) where

import AlexUserState
}

%wrapper "monadUserState"
$chars = [a-d]
$alternat = \|
$concaten = \.
$estrella = \*
$mes      = \+
$existeix = \?
@buida = "BUIDA" | "buida"

tokens :-

$white+    { skip }
$chars     { token (\(_, _, _, s) _ -> LSymbol $ head s)}
@buida     { tk LBuida }
$alternat  { tk LAltern }
$concaten  { tk LConcat }
$estrella  { tk LStar }
$mes       { tk LPlus }
$existeix  { tk LExists }
\(         { tk LLBrack }
\)         { tk LRBrack }
\;         { tk LSync }

{

tk :: LexerT -> AlexAction LexerT
tk = token . const . const

data LexerT = LSymbol Char
            | LBuida
            | LAltern
            | LConcat
            | LStar
            | LPlus
            | LExists
            | LLBrack
            | LRBrack
            | LSync
            | LEOF
            deriving (Show, Eq, Read, Ord)

scanner str = fmap reverse . runAlex str $ loop []

loop :: [LexerT] -> Alex [LexerT]
loop xs = do
  someToken <- alexMonadScan
  if someToken == LEOF
    then return $ xs
    else do
      loop $ someToken : xs


alexEOF :: Alex LexerT
alexEOF = return LEOF

lexWrapper :: (LexerT -> Alex a) -> Alex a
lexWrapper = (alexMonadScan >>=)


getLineAndColumn :: Alex (Int, Int)
getLineAndColumn = Alex $ \s@AlexState{alex_pos=pos} -> let (AlexPn _ line column) = pos in Right (s, (line, column))

mainOther :: IO ()
mainOther = do
  s <- getContents
  print $ scanner s

}
