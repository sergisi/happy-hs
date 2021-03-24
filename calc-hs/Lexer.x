{
module Lexer
  ( LexerT(..)
  , scanner
  , Alex(..)
  , lexWrapper
  , alexError
  , runAlex
  ) where

}

%wrapper "monad"

$operators = [ \* \- \+ \/ \( \) ]
$syncToken = \=

tokens :-

$white+ { skip }
\*  { tk LMult }
\/  { tk LDiv }
\+  { tk LSum }
\-  { tk LMinus }
\(  { tk LLBrack }
\)  { tk LRBrack }
\-?[0-9]+  { token (\(_, _, _, s) len -> LInt . read $ take len s) }
\=  { tk LSync }

{

tk :: LexerT -> AlexAction LexerT
tk = token . const . const

data LexerT = LMult
            | LDiv
            | LSum
            | LMinus
            | LLBrack
            | LRBrack
            | LInt Int
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

mainOther :: IO ()
mainOther = do
  s <- getContents
  print $ scanner s

}
