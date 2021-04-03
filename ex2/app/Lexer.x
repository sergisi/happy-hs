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

tokens :-

$white+    { skip }
\*         { tk LMult }
"div"      { tk LDiv }
"mod"      { tk LMod }
\+         { tk LSum }
\-         { tk LMinus }
\(         { tk LLBrack }
\)         { tk LRBrack }
\-?[0-9]+  { token (\(_, _, _, s) len -> LInt . read $ take len s) }
\;         { tk LSync }

{

tk :: LexerT -> AlexAction LexerT
tk = token . const . const

data LexerT = LMult
            | LDiv
            | LMod
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


getLineAndColumn :: Alex (Int, Int)
getLineAndColumn = Alex $ \s@AlexState{alex_pos=pos} -> let (AlexPn _ line column) = pos in Right (s, (line, column))

mainOther :: IO ()
mainOther = do
  s <- getContents
  print $ scanner s

}
