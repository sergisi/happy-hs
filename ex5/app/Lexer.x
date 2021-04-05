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

$complementari = [\! ¬]
$id = [A-Z]
@impl = "->"
@dobleimpl = "<->"
$andOperator = [i∧]
$orOperator = [o∨]
@multilineCommentStart = "{-"
@multilineCommentEnd = "-}"
@inlineComment = "--" .*

tokens :-
<0> {
$white                 { skip }
@impl                  { tk LImpl }
@dobleimpl             { tk LDImpl }
$id                    { token (\(_, _, _, s) _ -> LVar $ head s) }
\(                     { tk LOpenBracket }
\)                     { tk LCloseBracket }
$andOperator           { tk LConj }
$orOperator            { tk LDisj }
$complementari         { tk LNeg }
\;                     { tk LSync }
@multilineCommentStart { begin state_comment }
@inlineComment         { skip }
}
<state_comment> {
    @multilineCommentEnd { begin 0 }
    -- without the next two rules it would break the comments if
    -- it werent at the start of a new line.
    [^\- \}] { skip }
    "-"      { skip }
    "}"      { skip }
    \n       { skip }
}
{

tk :: LexerT -> AlexAction LexerT
tk = token . const . const

data LexerT = LNeg
            | LConj
            | LDisj
            | LImpl
            | LDImpl -- Doble implmicació
            | LVar Char
            | LOpenBracket
            | LCloseBracket
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
