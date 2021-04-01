{
module Parser where
import Lexer
}

%name calc
%tokentype { LexerT }
%error {happyError}

%lexer {lexWrapper} {LEOF}
%monad {Alex}


%token
   '*'      { LMult }
   '/'      { LDiv }
   '+'      { LSum }
   '-'      { LMinus }
   '('      { LLBrack }
   ')'      { LRBrack }
   int      { LInt $$ }
   ';'      { LSync }
   rvar     { LRealReg $$ }
   ivar     { LIntReg $$ }
   '='      { LAssign }

%left '+' '-'
%left '*' '/'

%%

Line :: { [ Exp ] }
Line : Line ';' Assign {$3 : $1}
     | Line ';'        { $1 }
     | Assign          { [$1] }
     | {- empty -}     { [] }

Assign :: { Exp }
Assign : rvar '=' Exp { TRealAssign $1 $3 }
       | ivar '=' Exp { TIntAssign $1 $3 }
       | Exp          { $1 }

Exp :: { Exp }
Exp : Exp  '+' Exp { TSum $1 $3 }
    | Exp  '-' Exp { TMinus $1 $3 }
    | Exp  '*' Exp { TMult $1 $3 }
    | Exp  '/' Exp { TDiv $1 $3 }
    | int          { TVal $1 }
    | '('  Exp ')' { TBrack $2 }
    | rvar         { TRealGet $1 }
    | ivar         { TIntGet $1 }


{

data Exp = TSum Exp Exp
         | TMinus Exp Exp
         | TMult Exp Exp
         | TDiv Exp Exp
         | TVal Int
         | TBrack Exp
         | TRealAssign Char Exp
         | TIntAssign Char Exp
         | TRealGet Char
         | TIntGet Char
         deriving (Show, Eq, Ord, Read)

{-
eval :: Exp -> Int
eval (TSum a b) = eval a + eval b
eval (TMult a b) = eval a * eval b
eval (TDiv a b) = eval a `div` eval b
eval (TMinus a b) = eval a - eval b
eval (TBrack a) = eval a
eval (TVal a) = a
-}

happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

}
