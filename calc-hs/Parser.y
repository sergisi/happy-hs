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
   rvar     { LRealVar $$ }
   ivar     { LIntVar $$ }

%left '+' '-'
%left '*' '/'

%%

Line :: { [ Exp ] }
Line : Line ';' Exp {$3 : $1}
     | Line ';' { $1 }
     | Exp      { [$1] }
     | {- empty -} { [] }

Exp :: { Exp }
Exp : Exp '+' Exp { TSum $1 $3 }
    | Exp '-' Exp { TMinus $1 $3 }
    | Exp '*' Exp { TMult $1 $3 }
    | Exp '/' Exp { TDiv $1 $3 }
    | Reg '=' Exp { TAssign $1 $3 }
    | int         { TVal $1 }
    | '(' Exp ')' { TBrack $2 }
    | var         { TReg $1 }


{

data Exp = TSum Exp Exp
         | TMinus Exp Exp
         | TMult Exp Exp
         | TDiv Exp Exp
         | TVal Int
         | TBrack Exp
         | TAssign Char
         deriving (Show, Eq, Ord, Read)

eval :: Exp -> Int
eval (TSum a b) = eval a + eval b
eval (TMult a b) = eval a * eval b
eval (TDiv a b) = eval a `div` eval b
eval (TMinus a b) = eval a - eval b
eval (TBrack a) = eval a
eval (TVal a) = a

happyError :: LexerT -> Alex a
happyError _ = alexError "Happy error"

}
