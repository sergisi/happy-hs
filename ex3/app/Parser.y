{
module Parser where
import Lexer
import ParserData
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
   '%'      { LMod }

%left '+' '-'
%left '*' '/' '%'

%%

Line :: { [ Exp ] }
Line : Line ';' Exp    {$3 : $1}
     | Line ';'        { $1 }
     | Exp             { [$1] }
     | {- empty -}     { [] }


Exp :: { Exp }
Exp : Exp  '+' Exp { TSum $1 $3 }
    | Exp  '-' Exp { TMinus $1 $3 }
    | Exp  '*' Exp { TMult $1 $3 }
    | Exp  '/' Exp { TDiv $1 $3 }
    | Exp  '%' Exp { TMod $1 $3 }
    | int          { TVal $1 }
    | '('  Exp ')' { TBrack $2 }


{


happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

}
