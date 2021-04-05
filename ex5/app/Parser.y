{
module Parser where
import Lexer
import ParserData
}

%name eval
%tokentype { LexerT }
%error {happyError}

%lexer {lexWrapper} {LEOF}
%monad {Alex}


%token
   '->'     { LImpl }
   '<->'    { LDImpl }
   'i'      { LConj }
   'o'      { LDisj }
   '('      { LOpenBracket }
   ')'      { LCloseBracket }
   id       { LVar $$ }
   ';'      { LSync }
   '!'      { LNeg }

%left '->' '<->'
%left 'o'
%left 'i'
%right '!'

%%

Line :: { [ Exp ] }
Line : Line ';' Exp    {$3 : $1}
     | Line ';'        { $1 }
     | Exp             { [$1] }
     | {- empty -}     { [] }


Exp :: { Exp }
Exp : Exp 'i' Exp    { TConj $1 $3 }
    | Exp 'o' Exp    { TDisj $1 $3 }
    | '!' Exp        { TNeg $2 }
    | Exp '->'  Exp  { implicate $1 $3 }
    | Exp '<->' Exp  { TConj (TBrack $ implicate $1 $3) (TBrack $ implicate $3 $1) }
    | id             { TVal $1 }
    | '(' Exp ')'    { TBrack $2 }


{




happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

}
