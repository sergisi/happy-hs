{
module Parser where
import Lexer
import ParserData
}

%name stateMachine
%tokentype { LexerT }
%error {happyError}

%lexer {lexWrapper} {LEOF}
%monad {Alex}


%token
   sym      { LSymbol $$ }
   bui      { LBuida }
   '|'      { LAltern }
   '.'      { LConcat }
   '*'      { LStar }
   '+'      { LPlus }
   '?'      { LExists }
   '('      { LLBrack }
   ')'      { LRBrack }
   ';'      { LSync }

%left '|'
%left '.'
%left '*' '+' '?'

%%

Line :: { [ State ] }
Line : Line ';' Exp    {$3 : $1}
     | Line ';'        { $1 }
     | Exp             { [$1] }
     | {- empty -}     { [] }


Exp :: { State }
Exp : Exp '+'     { plus $1 }
    | Exp '?'     { exist $1}
    | Exp '*'     { kleen $1}
    | Exp '.' Exp { concaten $1 $3 }
    | Exp '|' Exp {% alternative $1 $3}
    | sym         {% stateFromChar $ (Just $1)}
    | '(' Exp ')' { $2 }
    | bui         {% stateFromChar Nothing }


{


happyError :: LexerT -> Alex a
happyError tok = alexError $ "Happy error on token: " ++ show tok

}
