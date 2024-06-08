{
module Parser where

import Lexer
}

%name parser 
%tokentype { Token }
%error { parseError }

%token 
    num     { TokenNum $$ }
    '+'     { TokenPlus }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    and     { TokenAnd }
    if    { TokenIf } 
    then  { TokenThen }
    else  { TokenElse }
 -- aula dia 20
    or      { TokenOr }
    '*'     { TokenTimes }
    '-'     { TokenSub}

%left '+' '-'
%left '*'

%% 

Exp     : num           { Num $1 }
Exp     : Exp '+' Exp   { Add $1 $3 }
-- Exp     : '(' Exp ')'   { Paren $2 }
Exp     : Exp and Exp { And $1 $3}
Exp     : if Exp then Exp else Exp { If $2 $4 $6}
--aula dia 20
Exp     : Exp or Exp    { Or $1 $3 }
Exp     : Exp '*' Exp   { Times $1 $3 }
Exp     : Exp '-' Exp  { Sub $1 $3}
{
parseError :: [Token] -> a
parseError _ = error "Erro sintático!"
}