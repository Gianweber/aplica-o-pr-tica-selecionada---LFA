
module Lexer where
import Data.Char
data Expr =  BTrue
            |BFalse
            |Num Int
            |Add Expr Expr
            |And Expr Expr
            |If Expr Expr Expr
            |Var String 
            |Lam String Ty Expr
            |App Expr Expr
            --aula dia 20
            |Or Expr Expr
            |Sub Expr Expr
            |Times Expr Expr
            deriving Show  

data Ty = TBool
        | TNum --tipo do parametro e tipo do retorno
        | TFun Ty Ty
        deriving (Show, Eq) 



--Lista de Tokens vazios 
data Token = TokenNum Int
            |TokenPlus
            |TokenAnd
            |TokenIf
            |TokenThen
            |TokenElse
            |TokenTrue
            |TokenFalse
            |TokenBool 
            |TokenLParen
            |TokenRParen
            -- aula dia 20
            |TokenTimes
            |TokenOr 
            |TokenSub
            deriving Show

-- Função que recebe o código e retorna uma lista de Tokens
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    |isSpace c = lexer cs
    |isDigit c = lexNum (c:cs)
    |isAlpha c = lexkw (c:cs)

lexer ('+':cs) = TokenPlus : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 

--aula dia 20 
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs 
lexer _ = error "Lexical error: caracter invalido!  "

lexNum cs = case span isDigit cs of 
                    (num, rest) -> TokenNum (read num) : lexer rest

lexkw cs = case span isAlpha cs of
                    ("true", rest) -> TokenTrue : lexer rest 
                    ("false", rest) -> TokenFalse : lexer rest 
                    ("if", rest) -> TokenIf : lexer rest 
                    ("then", rest) -> TokenThen : lexer rest 
                    ("else", rest) -> TokenElse : lexer rest 
                    ("and", rest) -> TokenAnd : lexer rest 
                    ("bool", rest) -> TokenBool : lexer rest 
                    --
                    ("or", rest) -> TokenOr : lexer rest
                    