{

-- Detalhes dessa implementação podem ser encontrados em:
-- https://www.haskell.org/happy/doc/html/sec-using.html

module Parser where
import Data.Char

}

%name parser
%tokentype { Token }
%error { parseError }

%token 
  true  { TokenTrue }
  false { TokenFalse }
  num		{ TokenNum $$ }
  '+'		{ TokenPlus }
  '&'   { TokenAnd }
  '?'   { TokenOr }
  '*'   { TokenMult }
  '|'   { TokenPair }
  '{'   { TokenPairOpen } 
  '}'   { TokenPairClose } 
  "fst" { TokenFirst }
  "snd" { TokenSecond }

%%

Exp	: true 		{ BTrue } 
        | false { BFalse }   
        | num		{ Num $1 }
        | Exp '+' Exp	{ Add $1 $3 }
        | Exp '&' Exp { And $1 $3 }
        | Exp '?' Exp { Or $1 $3 }
        | Exp '*' Exp { Mult $1 $3 }
        | '{' Exp '|' Exp '}' { Pair $2 $4 }
        | "fst" Exp { First $2 }
        | "snd" Exp { Second $2 }

---------------------------------
{

parseError :: [Token] -> a
parseError _ = error "Syntax error: sequência de caracteres inválida!"

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | And Expr Expr
          | If Expr Expr Expr
          | Mult Expr Expr
          | Or Expr Expr
          | Pair Expr Expr
          | First Expr
          | Second Expr
          deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenPlus
           | TokenAnd
           | TokenOr
           | TokenMult
           | TokenPair
           | TokenPairOpen
           | TokenPairClose
           | TokenFirst
           | TokenSecond
           deriving Show

-- Analisador léxico (lê o código e converte em uma lista de tokens)
-- Adicionar ao final para os outros operadores
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isSpace c = lexer cs
     | isAlpha c = lexBool (c:cs)
     | isDigit c = lexNum (c:cs)

lexer ('+':cs) = TokenPlus : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('?':cs) = TokenOr : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('|':cs) = TokenPair : lexer cs

lexer ('{':cs) = TokenPairOpen : lexer cs
lexer ('}':cs) = TokenPairClose : lexer cs

lexer _ = error "Lexical error: caracter inválido!"

lexBool cs = case span isAlpha cs of
               ("true", rest) -> TokenTrue : lexer rest
               ("false", rest) -> TokenFalse : lexer rest
               ("fst", rest) -> TokenFirst : lexer rest
               ("snd", rest) -> TokenSecond : lexer rest

-- Lê um token numérico 
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

}


