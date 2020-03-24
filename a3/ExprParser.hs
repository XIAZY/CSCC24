module ExprParser where

import Control.Applicative

import ExprDef
import ParserLib

mainParser :: Parser Expr
mainParser = whitespaces *> expr <* eof

expr :: Parser Expr
expr = logicalOr

atom :: Parser Expr
atom = fmap LitNat natural
           <|> fmap Var var
           <|> (openParen *> expr <* closeParen)
        where
            var = identifier ["or", "assert", "while", "do"]

unary :: Parser Expr
unary = (do
            operator "-"
            i <- unary
            return (Prim1 Neg i)
        ) <|> (do
            operator "!"
            i <- unary
            return (Prim1 Not i)
        ) <|> atom

binary :: Parser Expr
binary = chainl1 operand op where
    operand = unary
    op = (do
            operator "+"
            return (Prim2 Add)
        ) <|> (do
            operator "-"
            return (Prim2 Sub))

cmp :: Parser Expr
cmp = (do
        i <- binary
        operator "=="
        j <- binary
        return (Prim2 EqNat i j)) <|> binary

logicalAnd :: Parser Expr
logicalAnd = chainr1 operand op where
    operand = cmp
    op = (do
            operator "&&"
            return (Prim2 And)
        )
logicalOr :: Parser Expr
logicalOr = chainr1 operand op where
    operand = logicalAnd
    op = (do
            operator "||"
            return (Prim2 Or)
        )