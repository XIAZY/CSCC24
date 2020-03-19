module SExpr where

import Control.Applicative
import ParserLib

import SExprDef

-- [6 marks]
-- Lisp and Scheme use S-expressions. A basic version of the grammar in EBNF is:
--
--     S ::= identifier
--         | "(" S { S } ")"
--
-- `identifier` is as in ParserLib.hs for simplicity, and we don't have reserved
-- words.  We're also omitting various literals like numbers and quotations.
--
-- Implement `sexpr` below for this grammar.  Produce an AST of type `SExpr`
-- (defined in SExprDef.hs).
--
-- Spaces: Follow our convention for token-level parsers: Assume no spaces
-- before, skip spaces after.  If you stick to the token-level parsers, you're
-- good.  For example this input string
--
--     "(  f  ( g  x1 y1)  (h))  "
--
-- is parsed to
--
--     List [Ident "f", List [Ident "g",Ident "x1",Ident "y1"], List [Ident "h"]]
--
-- (If you're curious about initial leading spaces, see `mainParser` below.)


sexpr :: Parser SExpr
sexpr = fmap Ident (identifier []) <|>
  (openParen 
  >> some sexpr 
  >>= \es -> closeParen 
  >> return (List es))


-- If you're interested: Initial leading spaces, as well as (lack of) trailing
-- junk, are handled by having a "main parser":

mainParser :: Parser SExpr
mainParser = whitespaces *> sexpr <* eof
