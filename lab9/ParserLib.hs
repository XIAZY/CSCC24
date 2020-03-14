-- | Library of parser definition and operations.
module ParserLib where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.List

data Parser a = MkParser (String -> Maybe (String, a))
    -- Function from input string to:
    --
    -- * Nothing, if failure (syntax error);
    -- * Just (unconsumed input, answer), if success.

unParser :: Parser a -> String -> Maybe (String, a)
unParser (MkParser sf) = sf

-- Monadic Parsing in Haskell uses [] instead of Maybe to support ambiguous
-- grammars and multiple answers.

-- | Use a parser on an input string.
runParser :: Parser a -> String -> Maybe a
runParser (MkParser sf) inp = case sf inp of
                                Nothing -> Nothing
                                Just (_, a) -> Just a
                              -- OR: fmap (\(_,a) -> a) (sf inp)


-----------------------------
-- Character-level primitives
-----------------------------

-- | Read a character and return. Failure if input is empty.
anyChar :: Parser Char
anyChar = MkParser sf
  where
    sf "" = Nothing
    sf (c:cs) = Just (cs, c)

-- | Read a character and check against the given character.
char :: Char -> Parser Char
char wanted = MkParser sf
  where
    sf (c:cs) | c == wanted = Just (cs, c)
    sf _ = Nothing

-- | Read a character and check against the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = MkParser sf
  where
    sf (c:cs) | pred c = Just (cs, c)
    sf _ = Nothing

-- | Expect the input to be empty.
eof :: Parser ()
eof = MkParser sf
  where
    sf "" = Just ("", ())
    sf _ = Nothing


-- But you have to compose smaller parsers to build larger parsers and to return
-- more interesting answers, e.g., abstract syntax trees.
--
-- This is what fmap, pure, <*>, >>= are for.  And there are more...

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (MkParser sf) = MkParser sfb
      where
        sfb inp = case sf inp of
                    Nothing -> Nothing
                    Just (rest, a) -> Just (rest, f a)
                  -- OR: fmap (\(rest, a) -> (rest, f a)) (sf inp)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = MkParser (\inp -> Just (inp, a))

    -- liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    -- Consider the 1st parser to be stage 1, 2nd parser stage 2.
    liftA2 op (MkParser sf1) p2 = MkParser g
      where
        g inp = case sf1 inp of
                  Nothing -> Nothing
                  Just (middle, a) ->
                      case unParser p2 middle of
                        Nothing -> Nothing
                        Just (rest, b) -> Just (rest, op a b)

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    MkParser sf1 >>= k = MkParser g
      where
        g inp = case sf1 inp of
                  Nothing -> Nothing
                  Just (rest, a) -> unParser (k a) rest

instance Alternative Parser where
    -- empty :: Parser a
    -- Always fail.
    empty = MkParser (\_ -> Nothing)

    -- (<|>) :: Parser a -> Parser a -> Parser a
    -- Try the 1st one. If success, done; if failure, do the 2nd one
    MkParser sf1 <|> p2 = MkParser g
      where
        g inp = case sf1 inp of
                  Nothing -> unParser p2 inp
                  j -> j        -- the Just case

    -- many :: Parser a -> Parser [a]
    -- 0 or more times, maximum munch, collect the answers into a list.
    -- Can use default implementation. And it goes as:
    many p = some p <|> pure []
    -- How to make sense of it: To repeat 0 or more times, first try 1 or more
    -- times!  If that fails, then we know it's 0 times, and the answer is the
    -- empty list.

    -- some :: Parser a -> Parser [a]
    -- 1 or more times, maximum munch, collect the answers into a list.
    -- Can use default implementation. And it goes as:
    some p = liftA2 (:) p (many p)
    -- How to make sense of it: To repeat 1 or more times, do 1 time, then 0 or
    -- more times!  Use liftA2 to chain up and collect answers.


-- | Space or tab or newline (unix and windows).
whitespace :: Parser Char
whitespace = satisfy (\c -> c `elem` ['\t', '\n', '\r', ' '])

-- | Consume zero or more whitespaces, maximum munch.
whitespaces :: Parser String
whitespaces = many whitespace


-------------------------
-- Token-level primitives
-------------------------

-- | Read a natural number (non-negative integer), then skip trailing spaces.
natural :: Parser Integer
natural = fmap read (some (satisfy isDigit)) <* whitespaces
-- read :: Read a => String -> a
-- For converting string to your data type, assuming valid string.  Integer
-- is an instance of Read, and our string is valid, so we can use read.

-- | Read an identifier, then skip trailing spaces.  Disallow the listed keywords.
identifier :: [String] -> Parser String
identifier keywords =
    satisfy isAlpha
    >>= \c -> many (satisfy isAlphaNum)
    >>= \cs -> whitespaces
    >> let str = c:cs
    in if str `elem` keywords then empty else return str

-- | Read the wanted keyword, then skip trailing spaces.
keyword :: String -> Parser String
keyword wanted =
    satisfy isAlpha
    >>= \c -> many (satisfy isAlphaNum)
    >>= \cs -> whitespaces
    *> if c:cs == wanted then return wanted else empty

-- | Read something that looks like an operator, then skip trailing spaces.
anyOperator :: Parser String
anyOperator = some (satisfy symChar) <* whitespaces
  where
    symChar c = c `elem` "=/<>&|+-*%\\"

-- | Read the wanted operator, then skip trailing spaces.
operator :: String -> Parser String
operator wanted =
    anyOperator
    >>= \sym -> if sym == wanted then return wanted else empty

-- | Open and close parentheses.
openParen, closeParen :: Parser Char
openParen = char '(' <* whitespaces
closeParen = char ')' <* whitespaces


---------------------------------------
-- Canned solutions for infix operators
---------------------------------------

-- | One or more operands separated by an operator. Apply the operator(s) in a
-- right-associating way.
chainr1 :: Parser a               -- ^ operand parser
        -> Parser (a -> a -> a)   -- ^ operator parser
        -> Parser a               -- ^ whole answer
chainr1 getArg getOp = liftA2 link
                       getArg
                       (optional
                         (liftA2 (,) getOp (chainr1 getArg getOp)))
  where
    link x Nothing = x
    link x (Just (op,y)) = op x y

-- | One or more operands separated by an operator. Apply the operator(s) in a
-- left-associating way.
chainl1 :: Parser a               -- ^ operand parser
        -> Parser (a -> a -> a)   -- ^ operator parser
        -> Parser a               -- ^ whole answer
chainl1 getArg getOp = liftA2 link
                       getArg
                       (many (liftA2 (,) getOp getArg))
  where
    link x opys = foldl (\accum (op,y) -> op accum y) x opys
