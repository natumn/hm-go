module Lib
    ( someFunc
    )
where

import           Data.Char                      ( ord )
import           Text.Parsec                    ( char
                                                , oneOf
                                                , (<|>)
                                                , parseTest
                                                )
import           Text.Parsec.String             ( Parser )

data Exp = Add Exp Exp | Mul Exp Exp | Nat Int deriving Show
{- BNF of one digit addtion & multiplication
expr ::= term ('+' expr | ε)
term ::= factor ('*' term | ε)
factor ::= '(' expr ')' | nat
nat ::= '0' | '1' | '2' | ...
-}

-- expr ::= term ('+' expr | ε)
expr :: Parser Exp
expr = do
    t <- term
    do
            char '+'
            e <- expr
            return (Add t e)
        <|> return t

-- term ::= factor ('*' term | ε)
term :: Parser Exp
term = do
    f <- factor
    do
            char '*'
            t <- term
            return (Mul f t)
        <|> return f

-- factor ::= '(' expr ')' | nat
factor :: Parser Exp
factor =
    do
            char '('
            e <- expr
            char ')'
            return e
        <|> nat

-- nat ::= '0' | '1' | '2' | ...
nat :: Parser Exp
nat = do
    c <- oneOf ['0' .. '9']
    return (Nat (charToInt c))
    where charToInt c = ord c - ord '0'

{-
type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v inp = [(v, inp)]

failure :: Parser a
failure inp = []

item :: Parser Char
item inp = case inp of
    []       -> []
    (x : xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
    []         -> []
    [(v, out)] -> parse (f v) out

p1 :: Parser (Char, Char)
p1 = do
    x <- item
    y <- item
    return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
    []         -> parse q inp
    [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x : xs) = do
    char x
    string xs
    return (x : xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do
    v  <- p
    vs <- many p
    return (v : vs)

ident :: Parser String
ident = do
    x  <- lower
    xs <- many1 digit
    return (x : xs)

nat :: Parser Int
nat = do
    xs <- many1 alphanum
    return (read xs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

-- 以下は加算演算子よりも乗算演算子の方が結合度が高いことを表現できていない
-- expr ::= expr  '+' expr | expr '*' expr | '(' expr ')' | nat

-- expr    ::= term '+' expr | term
-- term    ::= factor '*' term | factor
-- factor  ::= '(' expr ')' | nat
-- nat     ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'

expr :: Parser Int
expr =
    do
            t <- term
            do
                symbol "+"
                e <- expr
                return (t + e)
        +++ return t

term :: Parser Int
term =
    do
            f <- factor
            do
                symbol "*"
                t <- term
                return (f * t)
        +++ return f

factor :: Parser Int
factor =
    do
            symbol "("
            e <- expr
            symbol ")"
            return e
        +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
    [(n, [] )] -> n
    [(_, out)] -> error ("unused input " ++ out)
    []         -> error "incalid input"
-}

someFunc :: IO ()
someFunc = putStrLn "someFunc"



