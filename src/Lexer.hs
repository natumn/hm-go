module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tk

lexer :: Tk.TokenParser()
lexer = Tk.makeTokenParser style
    where 
        ops = ["+", "*", "-", ";"]
        names = ["func", "extern"]
        style = emptyDef {
                    Tk.commentLine = "//"
                    , Tk.reservedOpNames = ops
                    , Tk.reservedNames = names
                }

integer :: Parser Integer
integer = Tk.integer lexer

float :: Parser Double
float = Tk.float lexer

parens :: Parser a -> Parser a
parens = Tk.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tk.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tk.semiSep lexer

identifier :: Parser String
identifier = Tk.identifier lexer

reserved :: String -> Parser ()
reserved = Tk.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tk.reservedOp lexer

