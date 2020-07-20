{-# LANGUAGE GADTs, OverloadedStrings #-}
module Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme  = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

rws = ["def", "extern"]
rop = ["+","*","-",";"]

identStart :: Parser Char
identStart = letterChar

identLetter :: Parser Char
identLetter = alphaNumChar

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
 where
   p       = (:) <$> identStart <*> many identLetter
   check x =
     if x `elem` rws
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

semiSep :: Parser a -> Parser [a]
semiSep = flip sepBy (symbol ";")

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy (symbol ",")

reserved :: String -> Parser ()
reserved name =
  lexeme $ try $
  do{ _ <- string' name
    ; notFollowedBy identLetter <?> ("end of " ++ show name)
    }
reservedOp :: String -> Parser ()
reservedOp name =
  lexeme $ try $
  do{ _ <- string' name
    ; notFollowedBy identLetter <?> ("end of " ++ show name)
    }