{-# LANGUAGE OverloadedStrings #-}

module Hyperscript.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

data Error = Error deriving (Eq, Ord)

type Parser = Parsec Void Text

data HsLexemes
  = PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | PERIOD
  | ELLIPSIS
  | BACKSLASH
  | COLON
  | PERCENT
  | PIPE
  | EXCLAMATION
  | QUESTION
  | POUND
  | AMPERSAND
  | DOLLAR
  | SEMI
  | COMMA
  | L_PAREN
  | R_PAREN
  | L_ANG
  | R_ANG
  | LTE_ANG
  | GTE_ANG
  | EQ_
  | EQQ
  | NEQ
  | NEQQ
  | L_BRACE
  | R_BRACE
  | L_BRACKET
  | R_BRACKET
  | EQUALS
  deriving (Eq, Show)

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

symbol = L.symbol space

plus = do
  symbol "+"
  pure PLUS

minus = do
  symbol "-"
  pure MINUS

multiply = do
  symbol "*"
  pure MULTIPLY

divide = do
  symbol "/"
  pure DIVIDE

period = do
  symbol "."
  pure PERIOD

ellipsis = do
  symbol ".."
  pure ELLIPSIS

backslash = do
  symbol "\\"
  pure BACKSLASH

colon = do
  symbol ":"
  pure COLON

percent = do
  symbol "%"
  pure PERCENT

pipe = do
  symbol "|"
  pure PIPE

exclamation = do
  symbol "!"
  pure EXCLAMATION

question = do
  symbol "?"
  pure QUESTION

pound = do
  symbol "#"
  pure POUND

ampersand = do
  symbol "&"
  pure AMPERSAND

dollar = do
  symbol "$"
  pure DOLLAR

semi = do
  symbol ";"
  pure SEMI

comma = do
  symbol ","
  pure COMMA

lParen = do
  symbol "("
  pure L_PAREN

rParen = do
  symbol ")"
  pure R_PAREN

lAng = do
  symbol "<"
  pure L_ANG

rAng = do
  symbol ">"
  pure R_ANG

lteAng = do
  symbol "<="
  pure LTE_ANG

gteAng = do
  symbol ">="
  pure GTE_ANG

eq = do
  symbol "=="
  pure EQ_

eqq = do
  symbol "==="
  pure EQQ

neq = do
  symbol "!="
  pure NEQ

neqq = do
  symbol "!=="
  pure NEQQ

lBrace = do
  symbol "{"
  pure L_BRACE

rBrace = do
  symbol "}"
  pure R_BRACE

lBracket = do
  symbol "["
  pure L_BRACKET

rBracket = do
  symbol "]"
  pure R_BRACKET

equals = do
  symbol "="
  pure EQUALS
