{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Hyperscript.Lexer where

import Control.Monad.Combinators (many, (<|>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Byte (space)
import qualified Text.Megaparsec.Char.Lexer as L

tokenize :: Parser [Token]
tokenize = filter (\case { TokenComment _ -> False; _ -> True }) <$> (many whitespace >> many parseToken)

-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme spaceConsumer

-- spaceConsumer :: Parser ()
-- spaceConsumer = L.space (whitespace >> pure ()) (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

parseToken :: Parser Token
parseToken =
  try parseComment
    <|> try parseWhitespace
    <|> try parseClassRef
    <|> try parseIDRef
    <|> try parseAttrRef
    <|> try parseShortAttrRef
    <|> try parseStyleRef
    <|> try parseIdentifier
    <|> try parseNumber
    <|> try parseString
    <|> try parseOp

parseComment :: Parser Token
parseComment = do
  chunk "--"
  TokenComment . pack  <$> ((\(x, e) -> x <> [e]) <$> try (manyTill_ anySingle (try (single '\n') <|> try (single '\r'))) <|> try (many anySingle))

parseWhitespace :: Parser Token
parseWhitespace = do
  some whitespace
  pure TokenWhitespace

parseClassRef :: Parser Token
parseClassRef = do
  notFollowedBy precedingSymbol
  single '.'
  lookAhead alpha
  try className <|> try classTemplate
  where
    className :: Parser Token
    className = TokenClassRef . pack . ("." <>) <$> many cssClassChar

    classTemplate :: Parser Token
    classTemplate = TokenClassRef . pack . ("." <>) <$> between (single '{') (single '}') (many (anySingleBut '}'))

cssClassChar :: Parser Char
cssClassChar =
  try alpha
    <|> try numeric
    <|> try (single '-')
    <|> try (single '_')
    <|> try (single ':')

parseIDRef :: Parser Token
parseIDRef = do
  notFollowedBy precedingSymbol
  single '#'
  try idName <|> try idTemplate
  where
    idName :: Parser Token
    idName = TokenIDRef . pack . ("#" <>) <$> many cssIDChar

    idTemplate :: Parser Token
    idTemplate = TokenIDRef . pack . ("#" <>) <$> between (single '{') (single '}') (many (anySingleBut '}'))

cssIDChar :: Parser Char
cssIDChar =
  try alpha
    <|> try numeric
    <|> try (single '-')
    <|> try (single '_')
    <|> try (single ':')

precedingSymbol :: Parser Char
precedingSymbol =
  try alphaNumeric
    <|> try (single ')')
    <|> try (single '\\')
    <|> try (single '\'')
    <|> try (single '`')
    <|> try (single '}')
    <|> try (single ']')

parseAttrRef :: Parser Token
parseAttrRef = do
  single '['
  single '@'
  TokenAttrRef . pack <$> someTill anySingle (single ']')

parseShortAttrRef :: Parser Token
parseShortAttrRef = do
  single '@'
  TokenAttrRef . pack <$> some cssIDChar

parseStyleRef :: Parser Token
parseStyleRef = do
  single '*'
  TokenStyleRef . pack <$> some (try alpha <|> try (single '-'))

parseIdentifier :: Parser Token
parseIdentifier =
  TokenIdentifier
    <$> (try (chunk "beep!") <|> do lookAhead alpha >> try (pack <$> some (try alphaNumeric <|> try isIdentifierChar)))

isIdentifierChar :: Parser Char
isIdentifierChar = try (single '_') <|> try (single '$')

parseNumber :: Parser Token
parseNumber = do
  whole <- pack <$> some numeric
  TokenNumber . (whole <>) <$> (try fractional <|> try (pure ""))
  where
    fractional :: Parser Text
    fractional = do
      single '.'
      fraction <- pack <$> some numeric
      case fraction of
        "" -> pure ""
        f -> pure $ "." <> f

parseString :: Parser Token
parseString = try doubleQuoted <|> try template <|> try singleQuoted
  where
    doubleQuoted :: Parser Token
    doubleQuoted =
      TokenString . pack
        <$> between (single '"') (single '"') (many $ anySingleBut '"')

    template :: Parser Token
    template =
      TokenString . pack
        <$> between (single '`') (single '`') (many $ anySingleBut '`')

    singleQuoted :: Parser Token
    singleQuoted =
      TokenString . pack
        <$> between (single '\'') (single '\'') (many $ anySingleBut '\'')

parseOp :: Parser Token
parseOp =
  TokenOp
    <$> ( try plus
            <|> try minus
            <|> try multiply
            <|> try divide
            <|> try period
            <|> try ellipsis
            <|> try backslash
            <|> try colon
            <|> try percent
            <|> try pipe
            <|> try exclamation
            <|> try question
            <|> try pound
            <|> try ampersand
            <|> try dollar
            <|> try semi
            <|> try comma
            <|> try lParen
            <|> try rParen
            <|> try lAng
            <|> try rAng
            <|> try lteAng
            <|> try gteAng
            <|> try eq
            <|> try eqq
            <|> try neq
            <|> try neqq
            <|> try lBrace
            <|> try rBrace
            <|> try lBracket
            <|> try rBracket
            <|> try equals
        )

data Op
  = OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpPeriod
  | OpEllipsis
  | OpBackslash
  | OpColon
  | OpPercent
  | OpPipe
  | OpExclamation
  | OpQuestion
  | OpPound
  | OpAmpersand
  | OpDollar
  | OpSemi
  | OpComma
  | OpLParen
  | OpRParen
  | OpLAng
  | OpRAng
  | OpLTEAng
  | OpGTEAng
  | OpEq
  | OpEqq
  | OpNEq
  | OpNEqq
  | OpLBrace
  | OpRBrace
  | OpLBracket
  | OpRBracket
  | OpEquals
  deriving (Eq, Show)

{-
		function consumeOp() {
			var op = makeOpToken();
			var value = consumeChar(); // consume leading char
			while (currentChar() && OP_TABLE[value + currentChar()]) {
				value += consumeChar();
			}
			op.type = OP_TABLE[value];
			op.value = value;
			op.end = position;
			return op;
		}
-}

-- data Error = Error deriving (Eq, Ord)

type Parser = Parsec Void Text

-- data TokenInfo = TokenInfo
--   { tokenInfoStart :: Int
--   , tokenInfoEnd :: Int
--   , tokenInfoColumn :: Int
--   , tokenInfoLine :: Int
--   , tokenInfoToken :: Token
--   }

data Token
  = TokenComment Text
  | TokenClassRef Text
  | TokenAttrRef Text
  | TokenStyleRef Text
  | TokenIDRef Text
  | TokenIdentifier Text
  | TokenNumber Text
  | TokenOp Op
  | TokenString Text
  | TokenWhitespace
  deriving (Eq, Show)

plus :: Parser Op
plus = do
  single '+'
  pure OpPlus

minus :: Parser Op
minus = do
  single '-'
  pure OpMinus

multiply :: Parser Op
multiply = do
  single '*'
  pure OpMultiply

divide :: Parser Op
divide = do
  single '/'
  pure OpDivide

period :: Parser Op
period = do
  single '.'
  pure OpPeriod

ellipsis :: Parser Op
ellipsis = do
  chunk ".."
  pure OpEllipsis

backslash :: Parser Op
backslash = do
  single '\\'
  pure OpBackslash

colon :: Parser Op
colon = do
  single ':'
  pure OpColon

percent :: Parser Op
percent = do
  single '%'
  pure OpPercent

pipe :: Parser Op
pipe = do
  single '|'
  pure OpPipe

exclamation :: Parser Op
exclamation = do
  single '!'
  pure OpExclamation

question :: Parser Op
question = do
  single '?'
  pure OpQuestion

pound :: Parser Op
pound = do
  single '#'
  pure OpPound

ampersand :: Parser Op
ampersand = do
  single '&'
  pure OpAmpersand

dollar :: Parser Op
dollar = do
  single '$'
  pure OpDollar

semi :: Parser Op
semi = do
  single ';'
  pure OpSemi

comma :: Parser Op
comma = do
  single ','
  pure OpComma

lParen :: Parser Op
lParen = do
  single '('
  pure OpLParen

rParen :: Parser Op
rParen = do
  single ')'
  pure OpRParen

lAng :: Parser Op
lAng = do
  single '<'
  pure OpLAng

rAng :: Parser Op
rAng = do
  single '>'
  pure OpRAng

lteAng :: Parser Op
lteAng = do
  chunk "<="
  pure OpLTEAng

gteAng :: Parser Op
gteAng = do
  chunk ">="
  pure OpGTEAng

eq :: Parser Op
eq = do
  chunk "=="
  pure OpEq

eqq :: Parser Op
eqq = do
  chunk "==="
  pure OpEqq

neq :: Parser Op
neq = do
  chunk "!="
  pure OpNEq

neqq :: Parser Op
neqq = do
  chunk "!=="
  pure OpNEqq

lBrace :: Parser Op
lBrace = do
  single '{'
  pure OpLBrace

rBrace :: Parser Op
rBrace = do
  single '}'
  pure OpRBrace

lBracket :: Parser Op
lBracket = do
  single '['
  pure OpLBracket

rBracket :: Parser Op
rBracket = do
  single ']'
  pure OpRBracket

equals :: Parser Op
equals = do
  single '='
  pure OpEquals

alphaNumeric :: Parser Char
alphaNumeric = try alpha <|> try numeric

alpha :: Parser Char
alpha = try (oneOf ['a' .. 'z']) <|> try (oneOf ['A' .. 'Z'])

numeric :: Parser Char
numeric = oneOf ['0' .. '9']

nonNewlineWhitespace :: Parser Char
nonNewlineWhitespace =
  try (single ' ')
    <|> try (single '\t')

whitespace :: Parser Char
whitespace =
  try (single ' ')
    <|> try (single '\t')
    <|> try newline

newline :: Parser Char
newline =
  try (single '\r')
    <|> try (single '\n')

isReservedChar :: Parser Char
isReservedChar = try (single '`') <|> try (single '^')
