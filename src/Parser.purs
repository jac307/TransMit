module Parser where

import Prelude
import Data.Identity
import Data.List
import Data.List.NonEmpty
import Data.Map
import Data.Either
import Data.Number
import Data.Int
import Prim.Boolean
import Parsing
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Parsing.Combinators
import Parsing.String (eof)
--import Parsing.Parser

import AST

parseProgram :: String -> Either String AST
parseProgram x = case (runParser x ast) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

type P a = ParserT String Identity a

ast :: P AST
ast = parametro

-- program :: P Program
-- program = do
--   whiteSpace
--   xs <- semiSep statement
--   eof
--   pure $ fromFoldableWithIndex xs

--runParser "transmission on" transmission
-- statement :: P AST
-- statement = ast
  --
  -- try $ choice [
  -- assignment,
  -- Transmission <$> transmission
  -- ]

transmission :: P Transmission
transmission = try $ choice [
  transmissionOnOff,
  litTransmission
  ]

----------
-- spago repl
-- import AST
-- import Parser
-- import Text.Parsing.Parser
-- runParser "var = channel \"url\"" assignment

-- someName = chanel "url"
assignment :: P Statement
assignment = do
  i <- identifier
  reservedOp "="
  c <- channel
  pure $ Assignment i c

channel :: P Channel
channel = do
  (reserved "channel" <|> reserved "chanel" <|> reserved "chianel")
  s <- stringLiteral
  pure $ Channel s
  --data Channel = Channel String | ChannelReference String

----------

-- tranmission
litTransmission :: P Transmission
litTransmission = do
  (reserved "transmission" <|> reserved "transmision" <|> reserved "transmisssion")
  pure $ LiteralTransmission false

-- transmission on / off
transmissionOnOff :: P Transmission
transmissionOnOff = do
  (reserved "transmission" <|> reserved "transmision" <|> reserved "transmisssion")
  b <- onOff
  pure $ LiteralTransmission b

onOff :: P Boolean
onOff = try $ choice [
  (reserved "on" <|> reserved "onn" <|> reserved "onnn")  $> true,
  (reserved "off" <|> reserved "of" <|> reserved "offf") $> false
]

----------

parametro :: P Number
parametro = try $ choice [
  try negativeInt,
  try negativeFloat,
  toNumber <$> integer,
  float
  ]

negativeFloat :: P Number
negativeFloat = do
  reservedOp "-"
  ((*) (-1.0)) <$> float

negativeInt :: P Number
negativeInt = do
  x <- symbol "-"
  f <- (toNumber <$> integer)
  pure $ (-1.0) * f

------------


tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["transmission", "on", "off", "channel"],
  reservedOpNames = ["=", "\"", "\""]
  }

-- transmission on ico
--
-- one = channel "/mivideo.mov";
-- transmission on ico switchear one
--
-- one = channel "/mivideo.mov";
-- two = channel "/mivideo2.mov";
-- transmission on switchear two rodar (360)
-- transmission on ico switchear one
--


-----------

-- methodWithOneParameter :: String -> (Transmission -> Number -> Transmission) -> P (Transmission)
-- methodWithOneParameter methodName constructor = try $ do
--   t <- transmissionOnOff
--   reserved methodName
--   p1 <- parametro
--   pure $ constructor t p1
--
-- methodWithTwoParameters :: String -> (Transmission -> Number -> Number -> Transmission) -> P (Transmission)
-- methodWithTwoParameters methodName constructor = try $ do
--   t <- transmissionOnOff
--   reserved methodName
--   p1 <- parametro
--   p2 <- parametro
--   pure $ constructor t p1 p2
--
-- methodWithThreeParameters :: String -> (Transmission -> Number -> Number -> Number -> Transmission) -> P (Transmission)
-- methodWithThreeParameters methodName constructor = try $ do
--   t <- transmissionOnOff
--   reserved methodName
--   p1 <- parametro
--   p2 <- parametro
--   p3 <- parametro
--   pure $ constructor t p1 p2 p3

---------------------

angles :: forall a. P a -> P a
angles = tokenParser.angles

braces :: forall a. P a -> P a
braces = tokenParser.braces

brackets :: forall a. P a -> P a
brackets = tokenParser.brackets

charLiteral :: P Char
charLiteral = tokenParser.charLiteral

colon :: P String
colon = tokenParser.colon

comma :: P String
comma = tokenParser.comma

commaSep :: forall a. P a -> P (List a)
commaSep = tokenParser.commaSep

commaSep1 :: forall a. P a -> P (NonEmptyList a)
commaSep1 = tokenParser.commaSep1

decimal :: P Int
decimal = tokenParser.decimal

dot :: P String
dot = tokenParser.dot

float :: P Number
float = tokenParser.float

hexadecimal :: P Int
hexadecimal = tokenParser.hexadecimal

identifier :: P String
identifier = tokenParser.identifier

integer :: P Int
integer = tokenParser.integer

lexeme :: forall a. P a -> P a
lexeme = tokenParser.lexeme

natural :: P Int
natural = tokenParser.natural

naturalOrFloat :: P (Either Int Number)
naturalOrFloat = tokenParser.naturalOrFloat

octal :: P Int
octal = tokenParser.octal

operator :: P String
operator = tokenParser.operator

parens :: forall a. P a -> P a
parens = tokenParser.parens

reserved :: String -> P Unit
reserved = tokenParser.reserved

reservedOp :: String -> P Unit
reservedOp = tokenParser.reservedOp

semi :: P String
semi = tokenParser.semi

semiSep :: forall a. P a -> P (List a)
semiSep = tokenParser.semiSep

semiSep1 :: forall a. P a -> P (NonEmptyList a)
semiSep1 = tokenParser.semiSep1

stringLiteral :: P String
stringLiteral = tokenParser.stringLiteral

symbol :: String -> P String
symbol = tokenParser.symbol

whiteSpace :: P Unit
whiteSpace = tokenParser.whiteSpace
