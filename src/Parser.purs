module Parser where

import Prelude (Unit, bind, discard, negate, pure, show, identity, ($), ($>), (*), (<$>), (<>), (+), unit)
import Control.Semigroupoid ((<<<))
import Data.Identity (Identity)
import Data.List (List, foldl)
import Data.List.NonEmpty (NonEmptyList)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Parsing (ParseError(..), ParserT, Position(..), runParser)
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..), GenTokenParser, makeTokenParser, unGenLanguageDef)
import Parsing.Combinators (choice, lookAhead, try, (<|>), many)
import Parsing.String (eof)

import AST (AST, Statement(..), TransmissionAST(..))
import Transmission (Vec3)

parseProgram :: String -> Either String AST
parseProgram x = case (runParser x ast) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

type P a = ParserT String Identity a

ast :: P AST
ast = do
  whiteSpace
  x <- choice [
    justAStatement,
    justWhiteSpace,
    noTranmission
    ]
  eof
  pure x

justAStatement :: P AST
justAStatement = do
  s <- statement
  pure $ Just s

statement :: P Statement
statement = choice [
  --try $ assignment,
  TransmissionAST <$> transmissionParser
]

justWhiteSpace :: P AST
justWhiteSpace = do
  lookAhead eof
  pure $ Nothing

noTranmission :: P AST
noTranmission = do
  reserved "turn off"
  pure Nothing

--- Transmission ---
--------------------

-- transmission on;
-- transmission off;
transmissionParser :: P TransmissionAST
transmissionParser = do
  _ <- pure unit
  reserved "transmission"
  b <- onOrOff
  let t = LiteralTransmissionAST b
  xs <- many transformations
  let xs' = foldl (<<<) identity xs
  pure $ xs' t

onOrOff :: P Boolean
onOrOff = try $ choice [
  (reserved "on" <|> reserved "onn" <|> reserved "onnn")  $> true,
  (reserved "off" <|> reserved "of" <|> reserved "offf") $> false
]

-- Transformations --
---------------------

transformations :: P (TransmissionAST -> TransmissionAST)
transformations = do
  _ <- pure unit
  choice [
  scalarParser,
  movetParser,
  rodarParser
  ]

scalarParser :: P (TransmissionAST -> TransmissionAST)
scalarParser = do
  (reserved "scalar")
  v3 <- vec3Param
  pure $ Scalar v3

movetParser :: P (TransmissionAST -> TransmissionAST)
movetParser = do
  (reserved "movet" <|> reserved "muvet" <|> reserved "muv" <|> reserved "move" <|> reserved "move it")
  v3 <- vec3Param
  pure $ Movet v3

rodarParser :: P (TransmissionAST -> TransmissionAST)
rodarParser = do
  (reserved "rodar")
  v3 <- vec3Param
  pure $ Rodar v3

----------

--                       x y z
-- transmission on movet 1 1 1;
-- transmission on movet 1;  -- vec3x not working
-- transmission on movet 1 1;
-- transmission on movet _ 1;
-- transmission on movet _ _ 1;

vec3Param :: P Vec3
vec3Param = choice [ try vec3xyz, vec3xy, try vec3z, try vec3y, try vec3x ]

--Function 1 1 1 --> modifies x,y,z
vec3xyz :: P Vec3
vec3xyz = do
  x <- number
  y <- number
  z <- number
  pure $ {x,y,z}

--Function _ _ 1 --> modifies z    defX / defY=0
vec3z :: P Vec3
vec3z = do
  reservedOp "_"
  reservedOp "_"
  let x = 0.0
  let y = 0.0
  z <- number
  pure $ {x,y,z}

--Function _ 1 --> modifies y    defX / defZ=0
vec3y :: P Vec3
vec3y = do
  reservedOp "_"
  let x = 0.0
  y <- number
  let z = 0.0
  pure $ {x,y,z}

--Function 1 1 --> modifies x,y    defZ=0
vec3xy :: P Vec3
vec3xy = do
  x <- number
  y <- number
  let z = 0.0
  pure $ {x,y,z}

--Function 1 --> modifies x    defY / defZ=0
vec3x :: P Vec3
vec3x = do
  x <- number
  let y = 0.0
  let z = 0.0
  pure $ {x,y,z}

----------

number :: P Number
number = choice [
  try negativeNumber,
  try float,
  toNumber <$> integer
  ]

negativeNumber :: P Number
negativeNumber = do
  reservedOp "-"
  ((*) (-1.0)) <$> float

------------


tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["transmission", "on", "off", "channel", "movet", "scalar", "rodar"],
  reservedOpNames = ["=", "\"", "\"", "_"]
  }

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
