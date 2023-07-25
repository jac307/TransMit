module Parser where

import Prelude (Unit, bind, discard, negate, pure, show, identity, ($), ($>), (*), (<$>), (<>), unit, map)
import Control.Semigroupoid ((<<<))
import Data.Identity (Identity)
import Data.List (List, catMaybes, foldl)
import Data.List.NonEmpty (NonEmptyList)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Parsing (ParseError(..), ParserT, Position(..), runParser)
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..), GenTokenParser, makeTokenParser, unGenLanguageDef)
import Parsing.Combinators (choice, lookAhead, try, (<|>), many, sepBy)
import Parsing.String (eof)

import AST (AST, Statement(..), TransmissionAST(..), tASTtoT)
import Transmission (Transmission, Vec3, Vec2)

parseProgram :: String -> Either String Program
parseProgram x = do
  ast <- parseAST x
  pure $ astToProgram ast

parseAST :: String -> Either String (List Statement)
parseAST x = case (runParser x ast) of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

type P a = ParserT String Identity a

ast :: P AST
ast = do
  whiteSpace
  x <- statements
  eof
  pure x

statements :: P (List Statement)
statements = sepBy statement (reservedOp ";")

statement :: P Statement
statement = choice [
  TransmissionAST <$> transmissionParser,
  onlySemiColon,
  onlyEOF,
  noTranmission
]

onlySemiColon :: P Statement
onlySemiColon = do
    lookAhead $ reservedOp ";"
    pure $ EmptyStatement

onlyEOF :: P Statement
onlyEOF = do
    lookAhead $ eof
    pure $ EmptyStatement

noTranmission :: P Statement
noTranmission = do
  reserved "turn off"
  pure $ EmptyStatement

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
  functionWithV2 "repet" ChannelRepeater,
  functionWithV3 "scalar" Scalar,
  functionWithV3 "movet" Movet,
  functionWithV3 "rodar" Rodar,
  functionWithString "fulcober" Fulcober,
  functionWithNumber "brillo" Brillo,
  functionWithV3 "color" Colour,
  functionWithV3 "emit" EmissionColour,
  functionWithNumber "intensity" EmissionIntensity,
  switchFunction,
  monitorFunction
  ]

switchFunction :: P (TransmissionAST -> TransmissionAST)
switchFunction = do
  _ <- pure unit
  reserved "switch"
  s <- stringLiteral
  pure $ Switch ("channels/" <> s)
  -- should remove the empty spaces at the beginning of s
  -- this function can only be use with transmission on

-- get rid of the quotation marks
monitorFunction :: P (TransmissionAST -> TransmissionAST)
monitorFunction = do
  _ <- pure unit
  reserved "monitor"
  s <- stringLiteral
  pure $ Monitor ("monitors/" <> s)
-- check empty spaces

functionWithString :: String -> (String -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithString functionName constructor = try $ do
  reserved functionName
  s <- identifier
  pure $ constructor s

functionWithV3 :: String -> (Vec3 -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithV3 functionName constructor = try $ do
  reserved functionName
  v3 <- vec3Param
  pure $ constructor v3

functionWithV2 :: String -> (Vec2 -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithV2 functionName constructor = try $ do
  reserved functionName
  v2 <- vec2xy
  pure $ constructor v2

functionWithNumber :: String -> (Number -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithNumber functionName constructor = try $ do
  reserved functionName
  n <- number
  pure $ constructor n

----------

vec2xy :: P Vec2
vec2xy = do
  x <- number
  y <- number
  pure $ {x,y}

-- channel = 1 "";
--                       x y z
-- transmission on movet 1 1 1 (channel 1);
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
  reservedOpNames = ["=", "\"", "\"", "_", ";"]
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

-- channel 1 "url";
-- transmission on switch 1

type Program = List Transmission --- This has to change if I add the camera ---- must be Transmission plus operation of the camera (record), plus,.... channel

-- we want a list that gives all the Just
-- catMaybes :: forall a. List (Maybe a) -> List a

astToProgram :: AST -> Program
astToProgram xs = catMaybes $ map statementToTransmission xs

-- statementsToTransmissionList :: (List Statement) -> (List Transmission)
-- statementsToTransmissionList xs = statementToTransmission <$> xs

statementToTransmission :: Statement -> Maybe Transmission
statementToTransmission EmptyStatement = Nothing
statementToTransmission (TransmissionAST tAST) = Just (tASTtoT tAST)
