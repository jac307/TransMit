module Parser
(
Program,
astToProgram,
parseProgram
) where

import Prelude (Unit, bind, discard, identity, negate, pure, show, unit, ($), ($>), (*), (<$>), (<>))
import Control.Alt ((<|>))
import Control.Semigroupoid ((<<<))
import Data.Identity (Identity)
import Data.List (List(..), elem, foldl, fromFoldable, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Parsing (ParseError(..), ParserT, Position(..), runParser)
import Parsing.Language (emptyDef)
import Parsing.Token (GenLanguageDef(..), GenTokenParser, makeTokenParser, unGenLanguageDef)
import Parsing.Combinators (choice, lookAhead, many, sepBy, try)
import Parsing.String (eof)
import Data.Number.Format (toString)

import Data.String.Common (toLower)
import Control.Alternative (empty)

import AST (AST, Statement(..), TransmissionAST(..), tASTtoTWithBase)
import Transmission (Transmission, Vec3, Vec2, DynVec3)

-------

type Program =
  {
  transmissions :: List Transmission,
  baseURL :: String
  }

type P a = ParserT String Identity a

parseProgram :: String -> Either String Program
parseProgram x = do
  ast <- parseAST x
  pure $ astToProgram ast

parseAST :: String -> Either String (List Statement)
parseAST x = case runParser x ast of
  Left err -> Left $ showParseError err
  Right prog -> Right prog

showParseError :: ParseError -> String
showParseError (ParseError e (Position p)) = show p.line <> ":" <> show p.column <> " " <> e

ast :: P AST
ast = do
  whiteSpace
  x <- statements
  eof
  pure x

statements :: P (List Statement)
statements = List.fromFoldable <$> sepBy statement (reservedOp ";")

astToProgram :: AST -> Program
astToProgram xs =
  let base = "https://jac307.github.io/MultimediaSamples/Video/main/"
   in case go base Nil xs of
        { baseURL: b, transmissions: ts } -> { transmissions: List.reverse ts, baseURL: b }
  where
    go base acc Nil = { baseURL: base, transmissions: acc }
    go _ acc (Broadcaster url : rest) = go url acc rest
    go base acc (TransmissionAST t : rest) =
      let tr = tASTtoTWithBase base t
       in go base (tr : acc) rest
    go base acc (_ : rest) = go base acc rest

-------
-------

statement :: P Statement
statement = choice
  [ try $ TransmissionAST <$> transmissionParser
  , try $ broadcaster  -- new
  , try $ onlySemiColon
  , try $ onlyEOF
  , try $ noTranmission
  ]

broadcaster :: P Statement
broadcaster = do
  _ <- matchKeyword (fromFoldable ["broadcaster", "baseurl", "url"])
  s <- identifier <|> stringLiteral
  pure $ Broadcaster s

onlySemiColon :: P Statement
onlySemiColon = do
    lookAhead $ reservedOp ";"
    pure $ EmptyStatement

onlyEOF :: P Statement
onlyEOF = do
    lookAhead $ eof
    pure $ EmptyStatement

noTranmission :: P Statement
noTranmission = try $ choice
  [ reserved "turn of"  $> EmptyStatement
  , reserved "turn off"  $> EmptyStatement
  , reserved "turns off" $> EmptyStatement
  , reserved "turnof"    $> EmptyStatement
  , reserved "apagar"    $> EmptyStatement
  ]

--- Transmission ---
--------------------

transmissionParser :: P TransmissionAST
transmissionParser = do
  _ <- pure unit
  word <- identifier
  let keyword = toLower word
  if keyword `elem` fromFoldable
       [ "trans", "transmission", "trasmission", "trasmision", "transmicion"
       , "transmision", "transmisssion", "trasmisssion", "trasmicion"
       , "transmissión", "trasmissión", "trasmisión",  "transmición"
       , "transmisión", "transmisssión", "trasmisssión",  "trasmición"
       ]
    then pure unit
    else empty
  b <- onOrOff
  let t = LiteralTransmissionAST b
  xs <- many (transformations b)
  let xs' = foldl (<<<) identity xs
  pure $ xs' t

onOrOff :: P Boolean
onOrOff = try $ choice
  [ try $ matchKeyword (fromFoldable [ "on", "onn", "onnn", "onnnn"]) $> true
  , try $ matchKeyword (fromFoldable [ "off", "of", "offf", "offff"]) $> false
  ]

-- Transformations --
---------------------

transformations :: Boolean -> P (TransmissionAST -> TransmissionAST)
transformations isOn = choice
  [
    -- volume
    functionWithNumberKeyword (fromFoldable ["vol", "volume", "volumen", "volúmen", "subele", "súbele", "pumpealo"]) Volume,

    -- repeat
    functionWithV2Keyword (fromFoldable ["repeat", "repet", "repit", "repitelo", "repítelo", "repitealo"]) ChannelRepeater,

    -- move
    functionWithV3Keyword (fromFoldable ["muv", "movet", "muvet", "muvit", "movit", "move it", "muevelo", "muévelo", "muvetelo"]) Movet,

    -- rotate
    functionWithDynV3Keyword (fromFoldable ["rodar", "rotate", "rotait", "rotaetelo", "rotatelo"]) Rodar,

    -- fullcover
    functionWithStringKeyword (fromFoldable ["fulcober", "fullcober", "fulcover", "fullcover"]) Fulcober,

    -- translucency
    functionWithNumberKeyword (fromFoldable ["translucido", "traslucido", "translusido", "translucent", "traslucent", "traslusent"]) Translucidez,

    -- color
    functionWithV3Keyword (fromFoldable ["color", "colour", "color it", "colorealo", "colourealo"]) Colour,

    -- emission color
    functionWithV3Keyword (fromFoldable ["emit", "emitir", "emitear", "emitealo"]) EmissionColour,

    -- brightness
    functionWithNumberKeyword (fromFoldable ["brillo", "brightness", "braignes", "braigtnes", "briyo"]) EmissionIntensity,

    -- other flexible functions
    if isOn then switchFunctionWrapper else empty,
    -- switchFunctionWrapper,
    try $ monitorFunction,
    try $ scalarFunction
  ]

-------

switchFunctionWrapper :: P (TransmissionAST -> TransmissionAST)
switchFunctionWrapper = try $ do
  _ <- matchKeyword (fromFoldable
    [ "switch", "suitch", "suich", "suish", "swish", "cámbiale" ])
  n <- number
  switchFunction n

switchFunction :: Number -> P (TransmissionAST -> TransmissionAST)
switchFunction n = do
  let s = toString n
  pure $ Switch s

monitorFunction :: P (TransmissionAST -> TransmissionAST)
monitorFunction = do
  _ <- matchKeyword (fromFoldable
    [ "mon", "monitor", "mónitor", "monnitor"])
  s <- identifier
  pure $ Monitor ("https://www.transmits.link/monitors/" <> s)

scalarFunction :: P (TransmissionAST -> TransmissionAST)
scalarFunction = do
  _ <- matchKeyword (fromFoldable
    [ "scalar", "escalar", "scale", "escale", "scail", "escail", "esqueil", "squeil", "biggealo", "bigealo"])
  n <- number
  pure $ Scalar n

---------

functionWithNumberKeyword :: List String -> (Number -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithNumberKeyword keywords constructor = try $ do
  _ <- matchKeyword keywords
  n <- number
  pure $ constructor n

functionWithV2Keyword :: List String -> (Vec2 -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithV2Keyword keywords constructor = try $ do
  _ <- matchKeyword keywords
  v2 <- vec2xy
  pure $ constructor v2

functionWithV3Keyword :: List String -> (Vec3 -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithV3Keyword keywords constructor = try $ do
  _ <- matchKeyword keywords
  v3 <- vec3xyz
  pure $ constructor v3

functionWithDynV3Keyword :: List String -> (DynVec3 -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithDynV3Keyword keywords constructor = try $ do
  _ <- matchKeyword keywords
  v <- dynVec3xyz
  pure $ constructor v

functionWithStringKeyword :: List String -> (String -> (TransmissionAST -> TransmissionAST)) -> P (TransmissionAST -> TransmissionAST)
functionWithStringKeyword keywords constructor = try $ do
  _ <- matchKeyword keywords
  s <- identifier
  pure $ constructor s

-------
matchFlexibleKeyword :: String -> List String -> Boolean
matchFlexibleKeyword input options =
  let keyword = toLower input
  in keyword `elem` fromFoldable options

matchKeyword :: List String -> P Unit
matchKeyword options = do
  word <- identifier
  let keyword = toLower word
  if keyword `elem` fromFoldable options
    then pure unit
    else empty
-------

------- PARAMETERS

----------
---Either Number Number --

dynVec3xyz :: P DynVec3
dynVec3xyz = do
  _ <- pure unit
  x <- dynNumber
  y <- dynNumber
  z <- dynNumber
  pure $ {x,y,z}

dynNumber :: P (Either Number Number)
dynNumber = choice [ try dynNumberLeft, try dynNumberRight ]

dynNumberLeft :: P (Either Number Number)
dynNumberLeft = do
  _ <- matchKeyword (fromFoldable
    [ "a", "auto", "automatic", "atomatic", "automatico", "automático"])
  v <- number
  pure $ Left v
  -- pure $ Left (v * 0.001) -- Normalize auto-input

dynNumberRight :: P (Either Number Number)
dynNumberRight = do
  _ <- pure unit
  v <- number
  pure $ Right v
  -- pure $ Right (v * 0.001) -- Normalize manual input

--- Fixed Number Options

vec3xyz :: P Vec3
vec3xyz = do
  _ <- pure unit
  x <- number
  y <- number
  z <- number
  pure $ {x,y,z}

vec2xy :: P Vec2
vec2xy = do
  _ <- pure unit
  x <- number
  y <- number
  pure $ {x,y}

number :: P Number
number = choice [
  try negativeNumber,
  try float,
  toNumber <$> integer
  ]

negativeNumber :: P Number
negativeNumber = do
  _ <- pure unit
  reservedOp "-"
  ((*) (-1.0)) <$> float

------------

tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = [],
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

-----
