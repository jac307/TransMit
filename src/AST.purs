module AST where

import Prelude
import Data.Number
import Prim.Boolean
import Data.Map
import Data.Maybe


type AST = Maybe Statement

defaultProgram :: AST
defaultProgram = Nothing

-- Transmission on --- cube with green
-- Transmission off -- cube with color white

-- type Program = Map Int Statement
--
-- defaultProgram' :: Program
-- defaultProgram' = empty

data Statement =
  Assignment String Channel |
  Transmission Transmission

instance showStatement :: Show Statement where
  show (Assignment s c) = "Assignment " <> show s <> show " " <> show c
  show (Transmission s) = "Transmission " <> show s

data Transmission =
  LiteralTransmission Boolean -- |
  -- Switchear Transmission Channel |
  -- Movet Transmission Number Number Number |
  -- Rodar Transmission Number Number Number |
  -- Scale Transmission Number

instance showLiteralTransmission :: Show Transmission where
  show (LiteralTransmission b) = "LitTransmission " <> show b
  -- show (Switchear t c) = "Switchear " <> show t <> show " " <> show c
  -- show (Movet t n n n) = "Movet " <> show t <> show " " <> show n <> show n <> show n
  -- show (Rodar t n n n) = "Rodar " <> show t <> show " " <> show n <> show n <> show n
  -- show (Scale t n) = "Scale " <> show t <> show " " <> show n


data Channel =
  Channel String |
  ChannelReference String

instance showChannel :: Show Channel where
  show (Channel s) = show s
  show (ChannelReference s) = show s


-- type monitor = monitor {
--   status    ::  Rational,                         -- on/off animated opacity
--   geometry  ::  (Text,Text),                      -- ("mtl", "obj")
--   texture   ::  Text,                             -- "url"
--   wrapping  ::  wrappingMode
--   size      ::  Rational                          -- proportionally: h,w,l
--   position  ::  (Rational, Rational, Rational),   -- (x,y,z)
--   rotation  ::  (Rational, Rational, Rational)    -- (x,y,z)
--   }
