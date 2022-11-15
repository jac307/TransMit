module AST where

import Prelude
import Effect (Effect)
import Data.Number
import Prim.Boolean
import Data.Map
import Data.Maybe

import Transmission (Transmission, defTransmission, Vec3)

type AST = Maybe Statement

defaultProgram :: AST
defaultProgram = Nothing

data Statement =
  TransmissionAST TransmissionAST

instance showStatement :: Show Statement where
  show (TransmissionAST s) = "TransmissionAST " <> show s

data TransmissionAST =
  LiteralTransmissionAST Boolean |
  Movet Vec3 TransmissionAST

instance showTransmissionAST :: Show TransmissionAST where
  show (LiteralTransmissionAST b) = "LitTransmission " <> show b
  show (Movet v3 t) = "Movet " <> show v3 <> show t

tASTtoT :: TransmissionAST -> Transmission
tASTtoT (LiteralTransmissionAST b) = defTransmission {estado = b} -- what happens with the channel?
tASTtoT (Movet v3 t) = (tASTtoT t) {position = v3}
