module AST where

import Prelude
import Effect (Effect)
import Data.Number
import Prim.Boolean
import Data.Map
import Data.Maybe

import Transmission (Transmission, defTransmission, defTransmissionOn, Vec3)

type AST = Maybe Statement

defaultProgram :: AST
defaultProgram = Nothing

data Statement =
  TransmissionAST TransmissionAST

instance showStatement :: Show Statement where
  show (TransmissionAST s) = "TransmissionAST " <> show s

data TransmissionAST =
  LiteralTransmissionAST Boolean |
  Scalar Vec3 TransmissionAST |
  Movet Vec3 TransmissionAST |
  Rodar Vec3 TransmissionAST

instance showTransmissionAST :: Show TransmissionAST where
  show (LiteralTransmissionAST b) = "LitTransmission " <> show b
  show (Scalar v3 t) = "Scalar" <> show v3 <> show t
  show (Movet v3 t) = "Movet " <> show v3 <> show t
  show (Rodar v3 t) = "Rodar" <> show v3 <> show t

tASTtoT :: TransmissionAST -> Transmission
tASTtoT (LiteralTransmissionAST false) = defTransmission
tASTtoT (LiteralTransmissionAST true) = defTransmissionOn
tASTtoT (Scalar v3 t) = (tASTtoT t) {size = v3}
tASTtoT (Movet v3 t) = (tASTtoT t) {position = v3}
tASTtoT (Rodar v3 t) = (tASTtoT t) {rotation = v3}
