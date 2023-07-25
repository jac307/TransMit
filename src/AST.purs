module AST where

import Prelude
import Data.List (List(..))
import Effect (Effect)
import Data.Number
import Prim.Boolean
import Data.Map
import Data.Maybe
import Effect (Effect)

import ThreeJS (FormatID, rgbaFormat)

import Transmission (Transmission, defTransmission, defTransmissionOn, Vec3, Vec2)

type AST = List Statement

data Statement =
  EmptyStatement |
  TransmissionAST TransmissionAST

instance showStatement :: Show Statement where
  show (TransmissionAST s) = "TransmissionAST " <> show s
  show (EmptyStatement) = "EmptyStatement"

data TransmissionAST =
  LiteralTransmissionAST Boolean |
  ChannelRepeater Vec2 TransmissionAST |
  Scalar Vec3 TransmissionAST |
  Movet Vec3 TransmissionAST |
  Rodar Vec3 TransmissionAST |
  Fulcober String TransmissionAST |
  Switch String TransmissionAST |
  Monitor String TransmissionAST |
  Brillo Number TransmissionAST |
  Colour Vec3 TransmissionAST |
  EmissionColour Vec3 TransmissionAST |
  EmissionIntensity Number TransmissionAST
  --Volume Float TransmissionAST

instance showTransmissionAST :: Show TransmissionAST where
  show (LiteralTransmissionAST b) = "LitTransmission " <> show b
  show (ChannelRepeater v2 t) = "Repit" <> show v2 <> show t
  show (Scalar v3 t) = "Scalar" <> show v3 <> show t
  show (Movet v3 t) = "Movet " <> show v3 <> show t
  show (Rodar v3 t) = "Rodar" <> show v3 <> show t
  show (Fulcober f t) = "Fulcober" <> show f <> show t
  show (Switch s t) = "Switch" <> show s <> show t
  show (Monitor s t) = "Monitor" <> show s <> show t
  show (Brillo n t) = "Brillo" <> show n <> show t
  show (Colour v3 t) = "Color" <> show v3 <> show t
  show (EmissionColour v3 t) = "Emission-color" <> show v3 <> show t
  show (EmissionIntensity n t) = "Emission-intensity" <> show n <> show t

tASTtoT :: TransmissionAST -> Transmission
tASTtoT (LiteralTransmissionAST false) = defTransmission
tASTtoT (LiteralTransmissionAST true) = defTransmissionOn
tASTtoT (ChannelRepeater v2 t) = (tASTtoT t) {channelReapeater = v2}
tASTtoT (Scalar v3 t) = (tASTtoT t) {size = v3}
tASTtoT (Movet v3 t) = (tASTtoT t) {position = v3}
tASTtoT (Rodar v3 t) = (tASTtoT t) {rotation = v3}
tASTtoT (Fulcober f t) = (tASTtoT t) {fulcober = f}
tASTtoT (Switch s t) = (tASTtoT t) {channel = s}
tASTtoT (Monitor s t) = (tASTtoT t) {tv = (s <> ".obj"), mapping = s <> ".mtl"}
tASTtoT (Brillo n t) = (tASTtoT t) {brillo = n}
tASTtoT (Colour v3 t) = (tASTtoT t) {colour = v3}
tASTtoT (EmissionColour v3 t) = (tASTtoT t) {emissionColour = v3}
tASTtoT (EmissionIntensity n t) = (tASTtoT t) {emissionIntensity = n}
