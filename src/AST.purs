module AST where

import Prelude (class Show, show, bind, pure, (*), (<>), ($))
import Data.List (List)
import Effect

import ThreeJS (FormatID, rgbaFormat)

import Transmission (Transmission, defTransmission, defTransmissionOn, Vec3, Vec2, DynVec3)

type AST = List Statement

data Statement =
  EmptyStatement |
  TransmissionAST TransmissionAST

instance showStatement :: Show Statement where
  show (TransmissionAST s) = "TransmissionAST " <> show s
  show (EmptyStatement) = "EmptyStatement"

data TransmissionAST =
  LiteralTransmissionAST Boolean |
  Volume Number TransmissionAST |
  ChannelRepeater Vec2 TransmissionAST |
  Scalar Number TransmissionAST |
  Movet Vec3 TransmissionAST |
  Rodar DynVec3 TransmissionAST |
  Fulcober String TransmissionAST |
  Switch String TransmissionAST |
  Monitor String TransmissionAST |
  Translucidez Number TransmissionAST |
  Colour Vec3 TransmissionAST |
  EmissionColour Vec3 TransmissionAST |
  EmissionIntensity Number TransmissionAST

instance showTransmissionAST :: Show TransmissionAST where
  show (LiteralTransmissionAST b) = "LitTransmission " <> show b
  show (Volume n t) = "Volume" <> show n <> show t
  show (ChannelRepeater v2 t) = "Repit" <> show v2 <> show t
  show (Scalar n t) = "Scalar" <> show n <> show t
  show (Movet v3 t) = "Movet " <> show v3 <> show t
  show (Rodar dv3 t) = "Rodar" <> show dv3 <> show t
  show (Fulcober f t) = "Fulcober" <> show f <> show t
  show (Switch s t) = "Switch" <> show s <> show t
  show (Monitor s t) = "Monitor" <> show s <> show t
  show (Translucidez n t) = "Translucidez" <> show n <> show t
  show (Colour v3 t) = "Color" <> show v3 <> show t
  show (EmissionColour v3 t) = "Emission-color" <> show v3 <> show t
  show (EmissionIntensity n t) = "Emission-intensity" <> show n <> show t

tASTtoT :: TransmissionAST -> Transmission
tASTtoT (LiteralTransmissionAST false) = defTransmission
tASTtoT (LiteralTransmissionAST true) = defTransmissionOn
tASTtoT (Volume n t) = (tASTtoT t) {volume = (n * 0.01)}
tASTtoT (ChannelRepeater v2 t) = (tASTtoT t) {channelReapeater = v2}
tASTtoT (Scalar n t) = (tASTtoT t) {size = n}
tASTtoT (Movet v3 t) = (tASTtoT t) {position = v3}
tASTtoT (Rodar dv3 t) = (tASTtoT t) {rotation = dv3}
tASTtoT (Fulcober f t) = (tASTtoT t) {fulcober = f}
tASTtoT (Switch s t) = (tASTtoT t) {channel = ("https://jac307.github.io/TransMit/channels/" <> s <> ".mov")}
tASTtoT (Monitor s t) = (tASTtoT t) {tv = (s <> ".obj"), mapping = (s <> ".mtl")}
tASTtoT (Translucidez n t) = (tASTtoT t) {translucidez = n}
tASTtoT (Colour v3 t) = (tASTtoT t) {colour = v3}
tASTtoT (EmissionColour v3 t) = (tASTtoT t) {emissionColour = v3}
tASTtoT (EmissionIntensity n t) = (tASTtoT t) {emissionIntensity = n}
