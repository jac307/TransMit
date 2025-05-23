module AST where

import Prelude (class Show, show, bind, pure, (*), (<>), ($))
import Data.List (List)
import Effect

import ThreeJS (FormatID, rgbaFormat)

import Transmission (Transmission, defTransmission, defTransmissionOn, Vec3, Vec2, DynVec3)

type AST = List Statement

data Statement =
  EmptyStatement |
  TransmissionAST TransmissionAST |
  Broadcaster String --new

instance showStatement :: Show Statement where
  show (TransmissionAST s) = "TransmissionAST " <> show s
  show (EmptyStatement) = "EmptyStatement"
  show (Broadcaster s) = "Broadcaster " <> s --new

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

tASTtoTWithBase :: String -> TransmissionAST -> Transmission
tASTtoTWithBase base (LiteralTransmissionAST false) = defTransmission
tASTtoTWithBase base (LiteralTransmissionAST true) = defTransmissionOn
tASTtoTWithBase base (Volume n t) = (tASTtoTWithBase base t) { volume = n * 0.01 }
tASTtoTWithBase base (ChannelRepeater v2 t) = (tASTtoTWithBase base t) { channelReapeater = v2 }
tASTtoTWithBase base (Scalar n t) = (tASTtoTWithBase base t) { size = n }
tASTtoTWithBase base (Movet v3 t) = (tASTtoTWithBase base t) { position = v3 }
tASTtoTWithBase base (Rodar dv3 t) = (tASTtoTWithBase base t) { rotation = dv3 }
tASTtoTWithBase base (Fulcober f t) = (tASTtoTWithBase base t) { fulcober = f }
tASTtoTWithBase base (Switch s t) = (tASTtoTWithBase base t) { channel = base <> s <> ".mov" }
tASTtoTWithBase base (Monitor s t) = (tASTtoTWithBase base t) { tv = s <> ".obj", mapping = s <> ".mtl" }
tASTtoTWithBase base (Translucidez n t) = (tASTtoTWithBase base t) { translucidez = n }
tASTtoTWithBase base (Colour v3 t) = (tASTtoTWithBase base t) { colour = v3 }
tASTtoTWithBase base (EmissionColour v3 t) = (tASTtoTWithBase base t) { emissionColour = v3 }
tASTtoTWithBase base (EmissionIntensity n t) = (tASTtoTWithBase base t) { emissionIntensity = n }
