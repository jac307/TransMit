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

-- channel number string
-- camera ? type, pos, rot, size
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
  Format String TransmissionAST |
  Switch String TransmissionAST -- |
  --Monitor String TransmissionAST -- |
  --Volume Float TransmissionAST

instance showTransmissionAST :: Show TransmissionAST where
  show (LiteralTransmissionAST b) = "LitTransmission " <> show b
  show (ChannelRepeater v2 t) = "Repit" <> show v2 <> show t
  show (Scalar v3 t) = "Scalar" <> show v3 <> show t
  show (Movet v3 t) = "Movet " <> show v3 <> show t
  show (Rodar v3 t) = "Rodar" <> show v3 <> show t
  show (Format f t) = "Format" <> show f <> show t
  show (Switch s t) = "Switch" <> show s <> show t

tASTtoT :: TransmissionAST -> Transmission
tASTtoT (LiteralTransmissionAST false) = defTransmission
tASTtoT (LiteralTransmissionAST true) = defTransmissionOn
tASTtoT (ChannelRepeater v2 t) = (tASTtoT t) {channelReapeater = v2}
tASTtoT (Scalar v3 t) = (tASTtoT t) {size = v3}
tASTtoT (Movet v3 t) = (tASTtoT t) {position = v3}
tASTtoT (Rodar v3 t) = (tASTtoT t) {rotation = v3}
tASTtoT (Format f t) = (tASTtoT t) {format = f}
tASTtoT (Switch s t) = (tASTtoT t) {channel = s}
