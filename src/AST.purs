module AST where

import Prelude
import Effect (Effect)
import Data.Number
import Prim.Boolean
import Data.Map
import Data.Maybe

import Transmission (Transmission(..), defTransmission)

type AST = Maybe Statement

defaultProgram :: AST
defaultProgram = Nothing

data Statement =
  TransmissionAST TransmissionAST
  --Assignment String Channel |

instance showStatement :: Show Statement where
  show (TransmissionAST s) = "TransmissionAST " <> show s
  -- show (Assignment s c) = "Assignment " <> show s <> show " " <> show ]

data TransmissionAST =
  LiteralTransmissionAST Boolean |
  Movet Number Number Number TransmissionAST  -- |
  -- Rodar Number Number Number TransmissionAST |
  -- Scale Number TransmissionAST
  -- Switchear TransmissionAST Channel |

instance showTransmissionAST :: Show TransmissionAST where
  show (LiteralTransmissionAST b) = "LitTransmission " <> show b
  show (Movet n1 n2 n3 t) = "Movet " <> " " <> show n1 <> show n2 <> show n3 <> show t
  -- show (Rodar t n1 n2 n3) = "Rodar " <> show t <> " " <> show n1 <> show n2 <> show n3
  -- show (Scale t n) = "Scale " <> show t <> " " <> show n
  -- show (Switchear t c) = "Switchear " <> show t <> " " <> show c

-- f :: TransmissionAST -> Transmission
--f (LiteralTransmissionAST b) = defTransmission {estado = b}
--                                                                                                                                                                                                                                                                          mm   f (Movet n1 n2 n3 t) = f t {locationX=n1, locationY=n2, locationZ=n3}




-- f (Switchear t c) = f t { someField = c }
-- f (Scale t s) = f t { someFieldorFields = s ... }

-- data Channel =
--   Channel String |
--   ChannelReference String
--
-- instance showChannel :: Show Channel where
--   show (Channel s) = show s
--   show (ChannelReference s) = show s
