module Transmission
(Transmission,
defTransmission,
Vec3)
where

type Transmission = {
  estado :: Boolean,
  tv :: String,
  mapping :: String,
  tvZone :: Int,
  channel :: String,
  position :: Vec3
  }

defTransmission :: Transmission
defTransmission = {
  estado: false,
  tv: "3dObjects/cubo.obj",
  mapping: "3dObjects/cubo.mtl",
  tvZone: 0,
  channel: "textures/04.mov", -- only when it is true
  position: defPosition
}

type Vec3 = {
  x :: Number,
  y :: Number,
  z :: Number
  }

defPosition :: Vec3
defPosition = { x: 0.0, y: 0.0, z: 0.0 }
