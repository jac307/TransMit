module Transmission
(Transmission,
defTransmission,
defTransmissionOn,
Vec3)
where

type Transmission = {
  estado :: Boolean,
  tv :: String,
  mapping :: String,
  tvZone :: Int,
  channel :: String,
  size :: Vec3,
  position :: Vec3,
  rotation :: Vec3
  }

defTransmission :: Transmission
defTransmission = {
  estado: false,
  tv: "3dObjects/cubo.obj",
  mapping: "3dObjects/cubo.mtl",
  tvZone: 0,
  channel: "textures/static.mov",
  size: defSize,
  position: defPosition,
  rotation: defRotation
}

defTransmissionOn :: Transmission
defTransmissionOn = defTransmission {estado = true, channel = "textures/04.mov"}

----

type Vec3 = {
  x :: Number,
  y :: Number,
  z :: Number
  }

defSize :: Vec3
defSize = {x: 1.0, y: 1.0,z: 1.0}

defPosition :: Vec3
defPosition = {x: 0.0, y: 0.0,z: 0.0}

defRotation :: Vec3
defRotation = {x: 0.0, y: 0.0,z: 0.0}
