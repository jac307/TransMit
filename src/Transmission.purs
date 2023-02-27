module Transmission where

type Transmission = {
  estado :: Boolean,
  tv :: String,
  mapping :: String,
  tvZone :: Int,
  channel :: String,
  channelReapeater :: Vec2,
  size :: Vec3,
  position :: Vec3,
  rotation :: Vec3
  }

-- add video material for ico - [0], ico2 needs element [1], globe nees the element [0]
defTransmission :: Transmission
defTransmission = {
  estado:             false,
  tv:                 "3dObjects/cubo.obj",
  mapping:            "3dObjects/cubo.mtl",
  tvZone:             0,
  channel:            "textures/static.mov",
  channelReapeater:   {x: 1.0, y: 1.0},
  size:               {x: 1.5, y: 1.5,z: 1.5},
  position:           {x: 0.0, y: 0.0,z: 0.0},
  rotation:           {x: 0.5, y: 0.0,z: 0.0}
}

defTransmissionOn :: Transmission
defTransmissionOn = defTransmission {estado = true, channel = "textures/Test-2.mp4"}

----

type Vec2 = { x :: Number, y :: Number }

type Vec3 = { x :: Number, y :: Number, z :: Number }
