module Transmission where

type Transmission = {
  estado :: Boolean, -- transmission on (channel) or off (static)
  tv :: String, -- geometry of the monitor obj file
  mapping :: String, -- how video envolves monitor mtl file
  tvZone :: Int,
  channel :: String, -- video playing
  channelReapeater :: Vec2, -- how many times the video repeats on the monitor
  fulcober :: String,
  size :: Vec3,
  position :: Vec3,
  rotation :: Vec3 -- Either Vec3 Vec3 --- Left absolute values, Right dynamic values (addition like in the Three.js value: rotation.x += 1 * 0.010;)
  }

-- add video material for ico - [0], ico2 needs element [1], globe nees the element [0]
defTransmission :: Transmission
defTransmission = {
  estado:             false,
  tv:                 "3dObjects/cubo.obj",
  mapping:            "3dObjects/cubo.mtl",
  tvZone:             0,
  channel:            "channels/static.mov",
  channelReapeater:   {x: 1.0, y: 1.0},
  fulcober:           "rgbaFormat",
  size:               {x: 1.5, y: 1.5,z: 1.5},
  position:           {x: 0.0, y: 0.0,z: 0.0},
  rotation:           {x: 0.5, y: 0.0,z: 0.0}
}

defTransmissionOn :: Transmission
defTransmissionOn = defTransmission {estado = true, channel = "channels/01.mov"}

----

type Vec2 = { x :: Number, y :: Number }

type Vec3 = { x :: Number, y :: Number, z :: Number }
