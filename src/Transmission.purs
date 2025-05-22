module Transmission where

import Data.Either

type Transmission = {
  estado :: Boolean, -- transmission on (channel) or off (static)
  tv :: String, -- geometry of the monitor obj file
  mapping :: String, -- how video envolves monitor mtl file
  channel :: String, -- video playing
  volume :: Number,
  channelReapeater :: Vec2, -- how many times the video repeats on the monitor
  fulcober :: String,
  translucidez :: Number,
  colour :: Vec3, -- missing application in parser
  emissionColour :: Vec3, -- missing application in parser
  emissionIntensity :: Number, -- missing application in parser
  size :: Number,
  position :: Vec3,
  rotation :: DynVec3
  }


defTransmission :: Transmission
defTransmission = {
  estado:             false,
  tv:                 "https://jac307.github.io/TransMit/monitors/oldi0.obj",
  mapping:            "https://jac307.github.io/TransMit/monitors/oldi0.mtl",
  volume:             0.03,
  channel:            "https://jac307.github.io/TransMit/channels/static.mp4",
  channelReapeater:   {x: 1.0, y: 1.0},
  fulcober:           "rgbaFormat",
  translucidez:       1.0,
  colour:             {x: 0.6, y: 0.6, z: 0.6}, -- rgb
  emissionColour:     {x: 0.0, y: 0.0, z: 0.0}, -- rgb
  emissionIntensity:  0.5,
  size:               1.0,
  position:           {x: 0.0, y: 0.0, z: 0.0},
  rotation:           {x: (Right 0.5), y: (Right 0.0), z: (Right 0.0)}
}


defTransmissionOn :: Transmission
defTransmissionOn = defTransmission {estado = true, channel = "https://jac307.github.io/TransMit/channels/00.mp4"}

----

type DynVec3 = {
  x :: Either Number Number,
  y :: Either Number Number,
  z :: Either Number Number
}

type Vec2 = { x :: Number, y :: Number }

type Vec3 = { x :: Number, y :: Number, z :: Number }
