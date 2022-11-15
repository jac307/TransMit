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


-- Transmission on --- cube with green
-- Transmission off -- cube with color white

-- uno = channel "" -- tal vez delimitarlo a unicamente urls
-- Transmission on $ switchear uno -- esto cambia vid url

-- transmission on rodar 360 rodar 120
--
-- transmission on;
-- transmission on rodar 360;
-- transmission on rodar 360 scale 0.5;
--
-- transmissionParser :: P TransmissionAST
-- transmissionParser = do
--   reserved "transmission"
--   x <- onOrOff
--   let t = LiteralTransmissionAST x
--   xs <- many transformations
--   let xs' = ... foldL on xs (including 'identity') to yield a single TransmissionAST -> TransmissionAST
--   pure $ xs' t
--
--
-- rodarParser :: P (TransmissionAST -> TransmissionAST)
-- rodarParser = do
--   reserved "rodar"
--   x <- number...
--   y <- number...
--   z <- number...
--   pure $ Rodar x y z
--
--
-- Rodar (Rodar (LiteralTransmission true) 360) 120
