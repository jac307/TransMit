module MonitorState
(
Monitor(..),
defMonitor,
removeMonitor,
alignMonitor,
playVideoElement
) where

import Prelude (Unit, unit, bind, discard, pure, show, negate, ($), (/), (/=), (==), (<>))
import Effect (Effect)
import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Effect.Class.Console (log, error)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe
import Data.Either
import Web.HTML.HTMLCanvasElement as HTML
import Web.HTML.HTMLMediaElement as HTML2
import Web.HTML.HTMLMediaElement (HTMLMediaElement)

import ThreeJS as TJS

import Transmission (Transmission, Vec3, Vec2)

type Monitor = {
  -- texture
  currVidURL :: Ref String,
  video :: HTMLMediaElement,
  vidTexture :: TJS.TextureLoader,
  vol :: Ref Number,
  -- object
  currObjURL :: Ref String,
  obj :: Ref (Maybe TJS.OBJ),
  -- material
  currMtlURL :: Ref String,
  material :: Ref (Maybe TJS.MTL),
  -- material settings
  opacity :: Ref Number,
  colour :: Ref Vec3,
  emissionColour :: Ref Vec3,
  emissionIntensity :: Ref Number
  }

----------------------------------------

defMonitor :: Effect Monitor
defMonitor = do
  -- texture
  currVidURL <- new defURL
  video <- defVidElem
  vidTexture <- defVidTexture video
  vol <- new 0.0
  -- object
  currObjURL <- new defURL
  obj <- new Nothing
  -- material
  currMtlURL <- new defURL
  material <- new Nothing
  -- material settings
  opacity <- new 1.0
  colour <- new {x: 0.0, y: 0.0, z: 0.0}
  emissionColour <- new {x: 0.0, y: 0.0, z: 0.0}
  emissionIntensity <- new 1.0
  --
  let mo = {currVidURL, video, vidTexture, vol, currObjURL, obj, currMtlURL, material, opacity, colour, emissionColour, emissionIntensity}
  pure mo

defURL :: String
defURL = ""

defVidElem :: Effect HTML2.HTMLMediaElement
defVidElem = do
  v <- TJS.createElement "video"
  crossOrigin v
  HTML2.setSrc defURL v
  pure v

foreign import crossOrigin :: HTMLMediaElement -> Effect Unit

defVidTexture :: HTML2.HTMLMediaElement -> Effect TJS.TextureLoader
defVidTexture  v = do
  vidTexture <- TJS.videoTexture v
  pure vidTexture

--------------------------------
---- monitor ---

removeMonitor :: TJS.Scene -> Monitor -> Effect Unit
removeMonitor sc mo = do
  removeObj sc mo
  removeMaterial sc mo
  stopVideoElement mo

alignMonitor :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
alignMonitor sc mo t = do
  -- 1. change video url if necessary
  updateURLfromVidElem mo t.channel
  updateVol mo t.volume
  -- 2. change/load obj url/object
  changeOrLoadObjIfNecessary sc mo t.tv t.translucidez (v3ToX t.colour) (v3ToY t.colour) (v3ToZ t.colour) (v3ToX t.emissionColour) (v3ToY t.emissionColour) (v3ToZ t.emissionColour) t.emissionIntensity
  -- 3. change/load material url/create new mesh
  changeOrLoadMatIfNecessary sc mo t.mapping t.translucidez (v3ToX t.colour) (v3ToY t.colour) (v3ToZ t.colour) (v3ToX t.emissionColour) (v3ToY t.emissionColour) (v3ToZ t.emissionColour) t.emissionIntensity
  -- 4. transform Transmission
  changeMatParametersIfNecessary sc mo t.translucidez (v3ToX t.colour) (v3ToY t.colour) (v3ToZ t.colour) (v3ToX t.emissionColour) (v3ToY t.emissionColour) (v3ToZ t.emissionColour) t.emissionIntensity
  transformTransmission sc mo t
  transformVidTexture mo.vidTexture t

---- Obj ---

changeOrLoadObjIfNecessary :: TJS.Scene -> Monitor -> String -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
changeOrLoadObjIfNecessary sc mo url t rC gC bC rE gE bE iE = do
  currURL <- read mo.currObjURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      removeObj sc mo -- remove and dispose obj
      loader <- TJS.newOBJLoader
      TJS.loadOBJ loader url $ \o -> do
        write (Just o) mo.obj
        tryToMakeTransmission sc mo t rC gC bC rE gE bE iE
      write url mo.currObjURL

---- Material ---

changeOrLoadMatIfNecessary :: TJS.Scene -> Monitor -> String -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
changeOrLoadMatIfNecessary sc mo url t rC gC bC rE gE bE iE = do
  currURL <- read mo.currMtlURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      loader <- TJS.newMTLLoader
      removeMaterial sc mo -- remove and dispose material
      TJS.loadMTL loader url $ \m -> do
        preloadMaterials m
        --TJS.printAnything m
        write (Just m) mo.material
        tryToMakeTransmission sc mo t rC gC bC rE gE bE iE
      write url mo.currMtlURL

changeMatParametersIfNecessary :: TJS.Scene -> Monitor -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
changeMatParametersIfNecessary sc mo t rC gC bC rE gE bE iE = do
  -- reading current mat settings
  currOpacity <- read mo.opacity
  currColour <- read mo.colour
  currEmissionColour <- read mo.emissionColour
  currEmissionIntensity <- read mo.emissionIntensity
  -- change opacity if necessary
  if t == currOpacity
    then (pure unit)
    else tryToMakeTransmission sc mo t rC gC bC rE gE bE iE
  -- change colour if necessary
  if {x:rC, y:gC, z:bC} == currColour
    then (pure unit)
    else tryToMakeTransmission sc mo t rC gC bC rE gE bE iE
  -- change emissive colour if necessary
  if {x:rE, y:gE, z:bE} == currEmissionColour
    then (pure unit)
    else tryToMakeTransmission sc mo t rC gC bC rE gE bE iE
  -- change emissive intensity if necessary
  if iE == currEmissionIntensity
    then (pure unit)
    else tryToMakeTransmission sc mo t rC gC bC rE gE bE iE
  -- write back curr values
  write t mo.opacity
  write {x:rC, y:gC, z:bC} mo.colour
  write {x:rE, y:gE, z:bE} mo.emissionColour
  write iE mo.emissionIntensity


---- Mesh ---

tryToMakeTransmission :: TJS.Scene -> Monitor -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
tryToMakeTransmission sc mo t rC gC bC rE gE bE iE = do
  currURL <- read mo.currObjURL
  g <- read mo.obj
  case g of
    Nothing -> pure unit
    Just g' -> do
      m <- read mo.material
      case m of
        Nothing -> pure unit
        Just m' -> makeTransmission currURL sc g' m' mo.vidTexture t rC gC bC rE gE bE iE

removeObj :: TJS.Scene -> Monitor -> Effect Unit
removeObj sc mo = do
  g <- read mo.obj
  case g of
    Nothing -> pure unit
    Just g' -> do
      TJS.disposeAnything g'
      TJS.removeObject3D sc g'
      write Nothing mo.obj

removeMaterial :: TJS.Scene -> Monitor -> Effect Unit
removeMaterial sc mo = do
  m <- read mo.material
  case m of
    Nothing -> pure unit
    Just m' -> do
      TJS.disposeAnything m'
      TJS.removeObject3D sc m'
      write Nothing mo.material


-------- Transmission --------

makeTransmission :: String -> TJS.Scene -> TJS.OBJ -> TJS.MTL -> TJS.TextureLoader -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
makeTransmission url sc g m vt t rC gC bC rE gE bE iE = do
  -- 1. combine the three things to make a mesh
  mapVidToMatNone m vt
  mapChildrenToMatNone g m
  -- Transform Material
  matTransparency g
  matOpacity g t
  matColor g rC gC bC
  matEmisColour g rE gE bE
  matEmisInt g iE
  --
  TJS.printAnything m
  -- 2. add mesh to scene
  TJS.addAnythingToScene sc g

-- Imported Functions --
foreign import preloadMaterials :: TJS.MTL -> Effect Unit
foreign import mapVidToMatNone :: TJS.MTL -> TJS.TextureLoader -> Effect Unit
foreign import mapChildrenToMatNone :: TJS.OBJ -> TJS.MTL -> Effect Unit
foreign import matTransparency :: TJS.OBJ -> Effect Unit
foreign import matOpacity :: TJS.OBJ -> Number -> Effect Unit
foreign import matColor :: TJS.OBJ -> Number -> Number -> Number -> Effect Unit
foreign import matEmisColour :: TJS.OBJ -> Number -> Number -> Number -> Effect Unit
foreign import matEmisInt :: TJS.OBJ -> Number -> Effect Unit
---

transformTransmission :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
transformTransmission sc mo t = do
  g <- read mo.obj -- call it: obj
  case g of
    Nothing -> pure unit
    Just o -> transformTransmission' o t

transformTransmission' :: TJS.OBJ -> Transmission -> Effect Unit
transformTransmission' g t = do
  TJS.setScaleOfAnything g t.size t.size t.size
  TJS.setPositionOfAnything g (v3ToX t.position) (v3ToY t.position) (v3ToZ t.position)
  setRotationX g t.rotation.x
  setRotationY g t.rotation.y
  setRotationZ g t.rotation.z

-- Set Rotation: Either dynamiRot or fixedRot
setRotationX :: TJS.OBJ -> Either Number Number -> Effect Unit
setRotationX o (Left v) = dynRotX o v
setRotationX o (Right n) = TJS.setRotationX o n

setRotationY :: TJS.OBJ -> Either Number Number -> Effect Unit
setRotationY o (Left v) = dynRotY o v
setRotationY o (Right n) = TJS.setRotationY o n

setRotationZ :: TJS.OBJ -> Either Number Number -> Effect Unit
setRotationZ o (Left v) = dynRotZ o v
setRotationZ o (Right n) = TJS.setRotationZ o n
                                 -- velocity
foreign import dynRotX :: TJS.OBJ -> Number -> Effect Unit
foreign import dynRotY :: TJS.OBJ -> Number -> Effect Unit
foreign import dynRotZ :: TJS.OBJ -> Number -> Effect Unit

-------- vidTexture --------

transformVidTexture :: TJS.TextureLoader -> Transmission -> Effect Unit
transformVidTexture vt t = do
  TJS.setRepeatOfAnything vt (v2ToX t.channelReapeater) (v2ToY t.channelReapeater)
  TJS.format vt (stringToFormatID t.fulcober)

stringToFormatID :: String -> TJS.FormatID
stringToFormatID "rgb" = TJS.rgbaFormat
stringToFormatID "rgba" = TJS.rgbaFormat
stringToFormatID "erregebe" = TJS.rgbaFormat
stringToFormatID "alcolor" = TJS.rgbaFormat
stringToFormatID "a" = TJS.alphaFormat
stringToFormatID "alpha" = TJS.alphaFormat
stringToFormatID "alfa" = TJS.alphaFormat
stringToFormatID "r" = TJS.redFormat
stringToFormatID "red" = TJS.redFormat
stringToFormatID "rojo" = TJS.redFormat
stringToFormatID "rg" = TJS.rgFormat
stringToFormatID "errege" = TJS.rgFormat
stringToFormatID "redgreen" = TJS.rgFormat
stringToFormatID "verdirojo" = TJS.rgFormat
stringToFormatID "b&w" = TJS.luminanceFormat
stringToFormatID "blackandwhite" = TJS.luminanceFormat
stringToFormatID "negriblanco" = TJS.luminanceFormat
stringToFormatID "luminancealpha" = TJS.luminanceAlphaFormat
stringToFormatID _ = TJS.rgbaFormat

-------- vElem & currVidURL --------

playVideoElement :: Monitor -> Effect Unit
playVideoElement mo = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  HTML2.play v

stopVideoElement :: Monitor -> Effect Unit
stopVideoElement mo = do
  let v = mo.video
  HTML2.pause v
  HTML2.setCurrentTime 0.0 v

--                                  url
updateURLfromVidElem :: Monitor -> String -> Effect Unit
updateURLfromVidElem mo url = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  currVol <- read mo.vol
  currURL <- read mo.currVidURL -- :: String
  if url /= currURL
    then do
      HTML2.setSrc url v
      TJS.preloadAnything v
      HTML2.load v
      HTML2.setLoop true v
      HTML2.setMuted false v
      HTML2.setVolume currVol v
      write url mo.currVidURL -- write new info
    else (pure unit)

updateVol :: Monitor -> Number -> Effect Unit
updateVol mo newVol = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  currVol <- read mo.vol -- :: Number
  if newVol /= currVol
    then do
      HTML2.setVolume newVol v
      write newVol mo.vol -- write new info
    else (pure unit)


------------------------

----- General -----

v2ToX :: Vec2 -> Number
v2ToX v2 = v2.x

v2ToY :: Vec2 -> Number
v2ToY v2 = v2.y

v3ToX :: Vec3 -> Number
v3ToX v3 = v3.x

v3ToY :: Vec3 -> Number
v3ToY v3 = v3.y

v3ToZ :: Vec3 -> Number
v3ToZ v3 = v3.z
