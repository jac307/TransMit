module MonitorState
(
Monitor(..),
defMonitor,
removeMonitor,
updateMonitor,
playVideoElement
) where

import Prelude (Unit, unit, bind, discard, pure, show, negate, ($), (/), (/=), (==), (<>))
import Effect (Effect)
import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Effect.Class.Console (log, error)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe
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
  -- object
  currObjURL :: Ref String,
  obj :: Ref (Maybe TJS.OBJ),
  -- material
  currMtlURL :: Ref String,
  material :: Ref (Maybe TJS.MTL)
  }

----------------------------------------

defMonitor :: Effect Monitor
defMonitor = do
  -- texture
  currVidURL <- new defURL
  video <- defVidElem
  vidTexture <- defVidTexture video
  -- object
  currObjURL <- new defURL
  obj <- new Nothing
  -- material
  currMtlURL <- new defURL
  material <- new Nothing
  let mo = {currVidURL, video, vidTexture, currObjURL, obj, currMtlURL, material}
  pure mo

defURL :: String
defURL = ""

defVidElem :: Effect HTML2.HTMLMediaElement
defVidElem = do
  v <- TJS.createElement "video"
  HTML2.setSrc defURL v
  pure v

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

updateMonitor :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
updateMonitor sc mo t = do
  -- 1. change video url if necessary
  updateURLfromVidElem mo t.channel
  -- 2. change/load obj url/object
  changeOrLoadObjIfNecessary sc mo t.tv t.tvZone
  -- 3. change/load material url/create new mesh
  changeOrLoadMatIfNecessary sc mo t.mapping t.tvZone
  -- 4. transform Transmission
  transformTransmission sc mo t
  transformVidTexture mo.vidTexture t

---- Obj ---

changeOrLoadObjIfNecessary :: TJS.Scene -> Monitor -> String -> Int -> Effect Unit
changeOrLoadObjIfNecessary sc mo url z = do
  currURL <- read mo.currObjURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      removeObj sc mo -- remove and dispose obj
      loader <- TJS.newOBJLoader
      TJS.loadOBJ loader url $ \o -> do
        write (Just o) mo.obj
        tryToMakeTransmission sc mo z
      write url mo.currObjURL

---- Material ---

changeOrLoadMatIfNecessary :: TJS.Scene -> Monitor -> String -> Int -> Effect Unit
changeOrLoadMatIfNecessary sc mo url z = do
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
        tryToMakeTransmission sc mo z
      write url mo.currMtlURL

---- Mesh ---

tryToMakeTransmission :: TJS.Scene -> Monitor -> Int -> Effect Unit
tryToMakeTransmission sc mo z = do
  g <- read mo.obj
  case g of
    Nothing -> pure unit
    Just g' -> do
      m <- read mo.material
      case m of
        Nothing -> pure unit
        Just m' -> makeTransmission sc g' m' z mo.vidTexture

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

makeTransmission :: TJS.Scene -> TJS.OBJ -> TJS.MTL -> Int -> TJS.TextureLoader -> Effect Unit
makeTransmission sc g m z vt = do
  -- 1. combine the three things to make a mesh
  mapVidTextToMat m vt
  mapMatToObj g z m
  -- 2. add mesh to scene
  TJS.addAnythingToScene sc g

transformTransmission :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
transformTransmission sc mo t = do
  g <- read mo.obj -- call it: obj
  case g of
    Nothing -> pure unit
    Just o -> transformTransmission' o t

transformTransmission' :: TJS.OBJ -> Transmission -> Effect Unit
transformTransmission' g t = do
  TJS.setScaleOfAnything g (v3ToX t.size) (v3ToY t.size) (v3ToZ t.size)
  TJS.setPositionOfAnything g (v3ToX t.position) (v3ToY t.position) (v3ToZ t.position)
  TJS.setRotationOfAnything g (v3ToX t.rotation) (v3ToY t.rotation) (v3ToZ t.rotation)

-------- vidTexture --------

transformVidTexture :: TJS.TextureLoader -> Transmission -> Effect Unit
transformVidTexture vt t = do
  TJS.setRepeatOfAnything vt (v2ToX t.channelReapeater) (v2ToY t.channelReapeater)
  TJS.format vt TJS.rgbaFormat
  -- def rgbaFormat, other options: alphaFormat, redFormat, rgFormat, luminanceFormat, luminanceAlphaFormat

-------- vElem & currVidURL --------

playVideoElement :: Monitor -> Effect Unit
playVideoElement mo = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  HTML2.play v

updateURLfromVidElem :: Monitor -> String -> Effect Unit
updateURLfromVidElem mo url = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  currURL <- read mo.currVidURL -- :: String
  if url /= currURL
    then do
      HTML2.setSrc url v
      TJS.preloadAnything v
      HTML2.load v
      HTML2.setLoop true v
      HTML2.setMuted false v
      HTML2.setVolume 0.0 v
      write url mo.currVidURL -- write new info
    else (pure unit)

------------------------
-- Imported Functions --
foreign import preloadMaterials :: TJS.MTL -> Effect Unit
foreign import mapVidTextToMat :: TJS.MTL -> TJS.TextureLoader -> Effect Unit
foreign import mapMatToObj :: TJS.OBJ -> Int -> TJS.MTL -> Effect Unit

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
