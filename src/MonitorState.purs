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
import Effect.Class.Console (log, error)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe
import Web.HTML.HTMLCanvasElement as HTML
import Web.HTML.HTMLMediaElement as HTML2

import ThreeJS as TJS

import Transmission (Transmission, Vec3)

type Monitor = {
  -- texture
  currVidURL :: Ref String,
  video :: HTML2.HTMLMediaElement,
  vidTexture :: TJS.TextureLoader,
  -- object
  currObjURL :: Ref String,
  geometry :: Ref (Maybe TJS.OBJ),
  -- material
  currMtlURL :: Ref String,
  material :: Ref (Maybe TJS.MTL),
  -- mesh
  mesh :: Ref (Maybe TJS.Mesh)
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
  geometry <- new Nothing
  -- material
  currMtlURL <- new defURL
  material <- new Nothing
  -- mesh
  mesh <- new Nothing
  let mo = {currVidURL, video, vidTexture, currObjURL, geometry, currMtlURL, material, mesh}
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
  removeGeometry sc mo
  removeMaterial sc mo

updateMonitor :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
updateMonitor sc mo t = do
  -- 1. change video url if necessary
  updateURLfromVidElem mo t.channel
  -- 2. change/load geometry url/object
  changeOrLoadGeoIfNecessary sc mo t.tv t.tvZone
  -- 3. change/load material url/create new mesh
  changeOrLoadMatIfNecessary sc mo t.mapping t.tvZone
  -- 4. transform Mesh
  transformMesh sc mo t

---- Geometry ---

changeOrLoadGeoIfNecessary :: TJS.Scene -> Monitor -> String -> Int -> Effect Unit
changeOrLoadGeoIfNecessary sc mo url z = do
  currURL <- read mo.currObjURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      removeGeometry sc mo -- remove and dispose geometry
      loader <- TJS.newOBJLoader
      TJS.loadOBJ loader url $ \o -> do
        write (Just o) mo.geometry
        tryToMakeMesh sc mo z
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
        tryToMakeMesh sc mo z
      write url mo.currMtlURL

---- Mesh ---

tryToMakeMesh :: TJS.Scene -> Monitor -> Int -> Effect Unit
tryToMakeMesh sc mo z = do
  g <- read mo.geometry
  case g of
    Nothing -> pure unit
    Just g' -> do
      m <- read mo.material
      case m of
        Nothing -> pure unit
        Just m' -> makeMesh sc g' m' z mo.vidTexture

removeGeometry :: TJS.Scene -> Monitor -> Effect Unit
removeGeometry sc mo = do
  g <- read mo.geometry
  case g of
    Nothing -> pure unit
    Just g' -> do
      TJS.disposeAnything g'
      TJS.removeObject3D sc g'
      write Nothing mo.geometry

removeMaterial :: TJS.Scene -> Monitor -> Effect Unit
removeMaterial sc mo = do
  m <- read mo.material
  case m of
    Nothing -> pure unit
    Just m' -> do
      TJS.disposeAnything m'
      TJS.removeObject3D sc m'
      write Nothing mo.material

makeMesh :: TJS.Scene -> TJS.OBJ -> TJS.MTL -> Int -> TJS.TextureLoader -> Effect Unit
makeMesh sc g m z vt = do
  -- 1. combine the three things to make a mesh
  mapVidTextToMat m vt
  mapMatToObj g z m
  -- 2. add mesh to scene
  TJS.addAnythingToScene sc g

-------- Transform Mesh --------

transformMesh :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
transformMesh sc mo t = do
  g <- read mo.geometry
  case g of
    Nothing -> pure unit
    Just o -> transformMesh' o t

transformMesh' :: TJS.OBJ -> Transmission -> Effect Unit
transformMesh' g t = do
  TJS.setScaleOfAnything g (v3ToX t.size) (v3ToY t.size) (v3ToZ t.size)
  TJS.setPositionOfAnything g (v3ToX t.position) (v3ToY t.position) (v3ToZ t.position)
  TJS.setRotationOfAnything g (v3ToX t.rotation) (v3ToY t.rotation) (v3ToZ t.rotation)

v3ToX :: Vec3 -> Number
v3ToX v3 = v3.x

v3ToY :: Vec3 -> Number
v3ToY v3 = v3.y

v3ToZ :: Vec3 -> Number
v3ToZ v3 = v3.z

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
