module MonitorState
(
Monitor(..),
defMonitor,
monitorOn,
monitorOff
) where

import Prelude (Unit, unit, bind, discard, pure, show, ($), (/), (/=), (==), (<>))
import Effect (Effect)
import Effect.Class.Console (log, error)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe
import Web.HTML.HTMLCanvasElement as HTML
import Web.HTML.HTMLMediaElement as HTML2

import ThreeJS as TJS

type Monitor = {
  -- texture
  currVidURL :: Ref String,
  video :: HTML2.HTMLMediaElement,
  vidTexture :: TJS.TextureLoader, -- create in the beginning
  -- object
  currObjURL :: Ref String,
  geometry :: Ref (Maybe TJS.OBJ),
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
  geometry <- new Nothing
  -- material
  currMtlURL <- new defURL
  material <- new Nothing
  --
  let mo = {currVidURL, video, vidTexture, currObjURL, geometry, currMtlURL, material}
  pure mo

defURL :: String
defURL = ""

defVidElem :: Effect HTML2.HTMLMediaElement
defVidElem = do
  v <- TJS.createElement "video"
  HTML2.setSrc defURL v -- later, the def url should be = "textures/static.mov"
  pure v

-- change to this other type
defVidTexture :: HTML2.HTMLMediaElement -> Effect TJS.TextureLoader
defVidTexture  v = do
  vidTexture <- TJS.videoTexture v
  pure vidTexture

--------------------------------
---- monitor ---

monitorOn :: TJS.Scene -> Monitor -> Effect Unit
monitorOn sc mo = do
  updateMonitor sc mo "textures/04.mov" "t/cubo.obj" "t/cubo.mtl"
  playVideoElement mo

monitorOff :: TJS.Scene -> Monitor -> Effect Unit
monitorOff sc mo = do
  updateMonitor sc mo "textures/static.mov" "3dObjects/cubo.obj" "3dObjects/cubo2.mtl"
  playVideoElement mo

updateMonitor :: TJS.Scene -> Monitor -> String -> String -> String -> Effect Unit
updateMonitor sc mo vURL objURL mtlURL = do
  -- 1. change video url if necessary
  updateURLfromVidElem mo vURL
  -- 2. change/load geometry url/object
  changeOrLoadGeoIfNecessary sc mo objURL
  -- 3. change/load material url/create new mesh
  changeOrLoadMatIfNecessary sc mo mtlURL
  -- add/complete
  -- if( video.readyState === video.HAVE_ENOUGH_DATA ) videoTexture.needsUpdate	= true;

---- Geometry ---

changeOrLoadGeoIfNecessary :: TJS.Scene -> Monitor -> String -> Effect Unit
changeOrLoadGeoIfNecessary sc mo url = do
  currURL <- read mo.currObjURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      loader <- TJS.newOBJLoader
      TJS.loadOBJ loader url $ \o -> do
        write (Just o) mo.geometry
        tryToMakeMesh sc mo
      write url mo.currObjURL

---- Material ---

changeOrLoadMatIfNecessary :: TJS.Scene -> Monitor -> String -> Effect Unit
changeOrLoadMatIfNecessary sc mo url = do
  currURL <- read mo.currMtlURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      loader <- TJS.newMTLLoader
      TJS.loadMTL loader url $ \m -> do
        preloadMaterials m
        --TJS.printAnything m
        write (Just m) mo.material
        tryToMakeMesh sc mo
      write url mo.currMtlURL

---- Mesh ---

-- note: only called if something has been just loaded
tryToMakeMesh :: TJS.Scene -> Monitor -> Effect Unit
tryToMakeMesh sc mo = do
  -- if there is already a mesh, delete it
  --deleteMeshIfThereIsOne re m -- ie. delete from scene and Monitor(ref)
  -- if in the refs you have a geometry and a material then
  g <- read mo.geometry
  case g of
    Nothing -> pure unit
    Just g' -> do
      m <- read mo.material
      case m of
        Nothing -> pure unit
        Just m' -> makeMesh sc g' m' mo.vidTexture

makeMesh :: TJS.Scene -> TJS.OBJ -> TJS.MTL -> TJS.TextureLoader -> Effect Unit
makeMesh sc g m vt = do
  -- 1. combine the three things to make a mesh
  -- TJS.printAnything m
  -- TJS.printAnything g
  -- TJS.printAnything vt
  mapVidTextToMat m vt
  mapMatToObj g 0 m
  -- 2. add mesh to scene
  TJS.addAnythingToScene sc g

-- Imported Functions --
foreign import preloadMaterials :: TJS.MTL -> Effect Unit
foreign import mapVidTextToMat :: TJS.MTL -> TJS.TextureLoader -> Effect Unit
foreign import mapMatToObj :: TJS.OBJ -> Int -> TJS.MTL -> Effect Unit

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
