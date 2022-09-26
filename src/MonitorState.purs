module MonitorState where
-- (
-- Monitor(..),
-- monitorOn,
-- monitorOff
-- ) where

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
  vidTexture :: TJS.TextureLoader,
  -- object
  currObjURL :: Ref String,
  geometry :: Ref (Maybe TJS.OBJ),
  -- material
  currMtlURL :: Ref String,
  material :: Ref (Maybe TJS.MTL)
  }

defMonitor :: Effect Monitor
defMonitor = do
  -- texture
  currVidURL <- new defURL
  video <- defVidElem
  vidTexture <- defVidTexture
  -- object
  currObjURL <- new defURL
  geometry <- new Nothing
  -- material
  currMtlURL <- new defURL
  material <- new Nothing
  let mo = {currVidURL, video, vidTexture, currObjURL, geometry, currMtlURL, material}
  pure mo

defURL :: String
defURL = ""

defVidElem :: Effect HTML2.HTMLMediaElement
defVidElem = do
  v <- TJS.createElement "video"
  HTML2.setSrc defURL v -- later, the def url should be = "textures/static.mov"
  pure v

defVidTexture :: Effect TJS.TextureLoader
defVidTexture = do
  v <- defVidElem
  vidTexture <- TJS.videoTexture v -- :: Effect TJS.TextureLoader
  pure vidTexture


---- Geometry ---

changeOrLoadGeoIfNecessary :: Monitor -> String -> Effect Unit
changeOrLoadGeoIfNecessary mo url = do
  currURL <- read mo.currObjURL
  --compare the specified URL to the stored URL to see whether a load should be triggered
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      g <- read mo.geometry -- :: Maybe TJS.OBJ
      case g of -- check if there is a geometry
        Just x -> pure unit -- what happens if there is one already?
        Nothing -> do -- if there isn't
          -- try to make a new mesh from stuff stored in Monitor
          loader <- TJS.newOBJLoader
          TJS.loadOBJ loader url $ \o -> do
            -- should I load the o to the scene?
            write (Just o) mo.geometry
            write url mo.currObjURL
            pure unit

---- Material ---

changeOrLoadMatIfNecessary :: Monitor -> String -> Effect Unit
changeOrLoadMatIfNecessary mo url = do
  currURL <- read mo.currMtlURL
  --compare the specified URL to the stored URL to see whether a load should be triggered
  if url == currURL
    then (pure unit)
    else do -- if load is triggered
      m <- read mo.material -- check if there is a material
      case m of
        Just x -> pure unit
        Nothing -> do
          loader <- TJS.newMTLLoader
          TJS.loadMTL loader url $ \o -> do
            write (Just o) mo.material
            write url mo.currMtlURL
            pure unit

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
  -- combine the three things to make a mesh
  mapVidTextToMat m vt
  mapMatToObj g m 1
  setMaterials g m
  -- add the mesh to the scene
  TJS.addAnythingToScene sc g
  -- store the mesh in the appropriate ref
  pure unit

-- Imported Functions --
foreign import setMaterials :: TJS.OBJ -> TJS.MTL -> Effect Unit
foreign import mapVidTextToMat :: TJS.MTL -> TJS.TextureLoader -> Effect Unit
foreign import mapMatToObj :: TJS.OBJ -> TJS.MTL -> Int -> Effect Unit











-------- vTexture --------

updateVideoTexture :: Monitor -> String -> Effect TJS.TextureLoader
updateVideoTexture mo url = do
  updateURLfromVidElem mo url -- Effect HTMLMediaElement
  vt <- TJS.videoTexture mo.video -- :: TJS.TextureLoader
  pure vt

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
