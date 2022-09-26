module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate,
Monitor
) where

import Prelude (Unit, unit, bind, discard, pure, show, ($), (/), (/=), (==), (<>))
import Effect (Effect)
import Effect.Class.Console (log, error)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe
import Data.Either
--import Data.Semigroup ((<>))
--import Data.Show
import Web.HTML.HTMLCanvasElement as HTML
import Web.HTML.HTMLMediaElement as HTML2
import Parsing

import ThreeJS as TJS

import AST
import Parser
import MonitorState

-- python -m SimpleHTTPServer 8000

type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
  monitor :: Ref (Maybe Monitor),
  program :: Ref AST
  }

----------------------------------------

launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "launch now with ineffective program"
  scene <- TJS.newScene
  camera <- TJS.newPerspectiveCamera 75.0 (16.0/9.0) 0.1 100.0
  TJS.setPositionOfAnything camera 0.0 0.0 5.0
  renderer <- TJS.newWebGLRenderer {antialias: true, canvas: cvs}
  TJS.setSize renderer 1250.0 720.0 false
  lights <- TJS.newHemisphereLight 0xffffbb 0x080820 1.0
  TJS.addAnythingToScene scene lights
  monitor <- new Nothing
  program <- new defaultProgram
  let re = {scene, camera, renderer, monitor, program}
  pure re


animate :: RenderEngine -> Effect Unit
animate re = do
  p <- read re.program
  log $ "animate parser: " <> (show p)
  case p of
    Nothing -> TJS.render re.renderer re.scene re.camera
    Just prog -> do
      runProgram re p -- runs every frame
      TJS.render re.renderer re.scene re.camera


evaluate :: RenderEngine -> String -> Effect (Maybe String)
evaluate re s = do
  case parseProgram s of
    Right p -> do
      write p re.program
      pure Nothing
    Left err -> pure $ Just err

----------------------------------------

runProgram :: RenderEngine -> AST -> Effect Unit --
runProgram re (Just (Transmission (LiteralTransmission true))) = tranmissionOn re
runProgram re (Just (Transmission (LiteralTransmission false))) = tranmissionOff re
--runProgram re Nothing = noTransmission re
runProgram re _ = pure unit
-- runProgram re Nothing = noTransmission re

-------- Tranmission Status --------

tranmissionOn :: RenderEngine -> Effect Unit
tranmissionOn re = do
  c <- read re.monitor -- :: Ref (Maybe Monitor)
  case c of
    Just m -> monitorOn re m
    Nothing -> do
      m <- defMonitor
      monitorOn re m
      write (Just m) re.monitor

tranmissionOff :: RenderEngine -> Effect Unit
tranmissionOff re = do
  c <- read re.monitor -- :: Ref (Maybe Monitor)
  case c of
    Just m -> monitorOff re m
    Nothing -> do
      m <- defMonitor
      monitorOff re m
      write (Just m) re.monitor

-- noTransmission :: RenderEngine -> Effect Unit
-- noTransmission re = do
--   c <- read re.monitor
--   case c of
--     Just m -> do
--       noMonitor re m
--       write Nothing re.monitor
--     Nothing -> pure unit


--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar






-----------------------------------------------
-----------------------------------------------

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
  mesh :: Ref (Maybe TJS.Mesh),
  --
  currGltfURL :: Ref String,
  shape :: Ref (Maybe TJS.GLTF)
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
  -- mesh
  mesh <- new Nothing
  --
  currGltfURL <- new defURL
  shape <- new Nothing
  let mo = {currVidURL, video, vidTexture, currObjURL, geometry, currMtlURL, material, mesh, currGltfURL, shape}
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


--------------------------------

monitorOn :: RenderEngine -> Monitor -> Effect Unit
monitorOn re mo = do
  -- updateMonitor re mo "textures/04.mov"
  -- playVideoElement mo
  updateGltfURL mo "3dObjects/cubo3.glb"
  changeOrLoadShapeIfNecessary re mo

monitorOff :: RenderEngine -> Monitor -> Effect Unit
monitorOff re mo = do
  updateMonitor' re mo "textures/static.mov"
  playVideoElement mo

noMonitor :: RenderEngine -> Monitor -> Effect Unit
noMonitor re mo = do
  emptyVT <- updateVideoTexture mo ""
  noMesh <- deleteMesh re mo
  pure unit

-- --keep in mind that it will be called repeatelly
updateMonitor' :: RenderEngine -> Monitor -> String -> Effect Unit
updateMonitor' re mo url = do
  vt <- updateVideoTexture mo url -- :: TJS.TextureLoader
  m <- createOrUpdateMesh re mo vt -- :: Effect TJS.Mesh
  pure unit

---- new monitor ---

updateMonitor :: RenderEngine -> Monitor -> String -> String -> Effect Unit
updateMonitor re mo vURL gltlURL = do
  -- 1. change video url if necessary
  --updateVideoTexture mo vURL
  -- 2. change/load geometry url/object
  updateGltfURL mo gltlURL
  changeOrLoadShapeIfNecessary re mo
  -- 3. change/load material url/create new mesh
  pure unit













---- shape ----

updateGltfURL :: Monitor -> String -> Effect Unit
updateGltfURL mo url = do
  currURL <- read mo.currGltfURL
  if url /= currURL
    then write url mo.currGltfURL
    else (pure unit)

changeOrLoadShapeIfNecessary :: RenderEngine -> Monitor -> Effect Unit
changeOrLoadShapeIfNecessary re mo = do
  gltfURL <- read mo.currGltfURL -- :: String
  s <- read mo.shape -- :: Maybe TJS.GLTF
  case s of
    Just m -> pure unit
    Nothing -> do
      loader <- TJS.newGLTFLoader
      TJS.loadGLTF1 loader gltfURL $ \o -> do
        TJS.preloadAnything o
        let newObj = o.scene
        TJS.addAnythingToScene re.scene newObj
        write (Just o) mo.shape
        pure unit

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


-------- end --------


-------- Mesh --------

createOrUpdateMesh :: RenderEngine -> Monitor -> TJS.TextureLoader -> Effect TJS.Mesh
createOrUpdateMesh re mo vt = do
  --deleteMeshIfChanged re mo -- to be non-trivial would need another argument that has to do with program/specification
  makeMeshIfNecessary re mo vt

deleteMesh :: RenderEngine -> Monitor -> Effect Unit
deleteMesh re mo = do
  m <- read mo.mesh
  case m of
    Nothing -> pure unit
    Just m' -> do
      write Nothing mo.mesh

deleteMeshIfChanged :: RenderEngine -> Monitor -> Effect Unit
deleteMeshIfChanged re mo = do
  m <- read mo.mesh
  case m of
    Nothing -> pure unit
    Just m' -> do
      -- right now, there's no condition that would necessitate a mesh change, so...
      pure unit
      -- if it did actually delete a mesh, it would write Nothing back to mo.mesh

makeMeshIfNecessary :: RenderEngine -> Monitor -> TJS.TextureLoader -> Effect TJS.Mesh
makeMeshIfNecessary re mo vt = do
  c <- read mo.mesh -- see if there is a mesh
  case c of
    Just m -> pure m -- if so, do nothing
    Nothing -> do -- if not, then create one
      geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
      material <- TJS.meshBasicMaterial { map: vt}
      cube <- TJS.newMesh geometry material
      TJS.addAnythingToScene re.scene cube
      write (Just cube) mo.mesh -- write mesh into the Monitor
      pure cube
