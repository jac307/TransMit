module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate,
Monitor
) where

import Prelude (Unit, unit, bind, discard, pure, show, ($), (/), (/=), (<>))
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

-- python -m SimpleHTTPServer 8000

type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
  monitor :: Ref (Maybe Monitor),
  program :: Ref AST
  }

------------- monitor -------------------

--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar

type Monitor = {
  video :: HTML2.HTMLMediaElement,
  vidTexture :: TJS.TextureLoader,
  mesh :: Ref (Maybe TJS.Mesh)
  }

defMonitor :: Effect Monitor
defMonitor = do
  video <- defVidElem
  vidTexture <- defVidTexture
  mesh <- new Nothing
  let mo = {video, vidTexture, mesh}
  pure mo

defVidElem :: Effect HTML2.HTMLMediaElement
defVidElem = do
  v <- TJS.createElement "video"
  HTML2.setSrc "" v -- later, the def url should be = "textures/static.mov"
  pure v

defVidTexture :: Effect TJS.TextureLoader
defVidTexture = do
  v <- defVidElem
  vidTexture <- TJS.videoTexture v -- :: Effect TJS.TextureLoader
  pure vidTexture


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
runProgram re _ = pure unit
-- runProgram re Nothing = noTransmission re


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

-------- monitorOn and monitorff --------

monitorOn :: RenderEngine -> Monitor -> Effect Unit
monitorOn re mo = do
  updateMonitor re mo "textures/04.mov"
  playVideoElement mo

monitorOff :: RenderEngine -> Monitor -> Effect Unit
monitorOff re mo = do
  updateMonitor re mo "textures/static.mov"
  playVideoElement mo

-------- monitor --------

-- --keep in mind that it will be called repeatelly
updateMonitor :: RenderEngine -> Monitor -> String -> Effect Unit
updateMonitor re mo url = do
  vt <- updateVideoTexture mo url -- :: TJS.TextureLoader
  m <- createOrUpdateMesh re mo vt -- :: Effect TJS.Mesh
  pure unit
  --connectVideoElementToMesh v m

-------- mesh --------

createOrUpdateMesh :: RenderEngine -> Monitor -> TJS.TextureLoader -> Effect TJS.Mesh
createOrUpdateMesh re mo vt = do
  --deleteMeshIfChanged re mo -- to be non-trivial would need another argument that has to do with program/specification
  makeMeshIfNecessary re mo vt

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

-------- vTexture --------

updateVideoTexture :: Monitor -> String -> Effect TJS.TextureLoader
updateVideoTexture mo url = do
  updateURLfromVidElem mo url -- Effect HTMLMediaElement
  vt <- TJS.videoTexture mo.video -- :: TJS.TextureLoader
  pure vt

-------- vElem --------

playVideoElement :: Monitor -> Effect Unit
playVideoElement mo = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  HTML2.play v


updateURLfromVidElem :: Monitor -> String -> Effect Unit
updateURLfromVidElem mo url = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  currURL <- HTML2.currentSrc v -- :: String
  if url /= currURL
    then do
      HTML2.setSrc url v
      TJS.preloadAnything v
      HTML2.load v
      HTML2.setLoop true v
      HTML2.setMuted false v
      HTML2.setVolume 0.0 v
    else (pure unit)

-------- end --------


----------------

-- animate :: RenderEngine -> Effect Unit
-- animate re = do
--
--   o <- read re.object
--   case o of
--     Just o' -> do
--       p <- TJS.getPositionOfAnything o'
--       log $ show p
--     Nothing -> log "nothing"
--
--   TJS.render re.renderer re.scene re.camera
--   TJS.requestAnimationFrame $ animate re



  -- object <- new Nothing
  -- -- creating and adding Geometry to Scene
  -- addingOBJtoScene scene "3dObjects/cubo.obj" object
  --
  -- let re = {scene, camera, renderer, object}
  --
  -- TJS.requestAnimationFrame $ animate re
  -- pure re




  -- object <- new Nothing
  -- addingOBJtoScene scene "3dObjects/cubo.obj" object


  -- addingOBJtoScene :: TJS.Scene -> String -> Effect Unit
  -- addingOBJtoScene sc urlObj = do
  --   TJS.loadOBJ urlObj $ \object -> do
  --     TJS.addAnythingToScene sc object
  --     pure unit

  -- addingOBJtoScene :: TJS.Scene -> String -> Ref (Maybe TJS.OBJ) -> Effect Unit
  -- addingOBJtoScene sc urlObj r = do
  --   TJS.loadOBJ urlObj $ \o -> do
  --     TJS.addAnythingToScene sc o
  --     write (Just o) r
  --     --loadMaterial materialUrl $ \m -> do
  --       -- stuff to add material to object?
  --       -- stuff to store material in a ref or something?
  --     pure unit
