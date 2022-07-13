module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude --(Unit, bind, discard, pure, ($), (/))
import Effect (Effect)
import Effect.Class.Console (log)
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


type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
  mesh :: Ref (Maybe TJS.Mesh),
  video :: Ref (Maybe ?Video),
  textureLoader :: Ref (Maybe TJS.TextureLoader),
  program :: Ref AST
  --object :: Ref (Maybe TJS.OBJ)
  }

{-
type Monitor = {
  mesh :: Ref (Maybe TJS.Mesh),
  video :: Ref (Maybe ?Video),
  textureLoader :: Ref (Maybe TJS.TextureLoader),
  }

updateMonitor :: RenderEngine -> Ref (Maybe Monitor) -> Statement -> Effect Unit
-- if there's no monitor:
-- make all of the fields appropriately
-- if there's already a monitor, then...
-- if transmission on:
-- make sure there's a cube (update Ref (Maybe TJS.Mesh) if not)
-- make sure the cube has the right video (update ... if not)
-- if transmission off:
-- ...

deleteMonitor :: RenderEngine -> Ref (Maybe Monitor) -> Effect Unit
-- if there's no monitor, nothing to do
-- if there is: delete everything and update the refs in Ref Monitor
  -}

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

  program <- new defaultProgram
  mesh <- new Nothing

  let re = {scene, camera, renderer, mesh, program}
  -- TJS.requestAnimationFrame $ animate re
  pure re



animate :: RenderEngine -> Effect Unit
animate re = do
  p <- read re.program
  log $ "animate parser: " <> (show p)
  case p of
    Nothing -> TJS.render re.renderer re.scene re.camera
    Just prog -> do
      runProgram re p
      TJS.render re.renderer re.scene re.camera


evaluate :: RenderEngine -> String -> Effect (Maybe String)
evaluate re s = do
  case parseProgram s of
    Right p -> do
      write p re.program
      -- log $ "parser on renderEngine evaluate function: " <> (show p)
      -- log $ "string on renderEngine evaluate function: " <> (show s)
      pure Nothing
    Left err -> pure $ Just err


runProgram :: RenderEngine -> AST -> Effect Unit
runProgram re (Just (Transmission (LiteralTransmission true))) = tranmissionOn re
runProgram re (Just (Transmission (LiteralTransmission false))) = tranmissionOff re
runProgram re _ = pure unit
-- runProgram re Nothing = noTransmission re


tranmissionOn :: RenderEngine -> Effect Unit
tranmissionOn re = do
  video <- TJS.createElement "video"
  HTML2.setSrc "textures/04.mov" video
  TJS.preloadAnything video
  HTML2.setAutoplay true video
  HTML2.setLoop true video
  HTML2.setMuted false video
  vidTexture <- TJS.videoTexture video
  HTML2.play video
  HTML2.setVolume 0.0 video
  geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
  material <- TJS.meshBasicMaterial { map: vidTexture }
  cube <- TJS.newMesh geometry material
  TJS.printAnything cube
  TJS.addAnythingToScene re.scene cube


seeIfCubeIfNotMakeOne :: RenderEngine -> Effect TJS.Mesh
seeIfCubeIfNotMakeOne re = do
  c <- read re.mesh
  case c of
    Just m -> pure m
    Nothing -> do
      nm <- ... make a new mesh that is appropriately set up
      write nm re.mesh
      pure nm

deleteCubeIfThereIsOne :: RenderEngine -> Effect Unit
deleteCubeIfThereIsOne re = do
  c <- read re.mesh
  case c of
    Nothing -> pure unit
    Just m -> do
      ...delete the mesh from the threejs scene...
      write Nothing re.mesh






getVidTexture :: RenderEngine -> String -> Effect TJS.TextureLoader
getVidTexture re url = do
  ...



tranmissionOff :: RenderEngine -> Effect Unit
tranmissionOff re = do
  m <- seeIfCubeIfNotMakeOne re
  vt <- setVideoURL re "textures/static.jpg"


setVideoURL :: RenderEngine -> String -> Effect TJS.TextureLoader
setVideoURL re url = do
  is there already a video?
    if not make one with the provided url
    if so:
      is the url the same as the provided url
      if not:
        delete (or maybe stop?) the previous video
        load a new video with the provided url





  {- imgTexture <- TJS.textureLoader "textures/static.jpg"
  TJS.preloadAnything imgTexture
  geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
  material <- TJS.meshBasicMaterial { map: imgTexture }
  cube <- TJS.newMesh geometry material
  TJS.addAnythingToScene re.scene cube -}











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
