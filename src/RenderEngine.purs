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
  video :: HTML2.HTMLMediaElement,
  vidTexture :: TJS.TextureLoader,
  imgTexture :: TJS.TextureLoader,
  program :: Ref AST
  --object :: Ref (Maybe TJS.OBJ)
  }

launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "launch now with ineffective program"
  scene <- TJS.newScene
  camera <- TJS.newPerspectiveCamera 75.0 (16.0/9.0) 0.1 100.0
  TJS.setPositionOfAnything camera 0.0 0.0 5.0

  renderer <- TJS.newWebGLRenderer {antialias: true, canvas: cvs}
  TJS.setSize renderer 1250.0 720.0 false

  -- texture <- TJS.textureLoader "textures/static.jpg"

  video <- TJS.createElement "video"
  HTML2.setSrc "textures/04.mov" video
  TJS.preloadAnything video
  HTML2.setAutoplay true video
  HTML2.setLoop true video
  HTML2.setMuted false video
  vidTexture <- TJS.videoTexture video

  imgTexture <- TJS.textureLoader "textures/static.jpg"
  TJS.preloadAnything imgTexture


  lights <- TJS.newHemisphereLight 0xffffbb 0x080820 1.0
  TJS.addAnythingToScene scene lights

  program <- new defaultProgram

  let re = {scene, camera, renderer, video, vidTexture, imgTexture, program}
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


-- animate :: RenderEngine -> Effect Unit
-- animate re = do
--   p <- read re.program
--   --log $ "animate parser: " <> (show p)
--
--   case p of
--
--     Just prog -> do
--       if (show p) == "(Just Transmission LitTransmission true)"
--         then do
--           HTML2.play re.video
--           HTML2.setVolume 0.0 re.video
--           geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
--           material <- TJS.meshBasicMaterial { map: re.texture }
--           cube <- TJS.newMesh geometry material
--           TJS.addAnythingToScene re.scene cube
--           TJS.render re.renderer re.scene re.camera
--
--         else do
--           geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
--           material <- TJS.meshBasicMaterial {color: 0x00ff00}
--           cube <- TJS.newMesh geometry material
--           TJS.addAnythingToScene re.scene cube
--           TJS.render re.renderer re.scene re.camera
--
--     Nothing -> TJS.render re.renderer re.scene re.camera


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
runProgram re p
  | (show p) == "(Just Transmission LitTransmission true)"  = tranmissionOn  re
  | otherwise = tranmissionOff re



tranmissionOn :: RenderEngine -> Effect Unit
tranmissionOn re = do
  HTML2.play re.video
  HTML2.setVolume 0.0 re.video
  geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
  material <- TJS.meshBasicMaterial { map: re.vidTexture }
  cube <- TJS.newMesh geometry material
  TJS.addAnythingToScene re.scene cube


tranmissionOff :: RenderEngine -> Effect Unit
tranmissionOff re = do
  geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
  material <- TJS.meshBasicMaterial { map: re.imgTexture }
  cube <- TJS.newMesh geometry material
  TJS.addAnythingToScene re.scene cube











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
