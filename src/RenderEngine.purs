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
import Parsing

import ThreeJS as TJS

import AST
import Parser

type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
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
  TJS.domElement renderer

  -- creating and adding Geometry to Scene
  geometry <- TJS.newBoxGeometry 1.0 1.0 1.0
  TJS.printAnything geometry

  material <- TJS.meshBasicMaterial {color: 0x00ff00}
  TJS.printAnything material

  cube <- TJS.newMesh geometry material
  TJS.addAnythingToScene scene cube

  lights <- TJS.newHemisphereLight 0xffffbb 0x080820 1.0
  TJS.addAnythingToScene scene lights
  --TJS.addAnythingToScene scene lights

  program <- new 10.0


  let re = {scene, camera, renderer, program}
  -- TJS.requestAnimationFrame $ animate re
  pure re

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


animate :: RenderEngine -> Effect Unit
animate re = do
  p <- read re.program
  log $ "animate parser: " <> (show p)
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


  -- video <- TJS.getElementById "video2"
  -- TJS.print video
  -- videoTexture <- TJS.videoTexture video
  -- TJS.print videoTexture

  -- video <- TJS.createElement "video"
  -- TJS.srcOfElement video "textures/leo.mov"
  -- TJS.preloadAnything video
  -- TJS.loop video true
  -- TJS.muted video true
  -- -- TJS.volume video 0.0
  -- TJS.autoplay video true
  -- TJS.play video

  -- videoTexture <- TJS.videoTexture video

  -- imgTexture <- TJS.textureLoader "textures/cellos.jpg"
  -- TJS.wrapS imgTexture TJS.mirroredRepeatWrapping
  -- TJS.wrapT imgTexture TJS.mirroredRepeatWrapping
  -- TJS.setRepeatOfAnything imgTexture 2.0 2.0
  -- TJS.print imgTexture

  -- material <- Material.createMeshBasic {map: imgTexture} --{color: 0x00ff00}
  -- TJS.print material

  -- object <- new Nothing
  -- -- creating and adding Geometry to Scene
  -- addingOBJtoScene scene "3dObjects/cubo.obj" object
  --
  -- let re = {scene, camera, renderer, object}
  --
  -- TJS.requestAnimationFrame $ animate re
  -- pure re





-- OLD



--
-- type RenderState =
--   {
--   texture :: String
--   geometry :: (String,String)
--   light :: Hexadecimal
--   }
--
-- defRenderState :: RenderState
-- defRenderState = defRenderState {
--   texture = "textures/04.mov",
--   geometry = ("obj/exp.mtl","obj/exp.obj"),
--   light = 0xffff,
--   }
--
--
-- launchRenderEngine :: Effect RenderEngine
-- launchRenderEngine = do
--   scene <- Scene.create
--   camera <- Camera.createPerspective 75 (16/9) 0.1 1000 --maybe later on it can receive a Window parameter so I can do innerWidth
--   renderer <- Renderer.createWebGL
--   Renderer.setSize renderer 500 800
--   Renderer.appendToDomByID renderer "canvas"
--   Object3D.setPosition camera 0 0 6
--   programRef <- new ?
--   programState <- defRenderState
--   let re = {scene, camera, renderer, programRef, programState}
--   requestAnimationFrame $ animate re
--   pure re
--
--
-- type Texture =
--   {
--   ?
--   }
--
-- type Geometry =
--   {
--   ?
--   }
--
-- type Light =
--   {
--   ?
--   }
--
-- addTexture :: RenderEngine -> Texture -> ?
-- addTexture = do
--   video <-
--
--   videoTexture
--
--
--   geometry <- Geometry.createBox 1.0 1.0 1.0
--   material <- Material.createMeshBasic { color: "red" }
--   mesh <- Object3D.createMesh geometry material
--   Scene.addObject re.scene mesh
--   pure { mesh }
--
-- const geometry = new THREE.BoxGeometry( 1, 1, 1 );
-- const material = new THREE.MeshBasicMaterial( {color: 0x00ff00} );
-- const cube = new THREE.Mesh( geometry, material );
-- scene.add( cube );
--
-- addGeometry :: RenderEngine -> Geometry -> ?
-- addGeometry re g= do
--   mtlLoader
--   objLoader
--
--
--
--
--
--
--
--
--
-- addLight :: RenderEngine -> Light -> ?
-- addLight = do
