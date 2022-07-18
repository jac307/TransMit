module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude --(Unit, bind, discard, pure, ($), (/))
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


type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
  video :: Ref (Maybe HTML2.HTMLMediaElement),
  textureLoader :: Ref (Maybe TJS.TextureLoader),
  mesh :: Ref (Maybe TJS.Mesh),
  program :: Ref AST
  }

{-
type Monitor = {
  video :: Ref (Maybe ?Video),
  textureLoader :: Ref (Maybe TJS.TextureLoader),
  --object :: Ref (Maybe TJS.OBJ)
  --map :: Ref (Maybe TJS.MTL)
  mesh :: Ref (Maybe TJS.Mesh),
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

  video' <- TJS.createElement "video"
  getURL <- HTML2.currentSrc video'
  log $ "url: " <> (show getURL)

  video <- new Nothing
  textureLoader <- new Nothing
  mesh <- new Nothing

  program <- new defaultProgram

  let re = {scene, camera, renderer, video, textureLoader, mesh, program}
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
runProgram re (Just (Transmission (LiteralTransmission false))) = tranmissionOn re
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

-- tranmissionOff :: RenderEngine -> Effect Unit
-- tranmissionOff re = do
--   m <- seeIfCubeIfNotMakeOne re
--   vt <- setVideoURL re "textures/static.jpg"

---------------

seeIfCubeIfNotMakeOne :: RenderEngine ->  Effect TJS.Mesh
seeIfCubeIfNotMakeOne re = do
  c <- read re.mesh -- see if there is a cube
  case c of
    Just m -> pure m -- if so, do nothing
    Nothing -> do -- if not, then create one
      nm <- do
        geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
        material <- TJS.meshBasicMaterial { colour: 0x00ff00 }
        cube <- TJS.newMesh geometry material
        pure cube
      write (Just nm) re.mesh -- write mesh into the RenderEngine
      pure nm

-- deleteCubeIfThereIsOne :: RenderEngine -> Effect Unit
-- deleteCubeIfThereIsOne re = do
--   c <- read re.mesh
--   case c of
--     Nothing -> pure unit
--     Just m -> do
--       ...delete the mesh from the threejs scene...
--       write Nothing re.mesh

---------------

-- setTexture???

setVideoURL :: RenderEngine -> String -> Effect Unit
setVideoURL re s = do
  velem <- toHTMLMediaElement re -- :: HTML2.HTMLMediaElement
  url <- getVideoURL re -- :: String
  case url of
    "" -> do -- if "", then add a url with the s input
      HTML2.setSrc s velem -- add new url
      addDefsToHTMLMediaElement velem -- autoplay,load,etc.
    otherwise -> do -- if not "", then:
      let b = compareURLs url s -- :: Boolean -- compare s to currentURL
      case b of
        true -> pure unit -- just continue playing, IS THIS CORRECT?
        false -> do
          HTML2.pause velem -- stop current video
          HTML2.setSrc s velem -- load new url
          addDefsToHTMLMediaElement velem -- autoplay,load,etc.


compareURLs :: String -> String -> Boolean
compareURLs s1 s2
  | s1 == s2 = true
  | otherwise = false


-- maybeURL :: String -> Maybe String
-- maybeURL s
--   | s == "" = Nothing
--   | otherwise = Just s


toHTMLMediaElement :: RenderEngine -> Effect HTML2.HTMLMediaElement
toHTMLMediaElement re = do
  v <- read re.video -- :: Maybe HTML2.HTMLMediaElement
  case v of
    Just x -> pure x
    Nothing -> createVideoElement re


getVideoURL :: RenderEngine -> Effect String
getVideoURL re = do
  velem <- read re.video -- :: Effect (Maybe HTMLMediaElement)
  case velem of
    Just e -> HTML2.currentSrc e
    Nothing -> do
      --- THIS MAY CHANGE!!!
      elem <- createVideoElement re -- :: HTMLMediaElement
      HTML2.src elem -- src :: HTMLMediaElement -> Effect String


createVideoElement :: RenderEngine -> Effect HTML2.HTMLMediaElement
createVideoElement re = do
  velem <- read re.video -- is there already a video element?
  case velem of
    Just e -> pure e -- if so do nothing
    Nothing -> do -- if not make one
      video' <- TJS.createElement "video"
      addDefsToHTMLMediaElement video'
      write (Just video') re.video
      pure video'


addDefsToHTMLMediaElement :: HTML2.HTMLMediaElement -> Effect Unit
addDefsToHTMLMediaElement velem = do
  TJS.preloadAnything velem
  HTML2.load velem
  HTML2.setAutoplay true velem
  HTML2.setLoop true velem
  HTML2.setMuted false velem

------------------------------------


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
