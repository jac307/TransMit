module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate,
Monitor
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

type Monitor = {
  video :: Ref (Maybe HTML2.HTMLMediaElement), -- could just without Ref and Maybe  ... add videoTexture... both can be without Ref and Maybe
  --url :: Ref (Maybe String),
  --object :: Ref (Maybe TJS.OBJ), -- only when it changes then update something
  --map :: Ref (Maybe TJS.MTL),
  mesh :: Ref (Maybe TJS.Mesh)
  }

defMonitor :: Effect Monitor
defMonitor = do
  video <- new Nothing -- it will be an actual velem
  mesh <- new Nothing
  let mo = {video, mesh}
  pure mo

-- updateMonitor :: RenderEngine -> Ref (Maybe Monitor) -> Statement -> Effect Unit
-- if there's no monitor:
-- make all of the fields appropriately
-- if there's already a monitor, then...
-- if transmission on:
-- make sure there's a cube (update Ref (Maybe TJS.Mesh) if not)
-- make sure the cube has the right video (update ... if not)
-- if transmission off:
-- ...

-- deleteMonitor :: RenderEngine -> Ref (Maybe Monitor) -> Effect Unit
-- if there's no monitor, nothing to do
-- if there is: delete everything and update the refs in Ref Monitor

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
  cube <- seeIfMeshIfNotMakeOne re.scene mo "textures/04.mov"
  playVideoElement mo

monitorOff :: RenderEngine -> Monitor -> Effect Unit
monitorOff re mo = do
  cube <- seeIfMeshIfNotMakeOne re.scene mo "textures/static.mov"
  playVideoElement mo

-- monitorOn :: RenderEngine -> Monitor -> Effect Unit
-- monitorOn re mo = do
--   updateMonitor re mo "textures/04.mov"
--   playVideoElement mo
--
-- monitorOff :: RenderEngine -> Monitor -> Effect Unit
-- monitorOff re mo = do
--   updateMonitor re mo "textures/static.mov"
--   playVideoElement mo

------

--
-- updateMonitor :: RenderEngine -> Monitor -> String -> Effect Unit -- keep in mind that it will be called repeatelly
-- updateMonitor re mo url = do
--   -- is there an url setup?...
--   v <- updateVideoURL mo url -- Effect HTMLMediaElement
--   -- updateVideoURL mo s -- :: Effect Unit -- if so, compare/change if necessary, if not, create ne
--   m <- createOrUpdateMesh re mo ... -- Effect Mesh; and it would store the mesh in the Monitor record
--   connectVideoElementToMesh v m


  -- store the video element, the url, the mesh for later

  -- for the future... the monitor would be created everytime one of these things happen: either the url changes or the geometry changes.


--   velem <- read mo.video -- read url :: Maybe String
--   case u of
--     Just x -> do -- if there is a url:
--       let b = compareURLs s (Just x) -- compare
--       case b of
--         true -> pure unit --if same, do nothing
--         false -> do -- if different, change url
--           HTML2.setSrc s v
--           addDefsToHTMLMediaElement v
--           write (Just b) mo.url -- write url
--           pure b
--     Nothing -> do -- if there is not, create one
--       nu <- do
--         HTML2.setSrc s v
--         addDefsToHTMLMediaElement v
--       write nu mo.url -- write url
--       pure nu
--
-- compareURLs :: String -> String -> Boolean
-- compareURLs s1 s2
--   | s1 == s2 = true
--   | otherwise = false


  -- u <- read mo.url -- is that url already setup? {read the url and compare} -- store url
  -- case u of
  --   Just x -> do
  --     if s == u
  --   then pure unit -- if so, there is nothing to do
  --   else do -- if not:
  --     m <- read mo.mesh -- read the mesh
  --     case m of
  --       Just x -> deleteMeshIfThereIsOne mo -- if there is an existing mesh: delete (using TJS functions) both from the scene
  --       Nothing -> do
  --         seeIfMeshIfNotMakeOne re.scene mo s -- if there no existing video mesh, then make it





-------- create mesh --------

-- createOrUpdateMesh :: RenderEngine -> Monitor -> Effect TJS.Mesh
-- createOrUpdateMesh re mo = do
--   deleteMeshIfChanged re mo -- to be non-trivial would need another argument that has to do with program/specification
--   makeMeshIfNecessary re mo
--
-- deleteMeshIfChanged :: RenderEngine -> Monitor -> Effect Unit
-- deleteMeshIfChanged re mo = do
--   m <- read mo.mesh
--   case m of
--     Nothing -> pure unit
--     Just m' -> do
--       -- right now, there's no condition that would necessitate a mesh change, so...
--       pure unit
--       -- if it did actually delete a mesh, it would write Nothing back to mo.mesh
--
--   -- is there an existing mesh that needs to be deleted? if so, delete it.
--   -- is there no mesh - if so, make one.
--
--
--
--   m <- read mo.mesh
--   case m of
--     -- should be more like: if there is an existing mesh, compare the mesh: if different, then delete mesh, if same: do nothing
--     Just x -> do
--       if m /= mo.mesh -- compara
--         then deleteMeshIfThereIsOne re mo -- :: Effect Unit
--       else pure x
--     Nothing -> do
--       nm <- seeIfMeshIfNotMakeOne' re mo -- :: Effect TJS.Mesh
--       write (Just nm) mo.mesh
    --  pure unit


seeIfMeshIfNotMakeOne' :: RenderEngine -> Monitor -> Effect TJS.Mesh
seeIfMeshIfNotMakeOne' re mo = do
  c <- read mo.mesh -- see if there is a mesh
  case c of
    Just m -> pure m -- if so, do nothing
    Nothing -> do -- if not, then create one
      velem <- toHTMLMediaElement mo -- this would be made when Monitor is first created
      vidTexture <- TJS.videoTexture velem -- this would be made when Monitor is first created
      geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
      material <- TJS.meshBasicMaterial { map: vidTexture }
      cube <- TJS.newMesh geometry material
      TJS.addAnythingToScene re.scene cube
      write (Just cube) mo.mesh -- write mesh into the Monitor
      pure cube

seeIfMeshIfNotMakeOne :: TJS.Scene -> Monitor -> String -> Effect TJS.Mesh
--seeIfMeshIfNotMakeOne :: TJS.Scene -> Monitor -> Effect TJS.Mesh
seeIfMeshIfNotMakeOne scene mo s = do
  c <- read mo.mesh -- see if there is a mesh
  case c of
    Just m -> pure m -- if so, do nothing
    Nothing -> do -- if not, then create one
      nm <- do
        velem <- toHTMLMediaElement mo
        updateVideoURL mo s
        vidTexture <- TJS.videoTexture velem
        geometry <- TJS.newBoxGeometry 2.0 2.0 2.0
        material <- TJS.meshBasicMaterial { map: vidTexture }
        cube <- TJS.newMesh geometry material
        TJS.addAnythingToScene scene cube
        pure cube
      write (Just nm) mo.mesh -- write mesh into the Monitor
      pure nm

deleteMeshIfThereIsOne :: RenderEngine -> Monitor -> Effect Unit
deleteMeshIfThereIsOne re mo = do
  c <- read mo.mesh
  case c of -- see if there is a mesh
    Nothing -> pure unit -- if not, do nothing
    Just m -> do -- if so, erase mesh
      TJS.removeAnythingFromScene re.scene mo.mesh
      write Nothing mo.mesh


-------- create velem --------

playVideoElement :: Monitor -> Effect Unit
playVideoElement mo = do
  v <- toHTMLMediaElement mo
  HTML2.play v
  --HTML2.setAutoplay true v


updateVideoURL :: Monitor -> String -> Effect Unit --HTML2.HTMLMediaElement
updateVideoURL re s = do
  velem <- toHTMLMediaElement re -- :: HTML2.HTMLMediaElement
  url <- getVideoURL re -- :: String
  if s /= url
    then do
      HTML2.setSrc s velem
      addDefsToHTMLMediaElement velem
    else pure unit


getVideoURL :: Monitor -> Effect String
getVideoURL re = do
  velem <- read re.video -- :: Effect (Maybe HTMLMediaElement)
  case velem of
    Just e -> HTML2.currentSrc e
    Nothing -> do
      --- THIS MAY CHANGE!!!
      elem <- createVideoElement re -- :: HTMLMediaElement
      HTML2.src elem -- src :: HTMLMediaElement -> Effect String


createVideoElement :: Monitor -> Effect HTML2.HTMLMediaElement
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
  HTML2.setLoop true velem
  HTML2.setMuted false velem
  HTML2.setVolume 0.0 velem

------------------------------------

toHTMLMediaElement :: Monitor -> Effect HTML2.HTMLMediaElement
toHTMLMediaElement re = do
  v <- read re.video -- :: Maybe HTML2.HTMLMediaElement
  case v of
    Just x -> pure x
    Nothing -> createVideoElement re


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
