module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude (Unit, bind, discard, pure, unit, ($), (/))
import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Web.HTML.HTMLCanvasElement as HTML

import ThreeJS as TJS

import Parser (Program, parseProgram)
import MonitorState (Monitor, defMonitor, removeMonitor, updateMonitor, playVideoElement)
import Transmission (Transmission)

-- python -m SimpleHTTPServer 8000

type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
  monitor :: Ref (Maybe Monitor), -- List Monitor
  program :: Ref Program -- :: List Statement
  }

--Look in Locomotion on runElements to check how to remove the elements that are not longer there by looking at the list and comparing the list... dropping what is not there anymore and storing what is left.

-- tangentemente... look at Arrays. And the difference between List vs Array


-- runElement
--create - update
-- or
-- remove - create - update

-- look at Plane
-- newPlane, updatePlane, removePlane
--- maybe newPlace :: Scene -> Transmission? -> Effect Monitor

----------------------------------------

launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "launch now with ineffective program"
  scene <- TJS.newScene
  camera <- TJS.newPerspectiveCamera 75.0 (16.0/9.0) 0.1 100.0
  TJS.setPosition camera 0.0 0.0 5.0
  renderer <- TJS.newWebGLRenderer {antialias: true, canvas: cvs}
  TJS.setSize renderer 1250.0 720.0 false
  lights <- TJS.newHemisphereLight 0xffffff 0xffffff 3.0
  TJS.addAnythingToScene scene lights
  monitor <- new Nothing
  program <- new Nil
  let re = {scene, camera, renderer, monitor, program}
  pure re

animate :: RenderEngine -> Effect Unit
animate re = do
  p <- read re.program
  runProgram re p
  TJS.render re.renderer re.scene re.camera

evaluate :: RenderEngine -> String -> Effect (Maybe String)
evaluate re s = do
  case parseProgram s of
    Right p -> do
      write p re.program
      pure Nothing
    Left err -> pure $ Just err

----------------------------------------

runProgram :: RenderEngine -> Program -> Effect Unit
runProgram re (x:_) = runTransmission re x
runProgram re _ = removeTransmission re

runTransmission :: RenderEngine -> Transmission -> Effect Unit
runTransmission re t = do
  m <- read re.monitor -- Ref (Maybe Monitor)
  m' <- case m of
    Nothing -> defMonitor       -- :: Effect Monitor
    Just x -> pure x            -- :: Effect Monitor
  write (Just m') re.monitor           -- m' :: Monitor
  updateMonitor re.scene m' t   -- :: Effect Unit
  playVideoElement m'           -- :: Effect Unit

removeTransmission :: RenderEngine -> Effect Unit
removeTransmission re = do
  c <- read re.monitor
  case c of
    Nothing -> pure unit
    Just m -> do
      removeMonitor re.scene m
      write Nothing re.monitor

--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar
