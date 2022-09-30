module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
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
runProgram re _ = pure unit
-- runProgram re Nothing = noTransmission re

-------- Tranmission Status --------

tranmissionOn :: RenderEngine -> Effect Unit
tranmissionOn re = do
  c <- read re.monitor -- :: Ref (Maybe Monitor)
  case c of
    Just m -> monitorOn re.scene m -- :: TJS.Scene -> Monitor
    Nothing -> do
      m <- defMonitor
      monitorOn re.scene m
      write (Just m) re.monitor

tranmissionOff :: RenderEngine -> Effect Unit
tranmissionOff re = do
  c <- read re.monitor -- :: Ref (Maybe Monitor)
  case c of
    Just m -> monitorOff re.scene m
    Nothing -> do
      m <- defMonitor
      monitorOff re.scene m
      write (Just m) re.monitor

--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar
