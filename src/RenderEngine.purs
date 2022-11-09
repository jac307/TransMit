module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude (Unit, bind, discard, pure, show, unit, ($), (/), (<>))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Web.HTML.HTMLCanvasElement as HTML
import Web.HTML.HTMLMediaElement as HTML2

import ThreeJS as TJS

import AST (AST, Statement(..), TransmissionAST(..), defaultProgram)
import Parser (parseProgram)
import MonitorState (Monitor, defMonitor, monitorOff, monitorOn, noMonitor)

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
    Nothing -> do
      noTransmission re
      TJS.render re.renderer re.scene re.camera
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
runProgram re (Just (TransmissionAST (LiteralTransmissionAST true))) = tranmissionOn re
runProgram re (Just (TransmissionAST (LiteralTransmissionAST false))) = tranmissionOff re
runProgram re _ = pure unit

-------- Tranmission Status --------

-- transmission on rodar (360)

noTransmission :: RenderEngine -> Effect Unit
noTransmission re = do
  c <- read re.monitor
  case c of
    Nothing -> do
      pure unit
    Just m -> do
      noMonitor re.scene m
      write Nothing re.monitor

tranmissionOn :: RenderEngine -> Effect Unit
tranmissionOn re = transmission re monitorOn

tranmissionOff :: RenderEngine -> Effect Unit
tranmissionOff re = transmission re monitorOff

transmission :: RenderEngine -> (TJS.Scene -> Monitor -> Effect Unit) -> Effect Unit
transmission re mState = do
  c <- read re.monitor -- :: Ref (Maybe Monitor)
  case c of
    Just m -> mState re.scene m
    Nothing -> do
      m <- defMonitor
      mState re.scene m
      write (Just m) re.monitor

--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar
