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
import MonitorState (Monitor, defMonitor, removeMonitor, updateMonitor, playVideoElement)
import Transmission (Transmission, defTransmission, defTransmissionOn)

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
  TJS.setPosition camera 0.0 0.0 5.0
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
  p <- read re.program -- :: AST = Maybe Statement
  log $ "animate parser: " <> (show p)
  case p of
    Nothing -> do
      removeTransmission re
      TJS.render re.renderer re.scene re.camera
    Just prog -> do
      runProgram re (astToProgram p) -- runs every frame
      TJS.render re.renderer re.scene re.camera

evaluate :: RenderEngine -> String -> Effect (Maybe String)
evaluate re s = do
  case parseProgram s of
    Right p -> do
      write p re.program
      pure Nothing
    Left err -> pure $ Just err

----------------------------------------

type Program = Maybe Transmission

astToProgram :: AST -> Program
astToProgram (Just (TransmissionAST (LiteralTransmissionAST false))) = Just (defTransmission)
astToProgram (Just (TransmissionAST (LiteralTransmissionAST true))) = Just (defTransmissionOn)
-- ask abot defTransmission?? should be the def or the current transmission?
-- astToProgram (Just (TransmissionAST (Movet v3 t))) = Just
astToProgram _ = Nothing

runProgram :: RenderEngine -> Program -> Effect Unit
runProgram re Nothing = removeTransmission re
runProgram re (Just t) = runTransmission re t

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
