module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude (Unit, bind, discard, pure, unit, ($), (+), (/), (>=), (<$>), (<*>), (==), (>), (-), otherwise, show, map)
import Data.List (List(..), (:), updateAt, length, snoc, null, foldMap, drop, length, zipWithA, range, take)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Prim.Boolean
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
  program :: Ref Program, -- :: List Statement
  monitors :: Ref (List Monitor)
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
  lights <- TJS.newHemisphereLight 0xffffff 0xffffff 3.0
  TJS.addAnythingToScene scene lights
  program <- new Nil
  monitors <- new Nil
  let re = {scene, camera, renderer, program, monitors}
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
      log (show p)
      write p re.program
      pure Nothing
    Left err -> pure $ Just err

----------------------------------------

runProgram :: RenderEngine -> Program -> Effect Unit
runProgram re p = do
  alignMonitors re p
  playVideoElementsInMonitors re

alignMonitors :: RenderEngine -> Program ->  Effect Unit
alignMonitors re p = do
  let tLen = length p -- :: Int
  ms <- read re.monitors -- :: List Monitor
  let mLen = length ms -- :: Int
  -- stage 1: align the number of monitors
  case mLen == tLen of
    true -> pure unit -- if same length, do nothing
    false -> do -- if different length, then:
      case mLen > tLen of
        -- if mLen is longer than tLen, then remove excess monitors
        true -> do
          let keptMonitors = take tLen ms -- monitors we keep
          let droppedMonitors = drop tLen ms -- monitors we drop
          traverse_ (removeMonitor re.scene) droppedMonitors
          write keptMonitors re.monitors
        -- if mLen is shorter than tLen, then add new monitors
        false -> do
          let howManyMonitorsAreMissing = tLen - mLen -- :: Int
          let indicesOfNewMonitors = range (mLen + 1) (mLen + howManyMonitorsAreMissing)
          traverse_ (newMonitor re) indicesOfNewMonitors
  -- stage 2: align each monitor with each tranmissions
  ms' <- read re.monitors
  _ <- zipWithA (updateMonitor re.scene) ms' p
  pure unit

--

newMonitor :: RenderEngine -> Int -> Effect Unit
newMonitor re i = do
  m <- read re.monitors -- :: List Monitor
  dm <- defMonitor -- :: Monitor
  let m' = replaceAt i dm m -- :: List Monitor
  write m' re.monitors

replaceAt :: forall a. Int -> a -> List a -> List a
replaceAt i v a
  | i >= length a = snoc a v
  | otherwise = fromMaybe a $ updateAt i v a

playVideoElementsInMonitors :: RenderEngine -> Effect Unit
playVideoElementsInMonitors re = do
  ms <- read re.monitors
  traverse_ playVideoElement ms -- :: Effect Unit


--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar
