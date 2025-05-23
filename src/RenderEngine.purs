module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude (Unit, bind, discard, otherwise, pure, unit, ($), (+), (-), (/), (==), (>), (>=), (<>))
import Data.List (List(..), drop, length, range, snoc, take, updateAt, zipWithA)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Traversable (traverse_)
import Web.HTML.HTMLCanvasElement as HTML

import ThreeJS as TJS

import MonitorState (Monitor, defMonitor, removeMonitor, alignMonitor, playVideoElement)
import Parser (Program, parseProgram)

-- python3 -m http.server 8000

type RenderEngine =
  {
  scene :: TJS.Scene,
  camera :: TJS.PerspectiveCamera,
  renderer :: TJS.Renderer,
  program :: Ref Program,
  monitors :: Ref (List Monitor)
  }

----------------------------------------

launch :: HTML.HTMLCanvasElement -> Effect RenderEngine
launch cvs = do
  log "launch now with ineffective program"
  scene <- TJS.newScene
  iWidth <- TJS.windowInnerWidth
  iHeight <- TJS.windowInnerHeight
  camera <- TJS.newPerspectiveCamera 45.0 (iWidth/iHeight) 0.1 100.0
  TJS.setPosition camera 0.0 0.0 15.0
  renderer <- TJS.newWebGLRenderer {antialias: true, canvas: cvs}
  TJS.setSize renderer iHeight iWidth false
  lights <- TJS.newHemisphereLight 0xffffff 0xffffff 3.0
  TJS.addAnythingToScene scene lights
  program <- new { transmissions: Nil, baseURL: "https://jac307.github.io/TransMit/channels/" }
  monitors <- new Nil
  let re = {scene, camera, renderer, program, monitors}
  pure re

animate :: RenderEngine -> Effect Unit
animate re = do
  p <- read re.program
  runProgram re p
  TJS.render re.renderer re.scene re.camera

evaluate :: RenderEngine -> String -> Effect { error :: Maybe String }
evaluate re s = do
  case parseProgram s of
    Right program -> do
      log $ "Parsed baseURL: " <> program.baseURL
      write program re.program
      pure { error: Nothing }
    Left err -> pure { error: Just err }


----------------------------------------

runProgram :: RenderEngine -> Program -> Effect Unit
runProgram re p = do
  alignMonitors re p
  playVideoElementsInMonitors re

-- new
alignMonitors :: RenderEngine -> Program -> Effect Unit
alignMonitors re program = do
  let transmissions = program.transmissions
  let tLen = length transmissions
  ms <- read re.monitors
  let mLen = length ms

  -- Stage 1: Align the number of monitors
  case mLen == tLen of
    true -> pure unit
    false ->
      case mLen > tLen of
        true -> do
          let keptMonitors = take tLen ms
          let droppedMonitors = drop tLen ms
          traverse_ (removeMonitor re.scene) droppedMonitors
          write keptMonitors re.monitors
        false -> do
          let howMany = tLen - mLen
          let indices = range (mLen + 1) (mLen + howMany)
          traverse_ (newMonitor re) indices

  -- Stage 2: Align each monitor with each transmission
  ms' <- read re.monitors
  _ <- zipWithA (alignMonitor re.scene) ms' transmissions
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
