module RenderEngine
(
RenderEngine(..),
launch,
animate,
evaluate
) where

import Prelude (Unit, bind, discard, pure, unit, ($), (/), (>=), (<$>), (<*>), otherwise)
import Data.List (List(..), (:), updateAt, length, snoc, null, foldMap)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Prim.Boolean
import Web.HTML.HTMLCanvasElement as HTML

import ThreeJS as TJS

import AST (Statement)
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
  program :: Ref Program, -- :: List Statement
  --
  monitors :: Ref (List Monitor),
  program2 :: Ref (List Statement)
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
  monitor <- new Nothing
  program <- new Nil
  monitors <- new Nil
  program2 <- new Nil
  let re = {scene, camera, renderer, monitor, program, monitors, program2}
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


-----

--Look in Locomotion on runElements to check how to remove the elements that are not longer there by looking at the list and comparing the list... dropping what is not there anymore and storing what is left.

-- runElements :: Array (Tuple ElementType ValueMap) -> R Unit
-- runElements xs = do
--   _ <- traverseWithIndex runElement xs
--   let nElements = length xs
--   -- remove any deleted elements
--   s <- get
--   traverse_ removeElement $ drop nElements s.elements
--   modify_ $ \x -> x { elements = take nElements x.elements }

-- tangentemente... look at Arrays. And the difference between List vs Array

-- runTranmissions :: List Monitor -> Effect Unit
-- runTranmissions xs = do
-- check list?, then:
-- either
  -- create / update elements
-- or
  -- remove, then create/update elements


runTranmission :: Int -> RenderEngine -> List Transmission -> Effect Unit
runTranmission i re ts = do
  ms <- read re.monitors -- :: List Monitor
  dm <- defMonitor -- :: Monitor
  let isListEmpty = null ms -- :: Boolean
  let ms' = case isListEmpty of
           true -> replaceAt i dm ms -- :: List Monitor
           false -> replaceAt i dm ms -- :: List Monitor
  -- re.monitors :: Ref (List Monitor)
  write ms' re.monitors        -- :: Effect Unit
  -- apply updateMonitor to each item on List Monitor
  -- give a List Tramission? since each Monitor = Transmission?
  -- updateMonitor :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
  foldMap (foldMap (updateMonitor re.scene) ms) ts
  -- playVideoElement in each item on List Monitor
  -- playVideoElement :: Monitor -> Effect Unit
  -- foldMap :: forall f m a. Foldable f => Monoid m => (a -> m) -> f a -> m
  foldMap playVideoElement ms -- Effect Unit

-- updateMonitors :: TJS.Scene -> List Monitor -> Transmission -> Effect Unit
-- updateMonitors sc ms t = foldMap (\m -> (updateMonitor sc m t)) ms

replaceAt :: forall a. Int -> a -> List a -> List a
replaceAt i v a
  | i >= length a = snoc a v
  | otherwise = fromMaybe a $ updateAt i v a


--- errores
--- primera linea> lo que le doy
--- segunda linea> lo que le debo dar
