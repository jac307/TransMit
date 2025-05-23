module Main where

import Prelude (Unit, bind, pure)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Web.HTML.HTMLCanvasElement as HTML

import RenderEngine as RE

launch :: HTML.HTMLCanvasElement -> Effect RE.RenderEngine
launch = RE.launch

evaluate :: RE.RenderEngine -> String -> Effect { success :: Boolean, error :: String }
evaluate re s = do
  result <- RE.evaluate re s
  case result.error of
    Just err -> pure { success: false, error: err }
    Nothing  -> pure { success: true, error: "" }

animate :: RE.RenderEngine -> Effect Unit
animate = RE.animate
