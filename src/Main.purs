module Main where

import Prelude
import Effect (Effect)
import Data.Maybe
import Web.HTML.HTMLCanvasElement as HTML
import Effect.Class.Console (log)

import RenderEngine as RE
import AST
import Parser

launch :: HTML.HTMLCanvasElement -> Effect RE.RenderEngine
launch = RE.launch

evaluate :: RE.RenderEngine -> String -> Effect { success :: Boolean, error :: String }
evaluate re s = do
  p <- RE.evaluate re s
  case p of
    Just error -> pure $ { success: false, error }
    Nothing -> pure $ { success: true, error: "" }

animate :: RE.RenderEngine -> Effect Unit
animate = RE.animate
