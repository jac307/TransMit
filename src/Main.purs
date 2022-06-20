module Main where

import Prelude
import Effect (Effect)
import Web.HTML.HTMLCanvasElement as HTML

import RenderEngine as RE
--import AST
--import Parser

launch :: HTML.HTMLCanvasElement -> Effect RE.RenderEngine
launch = RE.launch
