module Main where

import Prelude
import Effect (Effect)

import RenderEngine as RE

main :: Effect RE.RenderEngine
main = RE.launch
