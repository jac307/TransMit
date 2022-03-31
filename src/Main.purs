module Main where

import Prelude (Unit, bind, discard, pure, ($), (/))
import Effect (Effect)

import ThreeJS as TJS

import Graphics.Three.Scene as Scene
import Graphics.Three.Camera as Camera
import Graphics.Three.Renderer as Renderer
import Graphics.Three.Material as Material
import Graphics.Three.Geometry as Geometry
import Graphics.Three.Object3D as Object3D


type RenderEngine =
  {
  scene :: Scene.Scene,
  camera :: Camera.PerspectiveCamera,
  renderer :: Renderer.Renderer
  }

main :: Effect RenderEngine
main = do
  scene <- Scene.create
  camera <- Camera.createPerspective 75.0 (16.0/9.0) 0.1 100.0
  renderer <- Renderer.createWebGL { antialias: true }
  Renderer.setSize renderer 1250.0 720.0
  Renderer.appendToDomByID renderer "canvas"
  Object3D.setPosition camera 0.0 0.0 5.0
  video <- TJS.createVideoID "video"
  TJS.print video
  texture <- TJS.textureLoader "src/textures/cellos.jpg"
  TJS.wrapS texture TJS.mirroredRepeatWrapping
  TJS.wrapT texture TJS.mirroredRepeatWrapping
  TJS.setRepeatOfAnything texture 2.0 2.0
  TJS.print texture
  material <- Material.createMeshBasic {map: texture} --{color: 0x00ff00}
  TJS.print material
  geometry <- Geometry.createBox 2.0 2.0 2.0
  mesh <- Object3D.createMesh geometry material
  Scene.addObject scene mesh
  Object3D.setRotationEuler mesh 1.5 0.5 0.5
  TJS.print scene
  let re = {scene, camera, renderer}
  TJS.requestAnimationFrame $ animate re
  pure re

animate :: RenderEngine -> Effect Unit
animate re = do
  Renderer.render re.renderer re.scene re.camera
  TJS.requestAnimationFrame $ animate re
