module MonitorState
(
Monitor(..),
defMonitor,
removeMonitor,
updateMonitor,
playVideoElement
) where

import Prelude (Unit, unit, bind, discard, pure, show, negate, ($), (/), (/=), (==), (<>))
import Effect (Effect)
import Data.Eq (class Eq)
import Data.Ord (class Ord)
import Effect.Class.Console (log, error)
import Effect.Ref (Ref, new, read, write)
import Data.Maybe
import Web.HTML.HTMLCanvasElement as HTML
import Web.HTML.HTMLMediaElement as HTML2
import Web.HTML.HTMLMediaElement (HTMLMediaElement)

import ThreeJS as TJS

import Transmission (Transmission, Vec3, Vec2)

type Monitor = {
  -- texture
  currVidURL :: Ref String,
  video :: HTMLMediaElement,
  vidTexture :: TJS.TextureLoader,
  -- object
  currObjURL :: Ref String,
  obj :: Ref (Maybe TJS.OBJ),
  -- material
  currMtlURL :: Ref String,
  material :: Ref (Maybe TJS.MTL),
  opacity :: Ref Number
  }

----------------------------------------

defMonitor :: Effect Monitor
defMonitor = do
  -- texture
  currVidURL <- new defURL
  video <- defVidElem
  vidTexture <- defVidTexture video
  -- object
  currObjURL <- new defURL
  obj <- new Nothing
  -- material
  currMtlURL <- new defURL
  material <- new Nothing
  opacity <- new 1.0
  let mo = {currVidURL, video, vidTexture, currObjURL, obj, currMtlURL, material, opacity}
  pure mo

defURL :: String
defURL = ""

defVidElem :: Effect HTML2.HTMLMediaElement
defVidElem = do
  v <- TJS.createElement "video"
  HTML2.setSrc defURL v
  pure v

defVidTexture :: HTML2.HTMLMediaElement -> Effect TJS.TextureLoader
defVidTexture  v = do
  vidTexture <- TJS.videoTexture v
  pure vidTexture

--------------------------------
---- monitor ---

removeMonitor :: TJS.Scene -> Monitor -> Effect Unit
removeMonitor sc mo = do
  removeObj sc mo
  removeMaterial sc mo

--should this function be renamed alignMonitor?
updateMonitor :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
updateMonitor sc mo t = do
  -- 1. change video url if necessary
  updateURLfromVidElem mo t.channel
  -- 2. change/load obj url/object
  changeOrLoadObjIfNecessary sc mo t.tv t.brillo (v3ToX t.colour) (v3ToY t.colour) (v3ToZ t.colour) (v3ToX t.emissionColor) (v3ToY t.emissionColor) (v3ToZ t.emissionColor) t.emissionIntensity
  -- 3. change/load material url/create new mesh
  changeOrLoadMatIfNecessary sc mo t.mapping t.brillo (v3ToX t.colour) (v3ToY t.colour) (v3ToZ t.colour) (v3ToX t.emissionColor) (v3ToY t.emissionColor) (v3ToZ t.emissionColor) t.emissionIntensity
  -- 4. transform Transmission
  changeMatParametersIfNecessary sc mo t.brillo (v3ToX t.colour) (v3ToY t.colour) (v3ToZ t.colour) (v3ToX t.emissionColor) (v3ToY t.emissionColor) (v3ToZ t.emissionColor) t.emissionIntensity
  --changeMatParametersIfNecessary sc mo t.brillo
  transformTransmission sc mo t
  transformVidTexture mo.vidTexture t

  -- (v3ToX t.position) (v3ToY t.position) (v3ToZ t.position)

---- Obj ---

changeOrLoadObjIfNecessary :: TJS.Scene -> Monitor -> String -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
changeOrLoadObjIfNecessary sc mo url brillo rC gC bC rE gE bE iE = do
  currURL <- read mo.currObjURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      removeObj sc mo -- remove and dispose obj
      loader <- TJS.newOBJLoader
      TJS.loadOBJ loader url $ \o -> do
        write (Just o) mo.obj
        tryToMakeTransmission sc mo brillo rC gC bC rE gE bE iE
      write url mo.currObjURL

---- Material ---

changeOrLoadMatIfNecessary :: TJS.Scene -> Monitor -> String -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
changeOrLoadMatIfNecessary sc mo url brillo rC gC bC rE gE bE iE = do
  currURL <- read mo.currMtlURL
  if url == currURL
    then (pure unit)
    else do -- if the load is triggered:
      loader <- TJS.newMTLLoader
      removeMaterial sc mo -- remove and dispose material
      TJS.loadMTL loader url $ \m -> do
        preloadMaterials m
        --TJS.printAnything m
        write (Just m) mo.material
        tryToMakeTransmission sc mo brillo rC gC bC rE gE bE iE
      write url mo.currMtlURL

changeMatParametersIfNecessary :: TJS.Scene -> Monitor -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
changeMatParametersIfNecessary sc mo brillo rC gC bC rE gE bE iE = do
  currOpacity <- read mo.opacity
  if brillo == currOpacity
    then (pure unit)
    else tryToMakeTransmission sc mo brillo rC gC bC rE gE bE iE
  write brillo mo.opacity

---- Mesh ---

tryToMakeTransmission :: TJS.Scene -> Monitor -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
tryToMakeTransmission sc mo brillo rC gC bC rE gE bE iE = do
  currURL <- read mo.currObjURL
  g <- read mo.obj
  case g of
    Nothing -> pure unit
    Just g' -> do
      m <- read mo.material
      case m of
        Nothing -> pure unit
        Just m' -> makeTransmission currURL sc g' m' mo.vidTexture brillo rC gC bC rE gE bE iE

removeObj :: TJS.Scene -> Monitor -> Effect Unit
removeObj sc mo = do
  g <- read mo.obj
  case g of
    Nothing -> pure unit
    Just g' -> do
      TJS.disposeAnything g'
      TJS.removeObject3D sc g'
      write Nothing mo.obj

removeMaterial :: TJS.Scene -> Monitor -> Effect Unit
removeMaterial sc mo = do
  m <- read mo.material
  case m of
    Nothing -> pure unit
    Just m' -> do
      TJS.disposeAnything m'
      TJS.removeObject3D sc m'
      write Nothing mo.material

-------- Transmission --------

makeTransmission :: String -> TJS.Scene -> TJS.OBJ -> TJS.MTL -> TJS.TextureLoader -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Effect Unit
makeTransmission url sc g m vt brillo rC gC bC rE gE bE iE = do
  -- 1. combine the three things to make a mesh
  selectMapVidToMat url m vt
  selectMapToObj url g m
  -- Transform Material
  selectMatTrans url g
  selectMatOpacity url g brillo
  selectMatColor url g rC gC bC
  selectMatEmiColor url g rE gE bE
  selectMatEmiInten url g iE
  --
  TJS.printAnything m
  -- 2. add mesh to scene
  TJS.addAnythingToScene sc g


-- Imported Functions --
foreign import preloadMaterials :: TJS.MTL -> Effect Unit
--

selectMapVidToMat :: String -> TJS.MTL -> TJS.TextureLoader -> Effect Unit
-- Material 0
selectMapVidToMat "monitors/cubo.obj" m vt = mapVidToMatMat m vt
selectMapVidToMat "monitors/cubo-1.obj" m vt = mapVidToMatMat m vt
selectMapVidToMat "monitors/cubo-2.obj" m vt = mapVidToMatMat m vt
-- None 0
selectMapVidToMat "monitors/cubo-3.obj" m vt = mapVidToMatNone m vt
selectMapVidToMat "monitors/cubo-4.obj" m vt = mapVidToMatNone m vt
selectMapVidToMat "monitors/ico.obj" m vt = mapVidToMatNone m vt
selectMapVidToMat "monitors/globe.obj" m vt = mapVidToMatNone m vt
-- None 1
selectMapVidToMat "monitors/ico2.obj" m vt = mapVidToMatNone m vt
selectMapVidToMat "monitors/exp.obj" m vt = mapVidToMatNone m vt
--
selectMapVidToMat _ m vt = mapVidToMatMat m vt

foreign import mapVidToMatMat :: TJS.MTL -> TJS.TextureLoader -> Effect Unit
foreign import mapVidToMatNone :: TJS.MTL -> TJS.TextureLoader -> Effect Unit
--

selectMapToObj :: String -> TJS.OBJ -> TJS.MTL -> Effect Unit
-- Material 0
selectMapToObj "monitors/cubo.obj" g m = mapChildrenToMatMat g m
selectMapToObj "monitors/cubo-1.obj" g m = mapChildrenToMatMat g m
selectMapToObj "monitors/cubo-2.obj" g m = mapChildrenToMatMat g m
-- None 0
selectMapToObj "monitors/cubo-3.obj" g m = mapChildrenToMatNone0 g m
selectMapToObj "monitors/cubo-4.obj" g m = mapChildrenToMatNone0 g m
selectMapToObj "monitors/ico.obj" g m = mapChildrenToMatNone0 g m
selectMapToObj "monitors/globe.obj" g m = mapChildrenToMatNone0 g m
-- None 1
selectMapToObj "monitors/ico2.obj" g m = mapChildrenToMatNone1 g m
selectMapToObj "monitors/exp.obj" g m = mapChildrenToMatNone1 g m
--
selectMapToObj _ g m = mapChildrenToMatMat g m

foreign import mapChildrenToMatMat :: TJS.OBJ -> TJS.MTL -> Effect Unit
foreign import mapChildrenToMatNone0 :: TJS.OBJ -> TJS.MTL -> Effect Unit
foreign import mapChildrenToMatNone1 :: TJS.OBJ -> TJS.MTL -> Effect Unit
--

selectMatTrans :: String -> TJS.OBJ -> Effect Unit
-- 0
selectMatTrans "monitors/cubo.obj" g = matTransparency0 g
selectMatTrans "monitors/cubo-1.obj" g = matTransparency0 g
selectMatTrans "monitors/cubo-2.obj" g = matTransparency0 g
selectMatTrans "monitors/cubo-3.obj" g = matTransparency0 g
selectMatTrans "monitors/cubo-4.obj" g = matTransparency0 g
selectMatTrans "monitors/ico.obj" g = matTransparency0 g
selectMatTrans "monitors/globe.obj" g = matTransparency0 g
-- 1
selectMatTrans "monitors/ico2.obj" g = matTransparency1 g
selectMatTrans "monitors/exp.obj" g = matTransparency1 g
--
selectMatTrans _ g = matTransparency0 g

foreign import matTransparency0 :: TJS.OBJ -> Effect Unit
foreign import matTransparency1 :: TJS.OBJ -> Effect Unit
--

selectMatOpacity :: String -> TJS.OBJ -> Number -> Effect Unit
-- 0
selectMatOpacity "monitors/cubo.obj" g n = matOpacity0 g n
selectMatOpacity "monitors/cubo-1.obj" g n = matOpacity0 g n
selectMatOpacity "monitors/cubo-2.obj" g n = matOpacity0 g n
selectMatOpacity "monitors/cubo-3.obj" g n = matOpacity0 g n
selectMatOpacity "monitors/cubo-4.obj" g n = matOpacity0 g n
selectMatOpacity "monitors/ico.obj" g n = matOpacity0 g n
selectMatOpacity "monitors/globe.obj" g n = matOpacity0 g n
-- 1
selectMatOpacity "monitors/ico2.obj" g n = matOpacity1 g n
selectMatOpacity "monitors/exp.obj" g n = matOpacity1 g n
--
selectMatOpacity _ g n = matOpacity0 g n

foreign import matOpacity0 :: TJS.OBJ -> Number -> Effect Unit
foreign import matOpacity1 :: TJS.OBJ -> Number -> Effect Unit
--

selectMatColor :: String -> TJS.OBJ -> Number -> Number -> Number -> Effect Unit
-- 0
selectMatColor "monitors/cubo.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
selectMatColor "monitors/cubo-1.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
selectMatColor "monitors/cubo-2.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
selectMatColor "monitors/cubo-3.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
selectMatColor "monitors/cubo-4.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
selectMatColor "monitors/ico.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
selectMatColor "monitors/globe.obj" g n1 n2 n3 = matColor0 g n1 n2 n3
-- 1
selectMatColor "monitors/ico2.obj" g n1 n2 n3 = matColor1 g n1 n2 n3
selectMatColor "monitors/exp.obj" g n1 n2 n3 = matColor1 g n1 n2 n3
--
selectMatColor _ g n1 n2 n3 = matColor0 g n1 n2 n3

foreign import matColor0 :: TJS.OBJ -> Number -> Number -> Number -> Effect Unit
foreign import matColor1 :: TJS.OBJ -> Number -> Number -> Number -> Effect Unit
--

selectMatEmiInten :: String -> TJS.OBJ -> Number -> Effect Unit
-- 0
selectMatEmiInten "monitors/cubo.obj" g n = matEmisInt0 g n
selectMatEmiInten "monitors/cubo-1.obj" g n = matEmisInt0 g n
selectMatEmiInten "monitors/cubo-2.obj" g n = matEmisInt0 g n
selectMatEmiInten "monitors/cubo-3.obj" g n = matEmisInt0 g n
selectMatEmiInten "monitors/cubo-4.obj" g n = matEmisInt0 g n
selectMatEmiInten "monitors/ico.obj" g n = matEmisInt0 g n
selectMatEmiInten "monitors/globe.obj" g n = matEmisInt0 g n
-- 1
selectMatEmiInten "monitors/ico2.obj" g n = matEmisInt1 g n
selectMatEmiInten "monitors/exp.obj" g n = matEmisInt1 g n
--
selectMatEmiInten _ g n = matEmisInt0 g n

foreign import matEmisInt0 :: TJS.OBJ -> Number -> Effect Unit
foreign import matEmisInt1 :: TJS.OBJ -> Number -> Effect Unit
--

selectMatEmiColor :: String -> TJS.OBJ -> Number -> Number -> Number -> Effect Unit
-- 0
selectMatEmiColor "monitors/cubo.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
selectMatEmiColor "monitors/cubo-1.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
selectMatEmiColor "monitors/cubo-2.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
selectMatEmiColor "monitors/cubo-3.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
selectMatEmiColor "monitors/cubo-4.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
selectMatEmiColor "monitors/ico.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
selectMatEmiColor "monitors/globe.obj" g n1 n2 n3 = matEmisive0 g n1 n2 n3
-- 1
selectMatEmiColor "monitors/ico2.obj" g n1 n2 n3 = matEmisive1 g n1 n2 n3
selectMatEmiColor "monitors/exp.obj" g n1 n2 n3 = matEmisive1 g n1 n2 n3
--
selectMatEmiColor _ g n1 n2 n3 = matEmisive0 g n1 n2 n3

foreign import matEmisive0 :: TJS.OBJ -> Number -> Number -> Number -> Effect Unit
foreign import matEmisive1 :: TJS.OBJ -> Number -> Number -> Number -> Effect Unit
---

transformTransmission :: TJS.Scene -> Monitor -> Transmission -> Effect Unit
transformTransmission sc mo t = do
  g <- read mo.obj -- call it: obj
  case g of
    Nothing -> pure unit
    Just o -> transformTransmission' o t

transformTransmission' :: TJS.OBJ -> Transmission -> Effect Unit
transformTransmission' g t = do
  TJS.setScaleOfAnything g (v3ToX t.size) (v3ToY t.size) (v3ToZ t.size)
  TJS.setPositionOfAnything g (v3ToX t.position) (v3ToY t.position) (v3ToZ t.position)
  TJS.setRotationOfAnything g (v3ToX t.rotation) (v3ToY t.rotation) (v3ToZ t.rotation)

-------- vidTexture --------

transformVidTexture :: TJS.TextureLoader -> Transmission -> Effect Unit
transformVidTexture vt t = do
  TJS.setRepeatOfAnything vt (v2ToX t.channelReapeater) (v2ToY t.channelReapeater)
  TJS.format vt (stringToFormatID t.fulcober)

stringToFormatID :: String -> TJS.FormatID
stringToFormatID "rgba" = TJS.rgbaFormat
stringToFormatID "alpha" = TJS.alphaFormat
stringToFormatID "red" = TJS.redFormat
stringToFormatID "redgreen" = TJS.rgFormat
stringToFormatID "luminance" = TJS.luminanceFormat
stringToFormatID "luminancealpha" = TJS.luminanceAlphaFormat
stringToFormatID _ = TJS.rgbaFormat


-------- vElem & currVidURL --------

playVideoElement :: Monitor -> Effect Unit
playVideoElement mo = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  HTML2.play v

updateURLfromVidElem :: Monitor -> String -> Effect Unit
updateURLfromVidElem mo url = do
  let v = mo.video -- :: HTML2.HTMLMediaElement
  currURL <- read mo.currVidURL -- :: String
  if url /= currURL
    then do
      HTML2.setSrc url v
      TJS.preloadAnything v
      HTML2.load v
      HTML2.setLoop true v
      HTML2.setMuted false v
      HTML2.setVolume 0.0 v
      write url mo.currVidURL -- write new info
    else (pure unit)

------------------------

----- General -----

v2ToX :: Vec2 -> Number
v2ToX v2 = v2.x

v2ToY :: Vec2 -> Number
v2ToY v2 = v2.y

v3ToX :: Vec3 -> Number
v3ToX v3 = v3.x

v3ToY :: Vec3 -> Number
v3ToY v3 = v3.y

v3ToZ :: Vec3 -> Number
v3ToZ v3 = v3.z
