(() => {
  // output/ThreeJS/foreign.js
  var newScene = () => new THREE.Scene();
  var newPerspectiveCamera = (fov) => (aspect) => (near) => (far) => () => new THREE.PerspectiveCamera(fov, aspect, near, far);
  var newWebGLRenderer = (params) => () => new THREE.WebGLRenderer(params);
  var render = (renderer) => (scene) => (camera) => () => renderer.render(scene, camera);
  var setSize = (renderer) => (w) => (h) => (updateStyle) => () => renderer.setSize(w, h, updateStyle);
  var domElement = (renderer) => () => document.body.appendChild(renderer.domElement);
  var newMesh = (geometry) => (material) => () => new THREE.Mesh(geometry, material);
  var addAnythingToScene = (scene) => (anything) => () => scene.add(anything);
  var setPositionOfAnything = (thing) => (x) => (y) => (z) => () => thing.position.set(x, y, z);
  var printAnything = (thing) => () => console.log(thing);
  var newHemisphereLight = (skyColor) => (groundColor) => (intensity) => () => new THREE.HemisphereLight(skyColor, groundColor, intensity);
  var requestAnimationFrame = (callback) => () => window.requestAnimationFrame(callback);
  var newBoxGeometry = (w) => (h) => (d) => () => new THREE.BoxGeometry(w, h, d);
  var meshBasicMaterial = (params) => () => new THREE.MeshBasicMaterial(params);
  var clampToEdgeWrapping = THREE.ClampToEdgeWrapping;
  var repeatWrapping = THREE.RepeatWrapping;
  var mirroredRepeatWrapping = THREE.MirroredRepeatWrapping;
  var nearestFilter = THREE.NearestFilter;
  var linearFilter = THREE.LinearFilter;

  // output/RenderEngine/index.js
  var animate = function(re) {
    return function __do2() {
      render(re.renderer)(re.scene)(re.camera)();
      return requestAnimationFrame(animate(re))();
    };
  };
  var launch = function __do() {
    var scene = newScene();
    var camera = newPerspectiveCamera(75)(16 / 9)(0.1)(100)();
    setPositionOfAnything(camera)(0)(0)(5)();
    var renderer = newWebGLRenderer({
      antialias: true
    })();
    setSize(renderer)(1250)(720)(false)();
    domElement(renderer)();
    var geometry = newBoxGeometry(1)(1)(1)();
    printAnything(geometry)();
    var material = meshBasicMaterial({
      color: 65280
    })();
    printAnything(material)();
    var cube = newMesh(geometry)(material)();
    addAnythingToScene(scene)(cube)();
    var lights = newHemisphereLight(16777147)(526368)(1)();
    addAnythingToScene(scene)(lights)();
    var re = {
      scene,
      camera,
      renderer
    };
    requestAnimationFrame(animate(re))();
    return re;
  };

  // output/Main/index.js
  var main = launch;

  // <stdin>
  main();
})();
