// output/Data.Show/foreign.js
var showNumberImpl = function(n) {
  var str = n.toString();
  return isNaN(str + ".0") ? str : str + ".0";
};

// output/Data.Show/index.js
var showNumber = {
  show: showNumberImpl
};
var show = function(dict) {
  return dict.show;
};

// output/Control.Semigroupoid/index.js
var semigroupoidFn = {
  compose: function(f) {
    return function(g) {
      return function(x) {
        return f(g(x));
      };
    };
  }
};

// output/Control.Category/index.js
var identity = function(dict) {
  return dict.identity;
};
var categoryFn = {
  identity: function(x) {
    return x;
  },
  Semigroupoid0: function() {
    return semigroupoidFn;
  }
};

// output/Effect/foreign.js
var pureE = function(a) {
  return function() {
    return a;
  };
};
var bindE = function(a) {
  return function(f) {
    return function() {
      return f(a())();
    };
  };
};

// output/Control.Apply/index.js
var apply = function(dict) {
  return dict.apply;
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};
var liftA1 = function(dictApplicative) {
  return function(f) {
    return function(a) {
      return apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
    };
  };
};

// output/Control.Bind/index.js
var bind = function(dict) {
  return dict.bind;
};

// output/Control.Monad/index.js
var ap = function(dictMonad) {
  return function(f) {
    return function(a) {
      return bind(dictMonad.Bind1())(f)(function(f$prime) {
        return bind(dictMonad.Bind1())(a)(function(a$prime) {
          return pure(dictMonad.Applicative0())(f$prime(a$prime));
        });
      });
    };
  };
};

// output/Effect/index.js
var $runtime_lazy = function(name, moduleName, init) {
  var state = 0;
  var val;
  return function(lineNumber) {
    if (state === 2)
      return val;
    if (state === 1)
      throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state = 1;
    val = init();
    state = 2;
    return val;
  };
};
var monadEffect = {
  Applicative0: function() {
    return applicativeEffect;
  },
  Bind1: function() {
    return bindEffect;
  }
};
var bindEffect = {
  bind: bindE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var applicativeEffect = {
  pure: pureE,
  Apply0: function() {
    return $lazy_applyEffect(0);
  }
};
var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
  return {
    map: liftA1(applicativeEffect)
  };
});
var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
  return {
    apply: ap(monadEffect),
    Functor0: function() {
      return $lazy_functorEffect(0);
    }
  };
});

// output/Effect.Class/index.js
var monadEffectEffect = {
  liftEffect: /* @__PURE__ */ identity(categoryFn),
  Monad0: function() {
    return monadEffect;
  }
};
var liftEffect = function(dict) {
  return dict.liftEffect;
};

// output/Effect.Console/foreign.js
var log = function(s) {
  return function() {
    console.log(s);
  };
};

// output/Effect.Class.Console/index.js
var log2 = function(dictMonadEffect) {
  var $33 = liftEffect(dictMonadEffect);
  return function($34) {
    return $33(log($34));
  };
};

// output/Effect.Ref/foreign.js
var _new = function(val) {
  return function() {
    return { value: val };
  };
};
var read = function(ref) {
  return function() {
    return ref.value;
  };
};

// output/Effect.Ref/index.js
var $$new = _new;

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
  return function __do() {
    var p = read(re.program)();
    log2(monadEffectEffect)(show(showNumber)(p))();
    render(re.renderer)(re.scene)(re.camera)();
    return requestAnimationFrame(animate(re))();
  };
};
var launch = function(cvs) {
  return function __do() {
    log2(monadEffectEffect)("launch now with ineffective program")();
    var scene = newScene();
    var camera = newPerspectiveCamera(75)(16 / 9)(0.1)(100)();
    setPositionOfAnything(camera)(0)(0)(5)();
    var renderer = newWebGLRenderer({
      antialias: true,
      canvas: cvs
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
    var program = $$new(0)();
    var re = {
      scene,
      camera,
      renderer,
      program
    };
    requestAnimationFrame(animate(re))();
    return re;
  };
};

// output/Main/index.js
var launch2 = launch;
export {
  launch2 as launch
};
