// output/Data.Functor/foreign.js
var arrayMap = function(f) {
  return function(arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i = 0; i < l; i++) {
      result[i] = f(arr[i]);
    }
    return result;
  };
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
var compose = function(dict) {
  return dict.compose;
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

// output/Data.Boolean/index.js
var otherwise = true;

// output/Data.Function/index.js
var flip = function(f) {
  return function(b) {
    return function(a) {
      return f(a)(b);
    };
  };
};
var $$const = function(a) {
  return function(v) {
    return a;
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Type.Proxy/index.js
var $$Proxy = /* @__PURE__ */ function() {
  function $$Proxy2() {
  }
  ;
  $$Proxy2.value = new $$Proxy2();
  return $$Proxy2;
}();

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var $$void = function(dictFunctor) {
  return map(dictFunctor)($$const(unit));
};
var voidLeft = function(dictFunctor) {
  return function(f) {
    return function(x) {
      return map(dictFunctor)($$const(x))(f);
    };
  };
};
var functorArray = {
  map: arrayMap
};

// output/Data.Semigroup/foreign.js
var concatArray = function(xs) {
  return function(ys) {
    if (xs.length === 0)
      return ys;
    if (ys.length === 0)
      return xs;
    return xs.concat(ys);
  };
};

// output/Data.Symbol/index.js
var reflectSymbol = function(dict) {
  return dict.reflectSymbol;
};

// output/Record.Unsafe/foreign.js
var unsafeGet = function(label) {
  return function(rec) {
    return rec[label];
  };
};

// output/Data.Semigroup/index.js
var semigroupArray = {
  append: concatArray
};
var append = function(dict) {
  return dict.append;
};

// output/Control.Alt/index.js
var alt = function(dict) {
  return dict.alt;
};

// output/Control.Apply/index.js
var apply = function(dict) {
  return dict.apply;
};
var applyFirst = function(dictApply) {
  return function(a) {
    return function(b) {
      return apply(dictApply)(map(dictApply.Functor0())($$const)(a))(b);
    };
  };
};
var applySecond = function(dictApply) {
  return function(a) {
    return function(b) {
      return apply(dictApply)(map(dictApply.Functor0())($$const(identity(categoryFn)))(a))(b);
    };
  };
};
var lift2 = function(dictApply) {
  return function(f) {
    return function(a) {
      return function(b) {
        return apply(dictApply)(map(dictApply.Functor0())(f)(a))(b);
      };
    };
  };
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

// output/Data.Bounded/foreign.js
var topInt = 2147483647;
var bottomInt = -2147483648;
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq2) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq2 : gt;
        };
      };
    };
  };
};
var ordIntImpl = unsafeCompareImpl;
var ordStringImpl = unsafeCompareImpl;
var ordCharImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqIntImpl = refEq;
var eqNumberImpl = refEq;
var eqCharImpl = refEq;
var eqStringImpl = refEq;

// output/Data.Eq/index.js
var eqString = {
  eq: eqStringImpl
};
var eqRowNil = {
  eqRecord: function(v) {
    return function(v1) {
      return function(v2) {
        return true;
      };
    };
  }
};
var eqRecord = function(dict) {
  return dict.eqRecord;
};
var eqRec = function() {
  return function(dictEqRecord) {
    return {
      eq: eqRecord(dictEqRecord)($$Proxy.value)
    };
  };
};
var eqNumber = {
  eq: eqNumberImpl
};
var eqInt = {
  eq: eqIntImpl
};
var eqChar = {
  eq: eqCharImpl
};
var eq = function(dict) {
  return dict.eq;
};
var eqRowCons = function(dictEqRecord) {
  return function() {
    return function(dictIsSymbol) {
      return function(dictEq) {
        return {
          eqRecord: function(v) {
            return function(ra) {
              return function(rb) {
                var tail2 = eqRecord(dictEqRecord)($$Proxy.value)(ra)(rb);
                var key = reflectSymbol(dictIsSymbol)($$Proxy.value);
                var get = unsafeGet(key);
                return eq(dictEq)(get(ra))(get(rb)) && tail2;
              };
            };
          }
        };
      };
    };
  };
};

// output/Data.Ordering/index.js
var LT = /* @__PURE__ */ function() {
  function LT2() {
  }
  ;
  LT2.value = new LT2();
  return LT2;
}();
var GT = /* @__PURE__ */ function() {
  function GT2() {
  }
  ;
  GT2.value = new GT2();
  return GT2;
}();
var EQ = /* @__PURE__ */ function() {
  function EQ2() {
  }
  ;
  EQ2.value = new EQ2();
  return EQ2;
}();

// output/Data.Ring/foreign.js
var intSub = function(x) {
  return function(y) {
    return x - y | 0;
  };
};

// output/Data.Semiring/foreign.js
var intAdd = function(x) {
  return function(y) {
    return x + y | 0;
  };
};
var intMul = function(x) {
  return function(y) {
    return x * y | 0;
  };
};
var numAdd = function(n1) {
  return function(n2) {
    return n1 + n2;
  };
};
var numMul = function(n1) {
  return function(n2) {
    return n1 * n2;
  };
};

// output/Data.Semiring/index.js
var zero = function(dict) {
  return dict.zero;
};
var semiringNumber = {
  add: numAdd,
  zero: 0,
  mul: numMul,
  one: 1
};
var semiringInt = {
  add: intAdd,
  zero: 0,
  mul: intMul,
  one: 1
};
var mul = function(dict) {
  return dict.mul;
};

// output/Data.Ring/index.js
var sub = function(dict) {
  return dict.sub;
};
var ringInt = {
  sub: intSub,
  Semiring0: function() {
    return semiringInt;
  }
};
var negate = function(dictRing) {
  return function(a) {
    return sub(dictRing)(zero(dictRing.Semiring0()))(a);
  };
};

// output/Data.Ord/index.js
var ordString = /* @__PURE__ */ function() {
  return {
    compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqString;
    }
  };
}();
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var ordChar = /* @__PURE__ */ function() {
  return {
    compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqChar;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};

// output/Data.Bounded/index.js
var top = function(dict) {
  return dict.top;
};
var boundedInt = {
  top: topInt,
  bottom: bottomInt,
  Ord0: function() {
    return ordInt;
  }
};
var boundedChar = {
  top: topChar,
  bottom: bottomChar,
  Ord0: function() {
    return ordChar;
  }
};
var bottom = function(dict) {
  return dict.bottom;
};

// output/Data.Show/foreign.js
var showIntImpl = function(n) {
  return n.toString();
};
var showCharImpl = function(c) {
  var code = c.charCodeAt(0);
  if (code < 32 || code === 127) {
    switch (c) {
      case "\x07":
        return "'\\a'";
      case "\b":
        return "'\\b'";
      case "\f":
        return "'\\f'";
      case "\n":
        return "'\\n'";
      case "\r":
        return "'\\r'";
      case "	":
        return "'\\t'";
      case "\v":
        return "'\\v'";
    }
    return "'\\" + code.toString(10) + "'";
  }
  return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
};
var showStringImpl = function(s) {
  var l = s.length;
  return '"' + s.replace(/[\0-\x1F\x7F"\\]/g, function(c, i) {
    switch (c) {
      case '"':
      case "\\":
        return "\\" + c;
      case "\x07":
        return "\\a";
      case "\b":
        return "\\b";
      case "\f":
        return "\\f";
      case "\n":
        return "\\n";
      case "\r":
        return "\\r";
      case "	":
        return "\\t";
      case "\v":
        return "\\v";
    }
    var k = i + 1;
    var empty2 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
    return "\\" + c.charCodeAt(0).toString(10) + empty2;
  }) + '"';
};
var showArrayImpl = function(f) {
  return function(xs) {
    var ss = [];
    for (var i = 0, l = xs.length; i < l; i++) {
      ss[i] = f(xs[i]);
    }
    return "[" + ss.join(",") + "]";
  };
};

// output/Data.Show/index.js
var showString = {
  show: showStringImpl
};
var showInt = {
  show: showIntImpl
};
var showChar = {
  show: showCharImpl
};
var show = function(dict) {
  return dict.show;
};
var showArray = function(dictShow) {
  return {
    show: showArrayImpl(show(dictShow))
  };
};

// output/Data.Maybe/index.js
var Nothing = /* @__PURE__ */ function() {
  function Nothing2() {
  }
  ;
  Nothing2.value = new Nothing2();
  return Nothing2;
}();
var Just = /* @__PURE__ */ function() {
  function Just2(value0) {
    this.value0 = value0;
  }
  ;
  Just2.create = function(value0) {
    return new Just2(value0);
  };
  return Just2;
}();
var maybe = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Nothing) {
        return v;
      }
      ;
      if (v2 instanceof Just) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
var functorMaybe = {
  map: function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }
      ;
      return Nothing.value;
    };
  }
};
var fromMaybe = function(a) {
  return maybe(a)(identity(categoryFn));
};
var fromJust = function() {
  return function(v) {
    if (v instanceof Just) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
  };
};
var applyMaybe = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return map(functorMaybe)(v.value0)(v1);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorMaybe;
  }
};
var bindMaybe = {
  bind: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Apply0: function() {
    return applyMaybe;
  }
};
var applicativeMaybe = /* @__PURE__ */ function() {
  return {
    pure: Just.create,
    Apply0: function() {
      return applyMaybe;
    }
  };
}();

// output/Data.Either/index.js
var Left = /* @__PURE__ */ function() {
  function Left2(value0) {
    this.value0 = value0;
  }
  ;
  Left2.create = function(value0) {
    return new Left2(value0);
  };
  return Left2;
}();
var Right = /* @__PURE__ */ function() {
  function Right2(value0) {
    this.value0 = value0;
  }
  ;
  Right2.create = function(value0) {
    return new Right2(value0);
  };
  return Right2;
}();
var functorEither = {
  map: function(f) {
    return function(m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }
      ;
      if (m instanceof Right) {
        return new Right(f(m.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
var either = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Left) {
        return v(v2.value0);
      }
      ;
      if (v2 instanceof Right) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var applyEither = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return map(functorEither)(v.value0)(v1);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorEither;
  }
};
var bindEither = {
  bind: /* @__PURE__ */ either(function(e) {
    return function(v) {
      return new Left(e);
    };
  })(function(a) {
    return function(f) {
      return f(a);
    };
  }),
  Apply0: function() {
    return applyEither;
  }
};
var applicativeEither = /* @__PURE__ */ function() {
  return {
    pure: Right.create,
    Apply0: function() {
      return applyEither;
    }
  };
}();

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i = len - 1; i >= 0; i--) {
        acc = f(xs[i])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i = 0; i < len; i++) {
        acc = f(acc)(xs[i]);
      }
      return acc;
    };
  };
};

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
};
var bind = function(dict) {
  return dict.bind;
};
var bindFlipped = function(dictBind) {
  return flip(bind(dictBind));
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
  }
};

// output/Control.Plus/index.js
var empty = function(dict) {
  return dict.empty;
};

// output/Control.Lazy/index.js
var $runtime_lazy = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var defer = function(dict) {
  return dict.defer;
};
var fix = function(dictLazy) {
  return function(f) {
    var $lazy_go = $runtime_lazy("go", "Control.Lazy", function() {
      return defer(dictLazy)(function(v) {
        return f($lazy_go(25));
      });
    });
    var go = $lazy_go(25);
    return go;
  };
};

// output/Data.HeytingAlgebra/foreign.js
var boolConj = function(b1) {
  return function(b2) {
    return b1 && b2;
  };
};
var boolDisj = function(b1) {
  return function(b2) {
    return b1 || b2;
  };
};
var boolNot = function(b) {
  return !b;
};

// output/Data.HeytingAlgebra/index.js
var not = function(dict) {
  return dict.not;
};
var ff = function(dict) {
  return dict.ff;
};
var disj = function(dict) {
  return dict.disj;
};
var heytingAlgebraBoolean = {
  ff: false,
  tt: true,
  implies: function(a) {
    return function(b) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
    };
  },
  conj: boolConj,
  disj: boolDisj,
  not: boolNot
};

// output/Data.EuclideanRing/foreign.js
var intDegree = function(x) {
  return Math.min(Math.abs(x), 2147483647);
};
var intDiv = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
  };
};
var intMod = function(x) {
  return function(y) {
    if (y === 0)
      return 0;
    var yy = Math.abs(y);
    return (x % yy + yy) % yy;
  };
};

// output/Data.CommutativeRing/index.js
var commutativeRingInt = {
  Ring0: function() {
    return ringInt;
  }
};

// output/Data.EuclideanRing/index.js
var mod = function(dict) {
  return dict.mod;
};
var euclideanRingInt = {
  degree: intDegree,
  div: intDiv,
  mod: intMod,
  CommutativeRing0: function() {
    return commutativeRingInt;
  }
};
var div = function(dict) {
  return dict.div;
};

// output/Data.Monoid/index.js
var mempty = function(dict) {
  return dict.mempty;
};

// output/Data.Tuple/index.js
var Tuple = /* @__PURE__ */ function() {
  function Tuple2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Tuple2.create = function(value0) {
    return function(value1) {
      return new Tuple2(value0, value1);
    };
  };
  return Tuple2;
}();
var snd = function(v) {
  return v.value1;
};
var fst = function(v) {
  return v.value0;
};

// output/Data.Bifunctor/index.js
var bimap = function(dict) {
  return dict.bimap;
};

// output/Data.Monoid.Disj/index.js
var Disj = function(x) {
  return x;
};
var semigroupDisj = function(dictHeytingAlgebra) {
  return {
    append: function(v) {
      return function(v1) {
        return disj(dictHeytingAlgebra)(v)(v1);
      };
    }
  };
};
var monoidDisj = function(dictHeytingAlgebra) {
  return {
    mempty: ff(dictHeytingAlgebra),
    Semigroup0: function() {
      return semigroupDisj(dictHeytingAlgebra);
    }
  };
};

// output/Unsafe.Coerce/foreign.js
var unsafeCoerce2 = function(x) {
  return x;
};

// output/Safe.Coerce/index.js
var coerce = function() {
  return unsafeCoerce2;
};

// output/Data.Newtype/index.js
var unwrap = coerce;
var alaF = function() {
  return function() {
    return function() {
      return function() {
        return function(v) {
          return coerce();
        };
      };
    };
  };
};

// output/Data.Foldable/index.js
var foldr = function(dict) {
  return dict.foldr;
};
var traverse_ = function(dictApplicative) {
  return function(dictFoldable) {
    return function(f) {
      return foldr(dictFoldable)(function() {
        var $316 = applySecond(dictApplicative.Apply0());
        return function($317) {
          return $316(f($317));
        };
      }())(pure(dictApplicative)(unit));
    };
  };
};
var foldl = function(dict) {
  return dict.foldl;
};
var foldMapDefaultR = function(dictFoldable) {
  return function(dictMonoid) {
    return function(f) {
      return foldr(dictFoldable)(function(x) {
        return function(acc) {
          return append(dictMonoid.Semigroup0())(f(x))(acc);
        };
      })(mempty(dictMonoid));
    };
  };
};
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: function(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};
var foldMap = function(dict) {
  return dict.foldMap;
};
var any = function(dictFoldable) {
  return function(dictHeytingAlgebra) {
    return alaF()()()()(Disj)(foldMap(dictFoldable)(monoidDisj(dictHeytingAlgebra)));
  };
};
var elem = function(dictFoldable) {
  return function(dictEq) {
    var $326 = any(dictFoldable)(heytingAlgebraBoolean);
    var $327 = eq(dictEq);
    return function($328) {
      return $326($327($328));
    };
  };
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

// output/Data.Identity/index.js
var Identity = function(x) {
  return x;
};
var functorIdentity = {
  map: function(f) {
    return function(m) {
      return f(m);
    };
  }
};
var applyIdentity = {
  apply: function(v) {
    return function(v1) {
      return v(v1);
    };
  },
  Functor0: function() {
    return functorIdentity;
  }
};
var bindIdentity = {
  bind: function(v) {
    return function(f) {
      return f(v);
    };
  },
  Apply0: function() {
    return applyIdentity;
  }
};
var applicativeIdentity = {
  pure: Identity,
  Apply0: function() {
    return applyIdentity;
  }
};
var monadIdentity = {
  Applicative0: function() {
    return applicativeIdentity;
  },
  Bind1: function() {
    return bindIdentity;
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

// output/Effect/index.js
var $runtime_lazy2 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
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
var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy2("functorEffect", "Effect", function() {
  return {
    map: liftA1(applicativeEffect)
  };
});
var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy2("applyEffect", "Effect", function() {
  return {
    apply: ap(monadEffect),
    Functor0: function() {
      return $lazy_functorEffect(0);
    }
  };
});

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
var write = function(val) {
  return function(ref) {
    return function() {
      ref.value = val;
    };
  };
};

// output/Effect.Ref/index.js
var $$new = _new;

// output/Control.Monad.Rec.Class/index.js
var Loop = /* @__PURE__ */ function() {
  function Loop2(value0) {
    this.value0 = value0;
  }
  ;
  Loop2.create = function(value0) {
    return new Loop2(value0);
  };
  return Loop2;
}();
var Done = /* @__PURE__ */ function() {
  function Done2(value0) {
    this.value0 = value0;
  }
  ;
  Done2.create = function(value0) {
    return new Done2(value0);
  };
  return Done2;
}();
var tailRecM = function(dict) {
  return dict.tailRecM;
};
var tailRec = function(f) {
  var go = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v instanceof Loop) {
        $copy_v = f(v.value0);
        return;
      }
      ;
      if (v instanceof Done) {
        $tco_done = true;
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 93, column 3 - line 93, column 25): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  return function($55) {
    return go(f($55));
  };
};
var monadRecIdentity = {
  tailRecM: function(f) {
    var runIdentity = function(v) {
      return v;
    };
    var $56 = tailRec(function($58) {
      return runIdentity(f($58));
    });
    return function($57) {
      return Identity($56($57));
    };
  },
  Monad0: function() {
    return monadIdentity;
  }
};
var bifunctorStep = {
  bimap: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Loop) {
          return new Loop(v(v2.value0));
        }
        ;
        if (v2 instanceof Done) {
          return new Done(v1(v2.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 29, column 1 - line 31, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};

// output/Data.Traversable/foreign.js
var traverseArrayImpl = function() {
  function array1(a) {
    return [a];
  }
  function array2(a) {
    return function(b) {
      return [a, b];
    };
  }
  function array3(a) {
    return function(b) {
      return function(c) {
        return [a, b, c];
      };
    };
  }
  function concat2(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply2) {
    return function(map2) {
      return function(pure2) {
        return function(f) {
          return function(array) {
            function go(bot, top2) {
              switch (top2 - bot) {
                case 0:
                  return pure2([]);
                case 1:
                  return map2(array1)(f(array[bot]));
                case 2:
                  return apply2(map2(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply2(apply2(map2(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                  return apply2(map2(concat2)(go(bot, pivot)))(go(pivot, top2));
              }
            }
            return go(0, array.length);
          };
        };
      };
    };
  };
}();

// output/Data.Traversable/index.js
var traverse = function(dict) {
  return dict.traverse;
};
var sequence = function(dict) {
  return dict.sequence;
};

// output/Data.Unfoldable/foreign.js
var unfoldrArrayImpl = function(isNothing2) {
  return function(fromJust2) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value = b;
            while (true) {
              var maybe2 = f(value);
              if (isNothing2(maybe2))
                return result;
              var tuple = fromJust2(maybe2);
              result.push(fst2(tuple));
              value = snd2(tuple);
            }
          };
        };
      };
    };
  };
};

// output/Data.Unfoldable1/foreign.js
var unfoldr1ArrayImpl = function(isNothing2) {
  return function(fromJust2) {
    return function(fst2) {
      return function(snd2) {
        return function(f) {
          return function(b) {
            var result = [];
            var value = b;
            while (true) {
              var tuple = f(value);
              result.push(fst2(tuple));
              var maybe2 = snd2(tuple);
              if (isNothing2(maybe2))
                return result;
              value = fromJust2(maybe2);
            }
          };
        };
      };
    };
  };
};

// output/Data.Unfoldable1/index.js
var unfoldable1Array = {
  unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd)
};

// output/Data.Unfoldable/index.js
var unfoldr = function(dict) {
  return dict.unfoldr;
};
var unfoldableArray = {
  unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(/* @__PURE__ */ fromJust())(fst)(snd),
  Unfoldable10: function() {
    return unfoldable1Array;
  }
};

// output/Data.NonEmpty/index.js
var NonEmpty = /* @__PURE__ */ function() {
  function NonEmpty2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  NonEmpty2.create = function(value0) {
    return function(value1) {
      return new NonEmpty2(value0, value1);
    };
  };
  return NonEmpty2;
}();

// output/Data.List.Types/index.js
var Nil = /* @__PURE__ */ function() {
  function Nil3() {
  }
  ;
  Nil3.value = new Nil3();
  return Nil3;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons3.create = function(value0) {
    return function(value1) {
      return new Cons3(value0, value1);
    };
  };
  return Cons3;
}();
var listMap = function(f) {
  var chunkedRevMap = function($copy_chunksAcc) {
    return function($copy_v) {
      var $tco_var_chunksAcc = $copy_chunksAcc;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(chunksAcc, v) {
        if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
          $tco_var_chunksAcc = new Cons(v, chunksAcc);
          $copy_v = v.value1.value1.value1;
          return;
        }
        ;
        var unrolledMap = function(v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
            return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
          }
          ;
          if (v1 instanceof Cons && v1.value1 instanceof Nil) {
            return new Cons(f(v1.value0), Nil.value);
          }
          ;
          return Nil.value;
        };
        var reverseUnrolledMap = function($copy_v1) {
          return function($copy_acc) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result2;
            function $tco_loop2(v1, acc) {
              if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                $tco_var_v1 = v1.value1;
                $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                return;
              }
              ;
              $tco_done1 = true;
              return acc;
            }
            ;
            while (!$tco_done1) {
              $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
            }
            ;
            return $tco_result2;
          };
        };
        $tco_done = true;
        return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  return chunkedRevMap(Nil.value);
};
var functorList = {
  map: listMap
};
var foldableList = {
  foldr: function(f) {
    return function(b) {
      var rev = function() {
        var go = function($copy_acc) {
          return function($copy_v) {
            var $tco_var_acc = $copy_acc;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(acc, v) {
              if (v instanceof Nil) {
                $tco_done = true;
                return acc;
              }
              ;
              if (v instanceof Cons) {
                $tco_var_acc = new Cons(v.value0, acc);
                $copy_v = v.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_acc, $copy_v);
            }
            ;
            return $tco_result;
          };
        };
        return go(Nil.value);
      }();
      var $205 = foldl(foldableList)(flip(f))(b);
      return function($206) {
        return $205(rev($206));
      };
    };
  },
  foldl: function(f) {
    var go = function($copy_b) {
      return function($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(b, v) {
          if (v instanceof Nil) {
            $tco_done1 = true;
            return b;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_b = f(b)(v.value0);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go;
  },
  foldMap: function(dictMonoid) {
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $207 = append(dictMonoid.Semigroup0())(acc);
        return function($208) {
          return $207(f($208));
        };
      })(mempty(dictMonoid));
    };
  }
};
var traversableList = {
  traverse: function(dictApplicative) {
    return function(f) {
      var $222 = map(dictApplicative.Apply0().Functor0())(foldl(foldableList)(flip(Cons.create))(Nil.value));
      var $223 = foldl(foldableList)(function(acc) {
        var $225 = lift2(dictApplicative.Apply0())(flip(Cons.create))(acc);
        return function($226) {
          return $225(f($226));
        };
      })(pure(dictApplicative)(Nil.value));
      return function($224) {
        return $222($223($224));
      };
    };
  },
  sequence: function(dictApplicative) {
    return traverse(traversableList)(dictApplicative)(identity(categoryFn));
  },
  Functor0: function() {
    return functorList;
  },
  Foldable1: function() {
    return foldableList;
  }
};

// output/Data.List/index.js
var updateAt = function(v) {
  return function(v1) {
    return function(v2) {
      if (v === 0 && v2 instanceof Cons) {
        return new Just(new Cons(v1, v2.value1));
      }
      ;
      if (v2 instanceof Cons) {
        return map(functorMaybe)(function(v3) {
          return new Cons(v2.value0, v3);
        })(updateAt(v - 1 | 0)(v1)(v2.value1));
      }
      ;
      return Nothing.value;
    };
  };
};
var uncons = function(v) {
  if (v instanceof Nil) {
    return Nothing.value;
  }
  ;
  if (v instanceof Cons) {
    return new Just({
      head: v.value0,
      tail: v.value1
    });
  }
  ;
  throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
};
var toUnfoldable = function(dictUnfoldable) {
  return unfoldr(dictUnfoldable)(function(xs) {
    return map(functorMaybe)(function(rec) {
      return new Tuple(rec.head, rec.tail);
    })(uncons(xs));
  });
};
var snoc = function(xs) {
  return function(x) {
    return foldr(foldableList)(Cons.create)(new Cons(x, Nil.value))(xs);
  };
};
var singleton3 = function(a) {
  return new Cons(a, Nil.value);
};
var reverse = /* @__PURE__ */ function() {
  var go = function($copy_acc) {
    return function($copy_v) {
      var $tco_var_acc = $copy_acc;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(acc, v) {
        if (v instanceof Nil) {
          $tco_done = true;
          return acc;
        }
        ;
        if (v instanceof Cons) {
          $tco_var_acc = new Cons(v.value0, acc);
          $copy_v = v.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_acc, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  return go(Nil.value);
}();
var take = /* @__PURE__ */ function() {
  var go = function($copy_acc) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_acc = $copy_acc;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v, v1) {
          if (v < 1) {
            $tco_done = true;
            return reverse(acc);
          }
          ;
          if (v1 instanceof Nil) {
            $tco_done = true;
            return reverse(acc);
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_acc = new Cons(v1.value0, acc);
            $tco_var_v = v - 1 | 0;
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 513, column 3 - line 513, column 35): " + [acc.constructor.name, v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  return go(Nil.value);
}();
var zipWith = function(f) {
  return function(xs) {
    return function(ys) {
      var go = function($copy_v) {
        return function($copy_v1) {
          return function($copy_acc) {
            var $tco_var_v = $copy_v;
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1, acc) {
              if (v instanceof Nil) {
                $tco_done = true;
                return acc;
              }
              ;
              if (v1 instanceof Nil) {
                $tco_done = true;
                return acc;
              }
              ;
              if (v instanceof Cons && v1 instanceof Cons) {
                $tco_var_v = v.value1;
                $tco_var_v1 = v1.value1;
                $copy_acc = new Cons(f(v.value0)(v1.value0), acc);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List (line 779, column 3 - line 779, column 21): " + [v.constructor.name, v1.constructor.name, acc.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_acc);
            }
            ;
            return $tco_result;
          };
        };
      };
      return reverse(go(xs)(ys)(Nil.value));
    };
  };
};
var zipWithA = function(dictApplicative) {
  return function(f) {
    return function(xs) {
      return function(ys) {
        return sequence(traversableList)(dictApplicative)(zipWith(f)(xs)(ys));
      };
    };
  };
};
var range2 = function(start) {
  return function(end) {
    if (start === end) {
      return singleton3(start);
    }
    ;
    if (otherwise) {
      var go = function($copy_s) {
        return function($copy_e) {
          return function($copy_step) {
            return function($copy_rest) {
              var $tco_var_s = $copy_s;
              var $tco_var_e = $copy_e;
              var $tco_var_step = $copy_step;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(s, e, step2, rest) {
                if (s === e) {
                  $tco_done = true;
                  return new Cons(s, rest);
                }
                ;
                if (otherwise) {
                  $tco_var_s = s + step2 | 0;
                  $tco_var_e = e;
                  $tco_var_step = step2;
                  $copy_rest = new Cons(s, rest);
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List (line 148, column 3 - line 149, column 65): " + [s.constructor.name, e.constructor.name, step2.constructor.name, rest.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_s, $tco_var_e, $tco_var_step, $copy_rest);
              }
              ;
              return $tco_result;
            };
          };
        };
      };
      return go(end)(start)(function() {
        var $223 = start > end;
        if ($223) {
          return 1;
        }
        ;
        return -1 | 0;
      }())(Nil.value);
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 144, column 1 - line 144, column 32): " + [start.constructor.name, end.constructor.name]);
  };
};
var manyRec = function(dictMonadRec) {
  return function(dictAlternative) {
    return function(p) {
      var go = function(acc) {
        return bind(dictMonadRec.Monad0().Bind1())(alt(dictAlternative.Plus1().Alt0())(map(dictAlternative.Plus1().Alt0().Functor0())(Loop.create)(p))(pure(dictAlternative.Applicative0())(new Done(unit))))(function(aa) {
          return pure(dictAlternative.Applicative0())(bimap(bifunctorStep)(function(v) {
            return new Cons(v, acc);
          })(function(v) {
            return reverse(acc);
          })(aa));
        });
      };
      return tailRecM(dictMonadRec)(go)(Nil.value);
    };
  };
};
var some = function(dictAlternative) {
  return function(dictLazy) {
    return function(v) {
      return apply(dictAlternative.Applicative0().Apply0())(map(dictAlternative.Plus1().Alt0().Functor0())(Cons.create)(v))(defer(dictLazy)(function(v1) {
        return many(dictAlternative)(dictLazy)(v);
      }));
    };
  };
};
var many = function(dictAlternative) {
  return function(dictLazy) {
    return function(v) {
      return alt(dictAlternative.Plus1().Alt0())(some(dictAlternative)(dictLazy)(v))(pure(dictAlternative.Applicative0())(Nil.value));
    };
  };
};
var length = /* @__PURE__ */ foldl(foldableList)(function(acc) {
  return function(v) {
    return acc + 1 | 0;
  };
})(0);
var fromFoldable = function(dictFoldable) {
  return foldr(dictFoldable)(Cons.create)(Nil.value);
};
var drop = function($copy_v) {
  return function($copy_v1) {
    var $tco_var_v = $copy_v;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v, v1) {
      if (v < 1) {
        $tco_done = true;
        return v1;
      }
      ;
      if (v1 instanceof Nil) {
        $tco_done = true;
        return Nil.value;
      }
      ;
      if (v1 instanceof Cons) {
        $tco_var_v = v - 1 | 0;
        $copy_v1 = v1.value1;
        return;
      }
      ;
      throw new Error("Failed pattern match at Data.List (line 536, column 1 - line 536, column 42): " + [v.constructor.name, v1.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_v, $copy_v1);
    }
    ;
    return $tco_result;
  };
};

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

// output/MonitorState/foreign.js
var crossOrigin = (v) => () => v.crossOrigin = "Anonymous";
var preloadMaterials = (m) => () => m.preload();
var mapVidToMatNone = (m) => (vt) => () => m.materials.None.map = vt;
var mapChildrenToMatNone = (o) => (m) => () => o.children[0].material = m.materials.None;
var matTransparency = (o) => () => o.children[0].material.transparent = true;
var matOpacity = (o) => (n) => () => o.children[0].material.opacity = n;
var matColor = (o) => (n1) => (n2) => (n3) => () => o.children[0].material.color = { r: n1, g: n2, b: n3 };
var matEmisColour = (o) => (n1) => (n2) => (n3) => () => o.children[0].material.emissive = { r: n1, g: n2, b: n3 };
var matEmisInt = (o) => (n) => () => o.children[0].material.emissiveIntensity = n;
var dynRotX = (o) => (v) => () => o.rotation.x += v;
var dynRotY = (o) => (v) => () => o.rotation.y += v;
var dynRotZ = (o) => (v) => () => o.rotation.z += v;

// output/ThreeJS/foreign.js
var newScene = () => new THREE.Scene();
var newPerspectiveCamera = (fov) => (aspect) => (near) => (far) => () => new THREE.PerspectiveCamera(fov, aspect, near, far);
var newWebGLRenderer = (params) => () => new THREE.WebGLRenderer(params);
var render = (renderer) => (scene) => (camera) => () => renderer.render(scene, camera);
var setSize = (renderer) => (w) => (h) => (updateStyle) => () => renderer.setSize(w, h, updateStyle);
var newOBJLoader = () => new THREE.OBJLoader();
var loadOBJ = (loader) => (url) => (cb) => () => loader.load(url, (x) => cb(x)());
var newMTLLoader = () => new THREE.MTLLoader();
var loadMTL = (loader) => (url) => (cb) => () => loader.load(url, (x) => cb(x)());
var addAnythingToScene = (scene) => (anything) => () => scene.add(anything);
var disposeAnything = (anything) => (anything2) => () => anything2.dispose();
var removeObject3D = (parent) => (child) => () => parent.remove(child);
var rotationX = (thing) => () => thing.rotation.x;
var rotationY = (thing) => () => thing.rotation.y;
var rotationZ = (thing) => () => thing.rotation.z;
var setRotationOfAnything = (thing) => (x) => (y) => (z) => () => thing.rotation.set(x, y, z);
var setPositionOfAnything = (thing) => (x) => (y) => (z) => () => thing.position.set(x, y, z);
var setScaleOfAnything = (thing) => (x) => (y) => (z) => () => thing.scale.set(x, y, z);
var setRepeatOfAnything = (thing) => (u) => (v) => () => thing.repeat.set(u, v);
var preloadAnything = (elem3) => () => elem3.preload = "auto";
var printAnything = (thing) => () => console.log(thing);
var newHemisphereLight = (skyColor) => (groundColor) => (intensity) => () => new THREE.HemisphereLight(skyColor, groundColor, intensity);
var windowInnerWidth = () => window.innerWidth;
var windowInnerHeight = () => window.innerHeight;
var createElement = (name2) => () => document.createElement(name2);
var videoTexture = (videoElem) => () => new THREE.VideoTexture(videoElem);
var clampToEdgeWrapping = THREE.ClampToEdgeWrapping;
var repeatWrapping = THREE.RepeatWrapping;
var mirroredRepeatWrapping = THREE.MirroredRepeatWrapping;
var nearestFilter = THREE.NearestFilter;
var linearFilter = THREE.LinearFilter;
var alphaFormat = THREE.AlphaFormat;
var redFormat = THREE.RedFormat;
var redIntegerFormat = THREE.RedIntegerFormat;
var rgFormat = THREE.RGFormat;
var rgIntegerFormat = THREE.RGIntegerFormat;
var rgbaFormat = THREE.RGBAFormat;
var rgbaIntegerFormat = THREE.RGBAIntegerFormat;
var luminanceFormat = THREE.LuminanceFormat;
var luminanceAlphaFormat = THREE.LuminanceAlphaFormat;
var depthFormat = THREE.DepthFormat;
var depthStencilFormat = THREE.DepthStencilFormat;
var format = (texture) => (formatID) => () => texture.format = formatID;

// output/ThreeJS.Unsafe/foreign.js
var setPosition = (thing) => (x) => (y) => (z) => () => thing.position.set(x, y, z);

// output/ThreeJS/index.js
var setRotationZ = function(o) {
  return function(z) {
    return function __do3() {
      var x = rotationX(o)();
      var y = rotationY(o)();
      return setRotationOfAnything(o)(x)(y)(z)();
    };
  };
};
var setRotationY = function(o) {
  return function(y) {
    return function __do3() {
      var x = rotationX(o)();
      var z = rotationZ(o)();
      return setRotationOfAnything(o)(x)(y)(z)();
    };
  };
};
var setRotationX = function(o) {
  return function(x) {
    return function __do3() {
      var y = rotationY(o)();
      var z = rotationZ(o)();
      return setRotationOfAnything(o)(x)(y)(z)();
    };
  };
};
var setPosition2 = function() {
  return setPosition;
};

// output/Web.HTML.HTMLMediaElement/foreign.js
function setSrc(src2) {
  return function(media) {
    return function() {
      media.src = src2;
    };
  };
}
function load(media) {
  return function() {
    return media.load();
  };
}
function setCurrentTime(currentTime2) {
  return function(media) {
    return function() {
      media.currentTime = currentTime2;
    };
  };
}
function setLoop(loop2) {
  return function(media) {
    return function() {
      media.loop = loop2;
    };
  };
}
function play(media) {
  return function() {
    media.play();
  };
}
function pause(media) {
  return function() {
    media.pause();
  };
}
function setVolume(volume2) {
  return function(media) {
    return function() {
      media.volume = volume2;
    };
  };
}
function setMuted(muted2) {
  return function(media) {
    return function() {
      media.muted = muted2;
    };
  };
}

// output/Data.Enum/foreign.js
function toCharCode(c) {
  return c.charCodeAt(0);
}
function fromCharCode(c) {
  return String.fromCharCode(c);
}

// output/Data.Enum/index.js
var toEnum = function(dict) {
  return dict.toEnum;
};
var fromEnum = function(dict) {
  return dict.fromEnum;
};
var toEnumWithDefaults = function(dictBoundedEnum) {
  return function(low) {
    return function(high) {
      return function(x) {
        var v = toEnum(dictBoundedEnum)(x);
        if (v instanceof Just) {
          return v.value0;
        }
        ;
        if (v instanceof Nothing) {
          var $54 = x < fromEnum(dictBoundedEnum)(bottom(dictBoundedEnum.Bounded0()));
          if ($54) {
            return low;
          }
          ;
          return high;
        }
        ;
        throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
      };
    };
  };
};
var defaultSucc = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) + 1 | 0);
    };
  };
};
var defaultPred = function(toEnum$prime) {
  return function(fromEnum$prime) {
    return function(a) {
      return toEnum$prime(fromEnum$prime(a) - 1 | 0);
    };
  };
};
var charToEnum = function(v) {
  if (v >= bottom(boundedInt) && v <= top(boundedInt)) {
    return new Just(fromCharCode(v));
  }
  ;
  return Nothing.value;
};
var enumChar = {
  succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
  pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
  Ord0: function() {
    return ordChar;
  }
};
var boundedEnumChar = /* @__PURE__ */ function() {
  return {
    cardinality: toCharCode(top(boundedChar)) - toCharCode(bottom(boundedChar)) | 0,
    toEnum: charToEnum,
    fromEnum: toCharCode,
    Bounded0: function() {
      return boundedChar;
    },
    Enum1: function() {
      return enumChar;
    }
  };
}();

// output/MonitorState/index.js
var v3ToZ = function(v3) {
  return v3.z;
};
var v3ToY = function(v3) {
  return v3.y;
};
var v3ToX = function(v3) {
  return v3.x;
};
var v2ToY = function(v2) {
  return v2.y;
};
var v2ToX = function(v2) {
  return v2.x;
};
var updateVol = function(mo) {
  return function(newVol) {
    return function __do3() {
      var currVol = read(mo.vol)();
      var $4 = newVol !== currVol;
      if ($4) {
        setVolume(newVol)(mo.video)();
        return write(newVol)(mo.vol)();
      }
      ;
      return unit;
    };
  };
};
var updateURLfromVidElem = function(mo) {
  return function(url) {
    return function __do3() {
      var currVol = read(mo.vol)();
      var currURL = read(mo.currVidURL)();
      var $5 = url !== currURL;
      if ($5) {
        setSrc(url)(mo.video)();
        preloadAnything(mo.video)();
        load(mo.video)();
        setLoop(true)(mo.video)();
        setMuted(false)(mo.video)();
        setVolume(currVol)(mo.video)();
        return write(url)(mo.currVidURL)();
      }
      ;
      return unit;
    };
  };
};
var stringToFormatID = function(v) {
  if (v === "rgb") {
    return rgbaFormat;
  }
  ;
  if (v === "rgba") {
    return rgbaFormat;
  }
  ;
  if (v === "erregebe") {
    return rgbaFormat;
  }
  ;
  if (v === "alcolor") {
    return rgbaFormat;
  }
  ;
  if (v === "a") {
    return alphaFormat;
  }
  ;
  if (v === "alpha") {
    return alphaFormat;
  }
  ;
  if (v === "alfa") {
    return alphaFormat;
  }
  ;
  if (v === "r") {
    return redFormat;
  }
  ;
  if (v === "red") {
    return redFormat;
  }
  ;
  if (v === "rojo") {
    return redFormat;
  }
  ;
  if (v === "rg") {
    return rgFormat;
  }
  ;
  if (v === "errege") {
    return rgFormat;
  }
  ;
  if (v === "redgreen") {
    return rgFormat;
  }
  ;
  if (v === "verdirojo") {
    return rgFormat;
  }
  ;
  if (v === "b&w") {
    return luminanceFormat;
  }
  ;
  if (v === "blackandwhite") {
    return luminanceFormat;
  }
  ;
  if (v === "negriblanco") {
    return luminanceFormat;
  }
  ;
  if (v === "luminancealpha") {
    return luminanceAlphaFormat;
  }
  ;
  return rgbaFormat;
};
var transformVidTexture = function(vt) {
  return function(t) {
    return function __do3() {
      setRepeatOfAnything(vt)(v2ToX(t.channelReapeater))(v2ToY(t.channelReapeater))();
      return format(vt)(stringToFormatID(t.fulcober))();
    };
  };
};
var stopVideoElement = function(mo) {
  return function __do3() {
    pause(mo.video)();
    return setCurrentTime(0)(mo.video)();
  };
};
var setRotationZ2 = function(o) {
  return function(v) {
    if (v instanceof Left) {
      return dynRotZ(o)(v.value0);
    }
    ;
    if (v instanceof Right) {
      return setRotationZ(o)(v.value0);
    }
    ;
    throw new Error("Failed pattern match at MonitorState (line 257, column 1 - line 257, column 63): " + [o.constructor.name, v.constructor.name]);
  };
};
var setRotationY2 = function(o) {
  return function(v) {
    if (v instanceof Left) {
      return dynRotY(o)(v.value0);
    }
    ;
    if (v instanceof Right) {
      return setRotationY(o)(v.value0);
    }
    ;
    throw new Error("Failed pattern match at MonitorState (line 253, column 1 - line 253, column 63): " + [o.constructor.name, v.constructor.name]);
  };
};
var setRotationX2 = function(o) {
  return function(v) {
    if (v instanceof Left) {
      return dynRotX(o)(v.value0);
    }
    ;
    if (v instanceof Right) {
      return setRotationX(o)(v.value0);
    }
    ;
    throw new Error("Failed pattern match at MonitorState (line 249, column 1 - line 249, column 63): " + [o.constructor.name, v.constructor.name]);
  };
};
var transformTransmission$prime = function(g) {
  return function(t) {
    return function __do3() {
      setScaleOfAnything(g)(t.size)(t.size)(t.size)();
      setPositionOfAnything(g)(v3ToX(t.position))(v3ToY(t.position))(v3ToZ(t.position))();
      setRotationX2(g)(t.rotation.x)();
      setRotationY2(g)(t.rotation.y)();
      return setRotationZ2(g)(t.rotation.z)();
    };
  };
};
var transformTransmission = function(sc) {
  return function(mo) {
    return function(t) {
      return function __do3() {
        var g = read(mo.obj)();
        if (g instanceof Nothing) {
          return unit;
        }
        ;
        if (g instanceof Just) {
          return transformTransmission$prime(g.value0)(t)();
        }
        ;
        throw new Error("Failed pattern match at MonitorState (line 236, column 3 - line 238, column 41): " + [g.constructor.name]);
      };
    };
  };
};
var removeObj = function(sc) {
  return function(mo) {
    return function __do3() {
      var g = read(mo.obj)();
      if (g instanceof Nothing) {
        return unit;
      }
      ;
      if (g instanceof Just) {
        disposeAnything(g.value0)();
        removeObject3D(sc)(g.value0)();
        return write(Nothing.value)(mo.obj)();
      }
      ;
      throw new Error("Failed pattern match at MonitorState (line 186, column 3 - line 191, column 27): " + [g.constructor.name]);
    };
  };
};
var removeMaterial = function(sc) {
  return function(mo) {
    return function __do3() {
      var m = read(mo.material)();
      if (m instanceof Nothing) {
        return unit;
      }
      ;
      if (m instanceof Just) {
        disposeAnything(m.value0)();
        removeObject3D(sc)(m.value0)();
        return write(Nothing.value)(mo.material)();
      }
      ;
      throw new Error("Failed pattern match at MonitorState (line 196, column 3 - line 201, column 32): " + [m.constructor.name]);
    };
  };
};
var removeMonitor = function(sc) {
  return function(mo) {
    return function __do3() {
      removeObj(sc)(mo)();
      removeMaterial(sc)(mo)();
      return stopVideoElement(mo)();
    };
  };
};
var playVideoElement = function(mo) {
  return play(mo.video);
};
var makeTransmission = function(url) {
  return function(sc) {
    return function(g) {
      return function(m) {
        return function(vt) {
          return function(t) {
            return function(rC) {
              return function(gC) {
                return function(bC) {
                  return function(rE) {
                    return function(gE) {
                      return function(bE) {
                        return function(iE) {
                          return function __do3() {
                            mapVidToMatNone(m)(vt)();
                            mapChildrenToMatNone(g)(m)();
                            matTransparency(g)();
                            matOpacity(g)(t)();
                            matColor(g)(rC)(gC)(bC)();
                            matEmisColour(g)(rE)(gE)(bE)();
                            matEmisInt(g)(iE)();
                            printAnything(m)();
                            return addAnythingToScene(sc)(g)();
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
var tryToMakeTransmission = function(sc) {
  return function(mo) {
    return function(t) {
      return function(rC) {
        return function(gC) {
          return function(bC) {
            return function(rE) {
              return function(gE) {
                return function(bE) {
                  return function(iE) {
                    return function __do3() {
                      var currURL = read(mo.currObjURL)();
                      var g = read(mo.obj)();
                      if (g instanceof Nothing) {
                        return unit;
                      }
                      ;
                      if (g instanceof Just) {
                        var m = read(mo.material)();
                        if (m instanceof Nothing) {
                          return unit;
                        }
                        ;
                        if (m instanceof Just) {
                          return makeTransmission(currURL)(sc)(g.value0)(m.value0)(mo.vidTexture)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                        }
                        ;
                        throw new Error("Failed pattern match at MonitorState (line 179, column 7 - line 181, column 90): " + [m.constructor.name]);
                      }
                      ;
                      throw new Error("Failed pattern match at MonitorState (line 175, column 3 - line 181, column 90): " + [g.constructor.name]);
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
var defVidTexture = function(v) {
  return function __do3() {
    var vidTexture = videoTexture(v)();
    return vidTexture;
  };
};
var defURL = "";
var defVidElem = function __do() {
  var v = createElement("video")();
  crossOrigin(v)();
  setSrc(defURL)(v)();
  return v;
};
var defMonitor = function __do2() {
  var currVidURL = $$new(defURL)();
  var video = defVidElem();
  var vidTexture = defVidTexture(video)();
  var vol = $$new(0)();
  var currObjURL = $$new(defURL)();
  var obj = $$new(Nothing.value)();
  var currMtlURL = $$new(defURL)();
  var material = $$new(Nothing.value)();
  var opacity = $$new(1)();
  var colour = $$new({
    x: 0,
    y: 0,
    z: 0
  })();
  var emissionColour = $$new({
    x: 0,
    y: 0,
    z: 0
  })();
  var emissionIntensity = $$new(1)();
  var mo = {
    currVidURL,
    video,
    vidTexture,
    vol,
    currObjURL,
    obj,
    currMtlURL,
    material,
    opacity,
    colour,
    emissionColour,
    emissionIntensity
  };
  return mo;
};
var changeOrLoadObjIfNecessary = function(sc) {
  return function(mo) {
    return function(url) {
      return function(t) {
        return function(rC) {
          return function(gC) {
            return function(bC) {
              return function(rE) {
                return function(gE) {
                  return function(bE) {
                    return function(iE) {
                      return function __do3() {
                        var currURL = read(mo.currObjURL)();
                        var $29 = url === currURL;
                        if ($29) {
                          return unit;
                        }
                        ;
                        removeObj(sc)(mo)();
                        var loader = newOBJLoader();
                        loadOBJ(loader)(url)(function(o) {
                          return function __do4() {
                            write(new Just(o))(mo.obj)();
                            return tryToMakeTransmission(sc)(mo)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                          };
                        })();
                        return write(url)(mo.currObjURL)();
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
var changeOrLoadMatIfNecessary = function(sc) {
  return function(mo) {
    return function(url) {
      return function(t) {
        return function(rC) {
          return function(gC) {
            return function(bC) {
              return function(rE) {
                return function(gE) {
                  return function(bE) {
                    return function(iE) {
                      return function __do3() {
                        var currURL = read(mo.currMtlURL)();
                        var $30 = url === currURL;
                        if ($30) {
                          return unit;
                        }
                        ;
                        var loader = newMTLLoader();
                        removeMaterial(sc)(mo)();
                        loadMTL(loader)(url)(function(m) {
                          return function __do4() {
                            preloadMaterials(m)();
                            write(new Just(m))(mo.material)();
                            return tryToMakeTransmission(sc)(mo)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                          };
                        })();
                        return write(url)(mo.currMtlURL)();
                      };
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
var changeMatParametersIfNecessary = function(sc) {
  return function(mo) {
    return function(t) {
      return function(rC) {
        return function(gC) {
          return function(bC) {
            return function(rE) {
              return function(gE) {
                return function(bE) {
                  return function(iE) {
                    return function __do3() {
                      var currOpacity = read(mo.opacity)();
                      var currColour = read(mo.colour)();
                      var currEmissionColour = read(mo.emissionColour)();
                      var currEmissionIntensity = read(mo.emissionIntensity)();
                      (function() {
                        var $31 = t === currOpacity;
                        if ($31) {
                          return unit;
                        }
                        ;
                        return tryToMakeTransmission(sc)(mo)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                      })();
                      (function() {
                        var $32 = eq(eqRec()(eqRowCons(eqRowCons(eqRowCons(eqRowNil)()({
                          reflectSymbol: function() {
                            return "z";
                          }
                        })(eqNumber))()({
                          reflectSymbol: function() {
                            return "y";
                          }
                        })(eqNumber))()({
                          reflectSymbol: function() {
                            return "x";
                          }
                        })(eqNumber)))({
                          x: rC,
                          y: gC,
                          z: bC
                        })(currColour);
                        if ($32) {
                          return unit;
                        }
                        ;
                        return tryToMakeTransmission(sc)(mo)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                      })();
                      (function() {
                        var $33 = eq(eqRec()(eqRowCons(eqRowCons(eqRowCons(eqRowNil)()({
                          reflectSymbol: function() {
                            return "z";
                          }
                        })(eqNumber))()({
                          reflectSymbol: function() {
                            return "y";
                          }
                        })(eqNumber))()({
                          reflectSymbol: function() {
                            return "x";
                          }
                        })(eqNumber)))({
                          x: rE,
                          y: gE,
                          z: bE
                        })(currEmissionColour);
                        if ($33) {
                          return unit;
                        }
                        ;
                        return tryToMakeTransmission(sc)(mo)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                      })();
                      (function() {
                        var $34 = iE === currEmissionIntensity;
                        if ($34) {
                          return unit;
                        }
                        ;
                        return tryToMakeTransmission(sc)(mo)(t)(rC)(gC)(bC)(rE)(gE)(bE)(iE)();
                      })();
                      write(t)(mo.opacity)();
                      write({
                        x: rC,
                        y: gC,
                        z: bC
                      })(mo.colour)();
                      write({
                        x: rE,
                        y: gE,
                        z: bE
                      })(mo.emissionColour)();
                      return write(iE)(mo.emissionIntensity)();
                    };
                  };
                };
              };
            };
          };
        };
      };
    };
  };
};
var alignMonitor = function(sc) {
  return function(mo) {
    return function(t) {
      return function __do3() {
        updateURLfromVidElem(mo)(t.channel)();
        updateVol(mo)(t.volume)();
        changeOrLoadObjIfNecessary(sc)(mo)(t.tv)(t.translucidez)(v3ToX(t.colour))(v3ToY(t.colour))(v3ToZ(t.colour))(v3ToX(t.emissionColour))(v3ToY(t.emissionColour))(v3ToZ(t.emissionColour))(t.emissionIntensity)();
        changeOrLoadMatIfNecessary(sc)(mo)(t.mapping)(t.translucidez)(v3ToX(t.colour))(v3ToY(t.colour))(v3ToZ(t.colour))(v3ToX(t.emissionColour))(v3ToY(t.emissionColour))(v3ToZ(t.emissionColour))(t.emissionIntensity)();
        changeMatParametersIfNecessary(sc)(mo)(t.translucidez)(v3ToX(t.colour))(v3ToY(t.colour))(v3ToZ(t.colour))(v3ToX(t.emissionColour))(v3ToY(t.emissionColour))(v3ToZ(t.emissionColour))(t.emissionIntensity)();
        transformTransmission(sc)(mo)(t)();
        return transformVidTexture(mo.vidTexture)(t)();
      };
    };
  };
};

// output/Transmission/index.js
var defTransmission = /* @__PURE__ */ function() {
  return {
    estado: false,
    tv: "https://www.transmits.link/monitors/oldi0.obj",
    mapping: "https://www.transmits.link/monitors/oldi0.mtl",
    volume: 0.03,
    channel: "https://www.transmits.link/channels/static.mp4",
    channelReapeater: {
      x: 1,
      y: 1
    },
    fulcober: "rgbaFormat",
    translucidez: 1,
    colour: {
      x: 0.6,
      y: 0.6,
      z: 0.6
    },
    emissionColour: {
      x: 0,
      y: 0,
      z: 0
    },
    emissionIntensity: 0.5,
    size: 1,
    position: {
      x: 0,
      y: 0,
      z: 0
    },
    rotation: {
      x: new Right(0.5),
      y: new Right(0),
      z: new Right(0)
    }
  };
}();
var defTransmissionOn = /* @__PURE__ */ function() {
  return {
    estado: true,
    tv: defTransmission.tv,
    mapping: defTransmission.mapping,
    channel: "https://jac307.github.io/MultimediaSamples/Video/main/1.mp4",
    volume: defTransmission.volume,
    channelReapeater: defTransmission.channelReapeater,
    fulcober: defTransmission.fulcober,
    translucidez: defTransmission.translucidez,
    colour: defTransmission.colour,
    emissionColour: defTransmission.emissionColour,
    emissionIntensity: defTransmission.emissionIntensity,
    size: defTransmission.size,
    position: defTransmission.position,
    rotation: defTransmission.rotation
  };
}();

// output/AST/index.js
var LiteralTransmissionAST = /* @__PURE__ */ function() {
  function LiteralTransmissionAST2(value0) {
    this.value0 = value0;
  }
  ;
  LiteralTransmissionAST2.create = function(value0) {
    return new LiteralTransmissionAST2(value0);
  };
  return LiteralTransmissionAST2;
}();
var Volume = /* @__PURE__ */ function() {
  function Volume2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Volume2.create = function(value0) {
    return function(value1) {
      return new Volume2(value0, value1);
    };
  };
  return Volume2;
}();
var ChannelRepeater = /* @__PURE__ */ function() {
  function ChannelRepeater2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ChannelRepeater2.create = function(value0) {
    return function(value1) {
      return new ChannelRepeater2(value0, value1);
    };
  };
  return ChannelRepeater2;
}();
var Scalar = /* @__PURE__ */ function() {
  function Scalar2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Scalar2.create = function(value0) {
    return function(value1) {
      return new Scalar2(value0, value1);
    };
  };
  return Scalar2;
}();
var Movet = /* @__PURE__ */ function() {
  function Movet2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Movet2.create = function(value0) {
    return function(value1) {
      return new Movet2(value0, value1);
    };
  };
  return Movet2;
}();
var Rodar = /* @__PURE__ */ function() {
  function Rodar2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Rodar2.create = function(value0) {
    return function(value1) {
      return new Rodar2(value0, value1);
    };
  };
  return Rodar2;
}();
var Fulcober = /* @__PURE__ */ function() {
  function Fulcober2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Fulcober2.create = function(value0) {
    return function(value1) {
      return new Fulcober2(value0, value1);
    };
  };
  return Fulcober2;
}();
var Switch = /* @__PURE__ */ function() {
  function Switch2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Switch2.create = function(value0) {
    return function(value1) {
      return new Switch2(value0, value1);
    };
  };
  return Switch2;
}();
var Monitor = /* @__PURE__ */ function() {
  function Monitor2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Monitor2.create = function(value0) {
    return function(value1) {
      return new Monitor2(value0, value1);
    };
  };
  return Monitor2;
}();
var Translucidez = /* @__PURE__ */ function() {
  function Translucidez2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Translucidez2.create = function(value0) {
    return function(value1) {
      return new Translucidez2(value0, value1);
    };
  };
  return Translucidez2;
}();
var Colour = /* @__PURE__ */ function() {
  function Colour2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Colour2.create = function(value0) {
    return function(value1) {
      return new Colour2(value0, value1);
    };
  };
  return Colour2;
}();
var EmissionColour = /* @__PURE__ */ function() {
  function EmissionColour2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  EmissionColour2.create = function(value0) {
    return function(value1) {
      return new EmissionColour2(value0, value1);
    };
  };
  return EmissionColour2;
}();
var EmissionIntensity = /* @__PURE__ */ function() {
  function EmissionIntensity2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  EmissionIntensity2.create = function(value0) {
    return function(value1) {
      return new EmissionIntensity2(value0, value1);
    };
  };
  return EmissionIntensity2;
}();
var EmptyStatement = /* @__PURE__ */ function() {
  function EmptyStatement2() {
  }
  ;
  EmptyStatement2.value = new EmptyStatement2();
  return EmptyStatement2;
}();
var TransmissionAST = /* @__PURE__ */ function() {
  function TransmissionAST2(value0) {
    this.value0 = value0;
  }
  ;
  TransmissionAST2.create = function(value0) {
    return new TransmissionAST2(value0);
  };
  return TransmissionAST2;
}();
var Broadcaster = /* @__PURE__ */ function() {
  function Broadcaster2(value0) {
    this.value0 = value0;
  }
  ;
  Broadcaster2.create = function(value0) {
    return new Broadcaster2(value0);
  };
  return Broadcaster2;
}();
var tASTtoTWithBase = function(base) {
  return function(v) {
    if (v instanceof LiteralTransmissionAST && !v.value0) {
      return defTransmission;
    }
    ;
    if (v instanceof LiteralTransmissionAST && v.value0) {
      return defTransmissionOn;
    }
    ;
    if (v instanceof Volume) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v.value0 * 0.01,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof ChannelRepeater) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v.value0,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Scalar) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v.value0,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Movet) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v.value0,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Rodar) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v.value0
      };
    }
    ;
    if (v instanceof Fulcober) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v.value0,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Switch) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: base + (v.value0 + ".mp4"),
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Monitor) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v.value0 + ".obj",
        mapping: v.value0 + ".mtl",
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Translucidez) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v.value0,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof Colour) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v.value0,
        emissionColour: v1.emissionColour,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof EmissionColour) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v.value0,
        emissionIntensity: v1.emissionIntensity,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    if (v instanceof EmissionIntensity) {
      var v1 = tASTtoTWithBase(base)(v.value1);
      return {
        estado: v1.estado,
        tv: v1.tv,
        mapping: v1.mapping,
        channel: v1.channel,
        volume: v1.volume,
        channelReapeater: v1.channelReapeater,
        fulcober: v1.fulcober,
        translucidez: v1.translucidez,
        colour: v1.colour,
        emissionColour: v1.emissionColour,
        emissionIntensity: v.value0,
        size: v1.size,
        position: v1.position,
        rotation: v1.rotation
      };
    }
    ;
    throw new Error("Failed pattern match at AST (line 53, column 1 - line 53, column 61): " + [base.constructor.name, v.constructor.name]);
  };
};

// output/Data.Int/foreign.js
var fromNumberImpl = function(just) {
  return function(nothing) {
    return function(n) {
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};
var toNumber = function(n) {
  return n;
};

// output/Data.Number/foreign.js
var isFiniteImpl = isFinite;
var floor = Math.floor;
var pow = function(n) {
  return function(p) {
    return Math.pow(n, p);
  };
};

// output/Data.Int/index.js
var fromNumber = /* @__PURE__ */ function() {
  return fromNumberImpl(Just.create)(Nothing.value);
}();
var unsafeClamp = function(x) {
  if (!isFiniteImpl(x)) {
    return 0;
  }
  ;
  if (x >= toNumber(top(boundedInt))) {
    return top(boundedInt);
  }
  ;
  if (x <= toNumber(bottom(boundedInt))) {
    return bottom(boundedInt);
  }
  ;
  if (otherwise) {
    return fromMaybe(0)(fromNumber(x));
  }
  ;
  throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
};
var floor2 = function($25) {
  return unsafeClamp(floor($25));
};

// output/Data.Number.Format/foreign.js
function wrap(method) {
  return function(d) {
    return function(num) {
      return method.apply(num, [d]);
    };
  };
}
var toPrecisionNative = wrap(Number.prototype.toPrecision);
var toFixedNative = wrap(Number.prototype.toFixed);
var toExponentialNative = wrap(Number.prototype.toExponential);
function toString(num) {
  return num.toString();
}

// output/Data.String.Common/foreign.js
var toLower = function(s) {
  return s.toLowerCase();
};

// output/Data.String.Common/index.js
var $$null = function(s) {
  return s === "";
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};

// output/Data.Lazy/foreign.js
var defer2 = function(thunk) {
  var v = null;
  return function() {
    if (thunk === void 0)
      return v;
    v = thunk();
    thunk = void 0;
    return v;
  };
};
var force = function(l) {
  return l();
};

// output/Parsing/index.js
var $runtime_lazy3 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var ParseState = /* @__PURE__ */ function() {
  function ParseState2(value0, value1, value2) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value2;
  }
  ;
  ParseState2.create = function(value0) {
    return function(value1) {
      return function(value2) {
        return new ParseState2(value0, value1, value2);
      };
    };
  };
  return ParseState2;
}();
var ParseError = /* @__PURE__ */ function() {
  function ParseError2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ParseError2.create = function(value0) {
    return function(value1) {
      return new ParseError2(value0, value1);
    };
  };
  return ParseError2;
}();
var More = /* @__PURE__ */ function() {
  function More2(value0) {
    this.value0 = value0;
  }
  ;
  More2.create = function(value0) {
    return new More2(value0);
  };
  return More2;
}();
var Lift = /* @__PURE__ */ function() {
  function Lift2(value0) {
    this.value0 = value0;
  }
  ;
  Lift2.create = function(value0) {
    return new Lift2(value0);
  };
  return Lift2;
}();
var Stop = /* @__PURE__ */ function() {
  function Stop2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Stop2.create = function(value0) {
    return function(value1) {
      return new Stop2(value0, value1);
    };
  };
  return Stop2;
}();
var lazyParserT = {
  defer: function(f) {
    var m = defer2(f);
    return function(state1, more, lift3, $$throw, done) {
      var v = force(m);
      return v(state1, more, lift3, $$throw, done);
    };
  }
};
var functorParserT = {
  map: function(f) {
    return function(v) {
      return function(state1, more, lift3, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift3, $$throw, function(state2, a) {
            return more(function(v2) {
              return done(state2, f(a));
            });
          });
        });
      };
    };
  }
};
var applyParserT = {
  apply: function(v) {
    return function(v1) {
      return function(state1, more, lift3, $$throw, done) {
        return more(function(v2) {
          return v(state1, more, lift3, $$throw, function(state2, f) {
            return more(function(v3) {
              return v1(state2, more, lift3, $$throw, function(state3, a) {
                return more(function(v4) {
                  return done(state3, f(a));
                });
              });
            });
          });
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var bindParserT = {
  bind: function(v) {
    return function(next) {
      return function(state1, more, lift3, $$throw, done) {
        return more(function(v1) {
          return v(state1, more, lift3, $$throw, function(state2, a) {
            return more(function(v2) {
              var v3 = next(a);
              return v3(state2, more, lift3, $$throw, done);
            });
          });
        });
      };
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var applicativeParserT = {
  pure: function(a) {
    return function(state1, v, v1, v2, done) {
      return done(state1, a);
    };
  },
  Apply0: function() {
    return applyParserT;
  }
};
var monadParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Bind1: function() {
    return bindParserT;
  }
};
var monadRecParserT = {
  tailRecM: function(next) {
    return function(initArg) {
      return function(state1, more, lift3, $$throw, done) {
        var $lazy_loop = $runtime_lazy3("loop", "Parsing", function() {
          return function(state2, arg, gas) {
            var v = next(arg);
            return v(state2, more, lift3, $$throw, function(state3, step2) {
              if (step2 instanceof Loop) {
                var $120 = gas === 0;
                if ($120) {
                  return more(function(v1) {
                    return $lazy_loop(269)(state3, step2.value0, 30);
                  });
                }
                ;
                return $lazy_loop(271)(state3, step2.value0, gas - 1 | 0);
              }
              ;
              if (step2 instanceof Done) {
                return done(state3, step2.value0);
              }
              ;
              throw new Error("Failed pattern match at Parsing (line 265, column 39 - line 273, column 43): " + [step2.constructor.name]);
            });
          };
        });
        var loop2 = $lazy_loop(262);
        return loop2(state1, initArg, 30);
      };
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var monadThrowParseErrorParse = {
  throwError: function(err) {
    return function(state1, v, v1, $$throw, v2) {
      return $$throw(state1, err);
    };
  },
  Monad0: function() {
    return monadParserT;
  }
};
var altParserT = {
  alt: function(v) {
    return function(v1) {
      return function(v2, more, lift3, $$throw, done) {
        return more(function(v3) {
          return v(new ParseState(v2.value0, v2.value1, false), more, lift3, function(v4, err) {
            return more(function(v5) {
              if (v4.value2) {
                return $$throw(v4, err);
              }
              ;
              return v1(v2, more, lift3, $$throw, done);
            });
          }, done);
        });
      };
    };
  },
  Functor0: function() {
    return functorParserT;
  }
};
var stateParserT = function(k) {
  return function(state1, v, v1, v2, done) {
    var v3 = k(state1);
    return done(v3.value1, v3.value0);
  };
};
var runParserT$prime = function(dictMonadRec) {
  return function(state1) {
    return function(v) {
      var go = function($copy_step) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(step2) {
          var v1 = step2(unit);
          if (v1 instanceof More) {
            $copy_step = v1.value0;
            return;
          }
          ;
          if (v1 instanceof Lift) {
            $tco_done = true;
            return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(Loop.create)(v1.value0);
          }
          ;
          if (v1 instanceof Stop) {
            $tco_done = true;
            return pure(dictMonadRec.Monad0().Applicative0())(new Done(new Tuple(v1.value1, v1.value0)));
          }
          ;
          throw new Error("Failed pattern match at Parsing (line 144, column 13 - line 150, column 32): " + [v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_step);
        }
        ;
        return $tco_result;
      };
      return tailRecM(dictMonadRec)(go)(function(v1) {
        return v(state1, More.create, Lift.create, function(state2, err) {
          return new Stop(state2, new Left(err));
        }, function(state2, res) {
          return new Stop(state2, new Right(res));
        });
      });
    };
  };
};
var position = /* @__PURE__ */ stateParserT(function(v) {
  return new Tuple(v.value1, v);
});
var initialPos = {
  index: 0,
  line: 1,
  column: 1
};
var runParserT = function(dictMonadRec) {
  return function(s) {
    return function(p) {
      var initialState = new ParseState(s, initialPos, false);
      return map(dictMonadRec.Monad0().Bind1().Apply0().Functor0())(fst)(runParserT$prime(dictMonadRec)(initialState)(p));
    };
  };
};
var runParser = function(s) {
  var $185 = unwrap();
  var $186 = runParserT(monadRecIdentity)(s);
  return function($187) {
    return $185($186($187));
  };
};
var failWithPosition = function(message2) {
  return function(pos) {
    return throwError(monadThrowParseErrorParse)(new ParseError(message2, pos));
  };
};
var fail = function(message2) {
  return bindFlipped(bindParserT)(failWithPosition(message2))(position);
};
var plusParserT = {
  empty: /* @__PURE__ */ fail("No alternative"),
  Alt0: function() {
    return altParserT;
  }
};
var alternativeParserT = {
  Applicative0: function() {
    return applicativeParserT;
  },
  Plus1: function() {
    return plusParserT;
  }
};

// output/Data.List.NonEmpty/index.js
var toList = function(v) {
  return new Cons(v.value0, v.value1);
};
var cons$prime = function(x) {
  return function(xs) {
    return new NonEmpty(x, xs);
  };
};

// output/Parsing.Combinators/index.js
var withLazyErrorMessage = function(p) {
  return function(msg) {
    return alt(altParserT)(p)(defer(lazyParserT)(function(v) {
      return fail("Expected " + msg(unit));
    }));
  };
};
var withErrorMessage = function(p) {
  return function(msg) {
    return alt(altParserT)(p)(fail("Expected " + msg));
  };
};
var $$try = function(v) {
  return function(v1, more, lift3, $$throw, done) {
    return v(v1, more, lift3, function(v2, err) {
      return $$throw(new ParseState(v2.value0, v2.value1, v1.value2), err);
    }, done);
  };
};
var skipMany1 = function(p) {
  var go = function(v) {
    return alt(altParserT)(voidLeft(functorParserT)(p)(new Loop(unit)))(pure(applicativeParserT)(new Done(unit)));
  };
  return applySecond(applyParserT)(p)(tailRecM(monadRecParserT)(go)(unit));
};
var skipMany = function(p) {
  return alt(altParserT)(skipMany1(p))(pure(applicativeParserT)(unit));
};
var sepBy1 = function(p) {
  return function(sep) {
    return bind(bindParserT)(p)(function(a) {
      return bind(bindParserT)(manyRec(monadRecParserT)(alternativeParserT)(applySecond(applyParserT)(sep)(p)))(function(as) {
        return pure(applicativeParserT)(cons$prime(a)(as));
      });
    });
  };
};
var sepBy = function(p) {
  return function(sep) {
    return alt(altParserT)(map(functorParserT)(toList)(sepBy1(p)(sep)))(pure(applicativeParserT)(Nil.value));
  };
};
var option = function(a) {
  return function(p) {
    return alt(altParserT)(p)(pure(applicativeParserT)(a));
  };
};
var notFollowedBy = function(p) {
  return $$try(alt(altParserT)(applySecond(applyParserT)($$try(p))(fail("Negated parser succeeded")))(pure(applicativeParserT)(unit)));
};
var many2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
var lookAhead = function(v) {
  return function(state1, more, lift3, $$throw, done) {
    return v(state1, more, lift3, function(v1, err) {
      return $$throw(state1, err);
    }, function(v1, res) {
      return done(state1, res);
    });
  };
};
var choice = function(dictFoldable) {
  var go = function(p1) {
    return function(v) {
      if (v instanceof Nothing) {
        return new Just(p1);
      }
      ;
      if (v instanceof Just) {
        return new Just(alt(altParserT)(p1)(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Parsing.Combinators (line 357, column 11 - line 359, column 32): " + [v.constructor.name]);
    };
  };
  var $68 = fromMaybe(empty(plusParserT));
  var $69 = foldr(dictFoldable)(go)(Nothing.value);
  return function($70) {
    return $68($69($70));
  };
};
var between = function(open) {
  return function(close) {
    return function(p) {
      return applyFirst(applyParserT)(applySecond(applyParserT)(open)(p))(close);
    };
  };
};
var asErrorMessage = /* @__PURE__ */ flip(withErrorMessage);

// output/Data.Array/foreign.js
var replicateFill = function(count) {
  return function(value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
};
var replicatePolyfill = function(count) {
  return function(value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
};
var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var fromFoldableImpl = function() {
  function Cons3(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  }
  var emptyList = {};
  function curryCons(head4) {
    return function(tail2) {
      return new Cons3(head4, tail2);
    };
  }
  function listToArray(list) {
    var result = [];
    var count = 0;
    var xs = list;
    while (xs !== emptyList) {
      result[count++] = xs.head;
      xs = xs.tail;
    }
    return result;
  }
  return function(foldr2) {
    return function(xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  };
}();
var length2 = function(xs) {
  return xs.length;
};
var unconsImpl = function(empty2) {
  return function(next) {
    return function(xs) {
      return xs.length === 0 ? empty2({}) : next(xs[0])(xs.slice(1));
    };
  };
};
var findIndexImpl = function(just) {
  return function(nothing) {
    return function(f) {
      return function(xs) {
        for (var i = 0, l = xs.length; i < l; i++) {
          if (f(xs[i]))
            return just(i);
        }
        return nothing;
      };
    };
  };
};
var sortByImpl = function() {
  function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare2(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare2) {
    return function(fromOrdering) {
      return function(xs) {
        var out;
        if (xs.length < 2)
          return xs;
        out = xs.slice(0);
        mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
        return out;
      };
    };
  };
}();
var zipWith2 = function(f) {
  return function(xs) {
    return function(ys) {
      var l = xs.length < ys.length ? xs.length : ys.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(xs[i])(ys[i]);
      }
      return result;
    };
  };
};

// output/Data.Array.ST/foreign.js
var sortByImpl2 = function() {
  function mergeFromTo(compare2, fromOrdering, xs1, xs2, from2, to) {
    var mid;
    var i;
    var j;
    var k;
    var x;
    var y;
    var c;
    mid = from2 + (to - from2 >> 1);
    if (mid - from2 > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, from2, mid);
    if (to - mid > 1)
      mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
    i = from2;
    j = mid;
    k = from2;
    while (i < mid && j < to) {
      x = xs2[i];
      y = xs2[j];
      c = fromOrdering(compare2(x)(y));
      if (c > 0) {
        xs1[k++] = y;
        ++j;
      } else {
        xs1[k++] = x;
        ++i;
      }
    }
    while (i < mid) {
      xs1[k++] = xs2[i++];
    }
    while (j < to) {
      xs1[k++] = xs2[j++];
    }
  }
  return function(compare2) {
    return function(fromOrdering) {
      return function(xs) {
        return function() {
          if (xs.length < 2)
            return xs;
          mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
          return xs;
        };
      };
    };
  };
}();

// output/Data.Array/index.js
var zip = /* @__PURE__ */ function() {
  return zipWith2(Tuple.create);
}();
var uncons2 = /* @__PURE__ */ function() {
  return unconsImpl($$const(Nothing.value))(function(x) {
    return function(xs) {
      return new Just({
        head: x,
        tail: xs
      });
    };
  });
}();
var sortBy2 = function(comp) {
  return sortByImpl(comp)(function(v) {
    if (v instanceof GT) {
      return 1;
    }
    ;
    if (v instanceof EQ) {
      return 0;
    }
    ;
    if (v instanceof LT) {
      return -1 | 0;
    }
    ;
    throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [v.constructor.name]);
  });
};
var sort = function(dictOrd) {
  return function(xs) {
    return sortBy2(compare(dictOrd))(xs);
  };
};
var findIndex2 = /* @__PURE__ */ function() {
  return findIndexImpl(Just.create)(Nothing.value);
}();
var elemIndex = function(dictEq) {
  return function(x) {
    return findIndex2(function(v) {
      return eq(dictEq)(v)(x);
    });
  };
};
var notElem2 = function(dictEq) {
  return function(a) {
    return function(arr) {
      return isNothing(elemIndex(dictEq)(a)(arr));
    };
  };
};
var elem2 = function(dictEq) {
  return function(a) {
    return function(arr) {
      return isJust(elemIndex(dictEq)(a)(arr));
    };
  };
};
var cons3 = function(x) {
  return function(xs) {
    return append(semigroupArray)([x])(xs);
  };
};
var some2 = function(dictAlternative) {
  return function(dictLazy) {
    return function(v) {
      return apply(dictAlternative.Applicative0().Apply0())(map(dictAlternative.Plus1().Alt0().Functor0())(cons3)(v))(defer(dictLazy)(function(v1) {
        return many3(dictAlternative)(dictLazy)(v);
      }));
    };
  };
};
var many3 = function(dictAlternative) {
  return function(dictLazy) {
    return function(v) {
      return alt(dictAlternative.Plus1().Alt0())(some2(dictAlternative)(dictLazy)(v))(pure(dictAlternative.Applicative0())([]));
    };
  };
};

// output/Data.Array.NonEmpty.Internal/foreign.js
var traverse1Impl = function() {
  function Cont(fn) {
    this.fn = fn;
  }
  var emptyList = {};
  var ConsCell = function(head4, tail2) {
    this.head = head4;
    this.tail = tail2;
  };
  function finalCell(head4) {
    return new ConsCell(head4, emptyList);
  }
  function consList(x) {
    return function(xs) {
      return new ConsCell(x, xs);
    };
  }
  function listToArray(list) {
    var arr = [];
    var xs = list;
    while (xs !== emptyList) {
      arr.push(xs.head);
      xs = xs.tail;
    }
    return arr;
  }
  return function(apply2) {
    return function(map2) {
      return function(f) {
        var buildFrom = function(x, ys) {
          return apply2(map2(consList)(f(x)))(ys);
        };
        var go = function(acc, currentLen, xs) {
          if (currentLen === 0) {
            return acc;
          } else {
            var last3 = xs[currentLen - 1];
            return new Cont(function() {
              var built = go(buildFrom(last3, acc), currentLen - 1, xs);
              return built;
            });
          }
        };
        return function(array) {
          var acc = map2(finalCell)(f(array[array.length - 1]));
          var result = go(acc, array.length - 1, array);
          while (result instanceof Cont) {
            result = result.fn();
          }
          return map2(listToArray)(result);
        };
      };
    };
  };
}();

// output/Data.Function.Uncurried/foreign.js
var mkFn5 = function(fn) {
  return function(a, b, c, d, e) {
    return fn(a)(b)(c)(d)(e);
  };
};

// output/Data.String.CodePoints/foreign.js
var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";
var _unsafeCodePointAt0 = function(fallback) {
  return hasCodePointAt ? function(str) {
    return str.codePointAt(0);
  } : fallback;
};
var _codePointAt = function(fallback) {
  return function(Just2) {
    return function(Nothing2) {
      return function(unsafeCodePointAt02) {
        return function(index3) {
          return function(str) {
            var length5 = str.length;
            if (index3 < 0 || index3 >= length5)
              return Nothing2;
            if (hasStringIterator) {
              var iter = str[Symbol.iterator]();
              for (var i = index3; ; --i) {
                var o = iter.next();
                if (o.done)
                  return Nothing2;
                if (i === 0)
                  return Just2(unsafeCodePointAt02(o.value));
              }
            }
            return fallback(index3)(str);
          };
        };
      };
    };
  };
};
var _fromCodePointArray = function(singleton8) {
  return hasFromCodePoint ? function(cps) {
    if (cps.length < 1e4) {
      return String.fromCodePoint.apply(String, cps);
    }
    return cps.map(singleton8).join("");
  } : function(cps) {
    return cps.map(singleton8).join("");
  };
};
var _toCodePointArray = function(fallback) {
  return function(unsafeCodePointAt02) {
    if (hasArrayFrom) {
      return function(str) {
        return Array.from(str, unsafeCodePointAt02);
      };
    }
    return fallback;
  };
};

// output/Data.String.CodeUnits/foreign.js
var fromCharArray = function(a) {
  return a.join("");
};
var toCharArray = function(s) {
  return s.split("");
};
var singleton6 = function(c) {
  return c;
};
var _toChar = function(just) {
  return function(nothing) {
    return function(s) {
      return s.length === 1 ? just(s) : nothing;
    };
  };
};
var length3 = function(s) {
  return s.length;
};
var drop3 = function(n) {
  return function(s) {
    return s.substring(n);
  };
};
var splitAt2 = function(i) {
  return function(s) {
    return { before: s.substring(0, i), after: s.substring(i) };
  };
};

// output/Data.String.Unsafe/foreign.js
var charAt = function(i) {
  return function(s) {
    if (i >= 0 && i < s.length)
      return s.charAt(i);
    throw new Error("Data.String.Unsafe.charAt: Invalid index.");
  };
};

// output/Data.String.CodeUnits/index.js
var uncons3 = function(v) {
  if (v === "") {
    return Nothing.value;
  }
  ;
  return new Just({
    head: charAt(0)(v),
    tail: drop3(1)(v)
  });
};
var toChar = /* @__PURE__ */ function() {
  return _toChar(Just.create)(Nothing.value);
}();
var stripPrefix = function(v) {
  return function(str) {
    var v1 = splitAt2(length3(v))(str);
    var $15 = v1.before === v;
    if ($15) {
      return new Just(v1.after);
    }
    ;
    return Nothing.value;
  };
};

// output/Data.String.CodePoints/index.js
var $runtime_lazy4 = function(name2, moduleName, init3) {
  var state2 = 0;
  var val;
  return function(lineNumber) {
    if (state2 === 2)
      return val;
    if (state2 === 1)
      throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state2 = 1;
    val = init3();
    state2 = 2;
    return val;
  };
};
var CodePoint = function(x) {
  return x;
};
var unsurrogate = function(lead) {
  return function(trail) {
    return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
  };
};
var isTrail = function(cu) {
  return 56320 <= cu && cu <= 57343;
};
var isLead = function(cu) {
  return 55296 <= cu && cu <= 56319;
};
var uncons4 = function(s) {
  var v = length3(s);
  if (v === 0) {
    return Nothing.value;
  }
  ;
  if (v === 1) {
    return new Just({
      head: fromEnum(boundedEnumChar)(charAt(0)(s)),
      tail: ""
    });
  }
  ;
  var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
  var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
  var $21 = isLead(cu0) && isTrail(cu1);
  if ($21) {
    return new Just({
      head: unsurrogate(cu0)(cu1),
      tail: drop3(2)(s)
    });
  }
  ;
  return new Just({
    head: cu0,
    tail: drop3(1)(s)
  });
};
var unconsButWithTuple = function(s) {
  return map(functorMaybe)(function(v) {
    return new Tuple(v.head, v.tail);
  })(uncons4(s));
};
var toCodePointArrayFallback = function(s) {
  return unfoldr(unfoldableArray)(unconsButWithTuple)(s);
};
var unsafeCodePointAt0Fallback = function(s) {
  var cu0 = fromEnum(boundedEnumChar)(charAt(0)(s));
  var $25 = isLead(cu0) && length3(s) > 1;
  if ($25) {
    var cu1 = fromEnum(boundedEnumChar)(charAt(1)(s));
    var $26 = isTrail(cu1);
    if ($26) {
      return unsurrogate(cu0)(cu1);
    }
    ;
    return cu0;
  }
  ;
  return cu0;
};
var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
var fromCharCode2 = /* @__PURE__ */ function() {
  var $53 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
  return function($54) {
    return singleton6($53($54));
  };
}();
var singletonFallback = function(v) {
  if (v <= 65535) {
    return fromCharCode2(v);
  }
  ;
  var lead = div(euclideanRingInt)(v - 65536 | 0)(1024) + 55296 | 0;
  var trail = mod(euclideanRingInt)(v - 65536 | 0)(1024) + 56320 | 0;
  return fromCharCode2(lead) + fromCharCode2(trail);
};
var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
var eqCodePoint = {
  eq: function(x) {
    return function(y) {
      return x === y;
    };
  }
};
var ordCodePoint = {
  compare: function(x) {
    return function(y) {
      return compare(ordInt)(x)(y);
    };
  },
  Eq0: function() {
    return eqCodePoint;
  }
};
var codePointFromChar = /* @__PURE__ */ function() {
  var $55 = fromEnum(boundedEnumChar);
  return function($56) {
    return CodePoint($55($56));
  };
}();
var codePointAtFallback = function($copy_n) {
  return function($copy_s) {
    var $tco_var_n = $copy_n;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(n, s) {
      var v = uncons4(s);
      if (v instanceof Just) {
        var $44 = n === 0;
        if ($44) {
          $tco_done = true;
          return new Just(v.value0.head);
        }
        ;
        $tco_var_n = n - 1 | 0;
        $copy_s = v.value0.tail;
        return;
      }
      ;
      $tco_done = true;
      return Nothing.value;
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_n, $copy_s);
    }
    ;
    return $tco_result;
  };
};
var codePointAt = function(v) {
  return function(v1) {
    if (v < 0) {
      return Nothing.value;
    }
    ;
    if (v === 0 && v1 === "") {
      return Nothing.value;
    }
    ;
    if (v === 0) {
      return new Just(unsafeCodePointAt0(v1));
    }
    ;
    return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
  };
};
var boundedCodePoint = {
  bottom: 0,
  top: 1114111,
  Ord0: function() {
    return ordCodePoint;
  }
};
var boundedEnumCodePoint = /* @__PURE__ */ function() {
  return {
    cardinality: 1114111 + 1 | 0,
    fromEnum: function(v) {
      return v;
    },
    toEnum: function(n) {
      if (n >= 0 && n <= 1114111) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
    },
    Bounded0: function() {
      return boundedCodePoint;
    },
    Enum1: function() {
      return $lazy_enumCodePoint(0);
    }
  };
}();
var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy4("enumCodePoint", "Data.String.CodePoints", function() {
  return {
    succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
    Ord0: function() {
      return ordCodePoint;
    }
  };
});

// output/Parsing.String/index.js
var updatePosSingle = function(v) {
  return function(cp) {
    return function(after) {
      var v1 = fromEnum(boundedEnumCodePoint)(cp);
      if (v1 === 10) {
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 13) {
        var v2 = codePointAt(0)(after);
        if (v2 instanceof Just && fromEnum(boundedEnumCodePoint)(v2.value0) === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: v.column
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line + 1 | 0,
          column: 1
        };
      }
      ;
      if (v1 === 9) {
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: (v.column + 8 | 0) - mod(euclideanRingInt)(v.column - 1 | 0)(8) | 0
        };
      }
      ;
      return {
        index: v.index + 1 | 0,
        line: v.line,
        column: v.column + 1 | 0
      };
    };
  };
};
var updatePosString = function($copy_pos) {
  return function($copy_before) {
    return function($copy_after) {
      var $tco_var_pos = $copy_pos;
      var $tco_var_before = $copy_before;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(pos, before, after) {
        var v = uncons4(before);
        if (v instanceof Nothing) {
          $tco_done = true;
          return pos;
        }
        ;
        if (v instanceof Just) {
          var newPos = function() {
            if ($$null(v.value0.tail)) {
              return updatePosSingle(pos)(v.value0.head)(after);
            }
            ;
            if (otherwise) {
              return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 160, column 7 - line 162, column 52): " + []);
          }();
          $tco_var_pos = newPos;
          $tco_var_before = v.value0.tail;
          $copy_after = after;
          return;
        }
        ;
        throw new Error("Failed pattern match at Parsing.String (line 156, column 36 - line 163, column 38): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
      }
      ;
      return $tco_result;
    };
  };
};
var satisfyCodePoint = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = uncons4(v.value0);
            if (v3 instanceof Nothing) {
              return $$throw(v, new ParseError("Unexpected EOF", v.value1));
            }
            ;
            if (v3 instanceof Just) {
              var $44 = f(v3.value0.head);
              if ($44) {
                return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
              }
              ;
              return $$throw(v, new ParseError("Predicate unsatisfied", v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 131, column 7 - line 138, column 73): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var satisfy = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = uncons4(v.value0);
            if (v3 instanceof Nothing) {
              return $$throw(v, new ParseError("Unexpected EOF", v.value1));
            }
            ;
            if (v3 instanceof Just) {
              var cp = fromEnum(boundedEnumCodePoint)(v3.value0.head);
              var $53 = cp < 0 || cp > 65535;
              if ($53) {
                return $$throw(v, new ParseError("Expected Char", v.value1));
              }
              ;
              var ch = fromJust()(toEnum(boundedEnumChar)(cp));
              var $54 = f(ch);
              if ($54) {
                return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
              }
              ;
              return $$throw(v, new ParseError("Predicate unsatisfied", v.value1));
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 109, column 7 - line 124, column 75): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var eof = /* @__PURE__ */ mkFn5(function(v) {
  return function(v1) {
    return function(v2) {
      return function($$throw) {
        return function(done) {
          var $70 = $$null(v.value0);
          if ($70) {
            return done(new ParseState(v.value0, v.value1, true), unit);
          }
          ;
          return $$throw(v, new ParseError("Expected EOF", v.value1));
        };
      };
    };
  };
});
var consumeWith = function(f) {
  return mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw) {
          return function(done) {
            var v3 = f(v.value0);
            if (v3 instanceof Left) {
              return $$throw(v, new ParseError(v3.value0, v.value1));
            }
            ;
            if (v3 instanceof Right) {
              return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), true), v3.value0.value);
            }
            ;
            throw new Error("Failed pattern match at Parsing.String (line 280, column 7 - line 284, column 97): " + [v3.constructor.name]);
          };
        };
      };
    };
  });
};
var string = function(str) {
  return consumeWith(function(input) {
    var v = stripPrefix(str)(input);
    if (v instanceof Just) {
      return new Right({
        value: str,
        consumed: str,
        remainder: v.value0
      });
    }
    ;
    return new Left("Expected " + show(showString)(str));
  });
};
var $$char = function(c) {
  return withErrorMessage(satisfy(function(v) {
    return v === c;
  }))(show(showChar)(c));
};

// output/Data.Char/index.js
var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
var fromCharCode3 = /* @__PURE__ */ toEnum(boundedEnumChar);

// output/Data.CodePoint.Unicode.Internal/index.js
var NUMCAT_LU = /* @__PURE__ */ function() {
  function NUMCAT_LU2() {
  }
  ;
  NUMCAT_LU2.value = new NUMCAT_LU2();
  return NUMCAT_LU2;
}();
var NUMCAT_LL = /* @__PURE__ */ function() {
  function NUMCAT_LL2() {
  }
  ;
  NUMCAT_LL2.value = new NUMCAT_LL2();
  return NUMCAT_LL2;
}();
var NUMCAT_LT = /* @__PURE__ */ function() {
  function NUMCAT_LT2() {
  }
  ;
  NUMCAT_LT2.value = new NUMCAT_LT2();
  return NUMCAT_LT2;
}();
var NUMCAT_LM = /* @__PURE__ */ function() {
  function NUMCAT_LM2() {
  }
  ;
  NUMCAT_LM2.value = new NUMCAT_LM2();
  return NUMCAT_LM2;
}();
var NUMCAT_LO = /* @__PURE__ */ function() {
  function NUMCAT_LO2() {
  }
  ;
  NUMCAT_LO2.value = new NUMCAT_LO2();
  return NUMCAT_LO2;
}();
var NUMCAT_MN = /* @__PURE__ */ function() {
  function NUMCAT_MN2() {
  }
  ;
  NUMCAT_MN2.value = new NUMCAT_MN2();
  return NUMCAT_MN2;
}();
var NUMCAT_MC = /* @__PURE__ */ function() {
  function NUMCAT_MC2() {
  }
  ;
  NUMCAT_MC2.value = new NUMCAT_MC2();
  return NUMCAT_MC2;
}();
var NUMCAT_ME = /* @__PURE__ */ function() {
  function NUMCAT_ME2() {
  }
  ;
  NUMCAT_ME2.value = new NUMCAT_ME2();
  return NUMCAT_ME2;
}();
var NUMCAT_ND = /* @__PURE__ */ function() {
  function NUMCAT_ND2() {
  }
  ;
  NUMCAT_ND2.value = new NUMCAT_ND2();
  return NUMCAT_ND2;
}();
var NUMCAT_NL = /* @__PURE__ */ function() {
  function NUMCAT_NL2() {
  }
  ;
  NUMCAT_NL2.value = new NUMCAT_NL2();
  return NUMCAT_NL2;
}();
var NUMCAT_NO = /* @__PURE__ */ function() {
  function NUMCAT_NO2() {
  }
  ;
  NUMCAT_NO2.value = new NUMCAT_NO2();
  return NUMCAT_NO2;
}();
var NUMCAT_PC = /* @__PURE__ */ function() {
  function NUMCAT_PC2() {
  }
  ;
  NUMCAT_PC2.value = new NUMCAT_PC2();
  return NUMCAT_PC2;
}();
var NUMCAT_PD = /* @__PURE__ */ function() {
  function NUMCAT_PD2() {
  }
  ;
  NUMCAT_PD2.value = new NUMCAT_PD2();
  return NUMCAT_PD2;
}();
var NUMCAT_PS = /* @__PURE__ */ function() {
  function NUMCAT_PS2() {
  }
  ;
  NUMCAT_PS2.value = new NUMCAT_PS2();
  return NUMCAT_PS2;
}();
var NUMCAT_PE = /* @__PURE__ */ function() {
  function NUMCAT_PE2() {
  }
  ;
  NUMCAT_PE2.value = new NUMCAT_PE2();
  return NUMCAT_PE2;
}();
var NUMCAT_PI = /* @__PURE__ */ function() {
  function NUMCAT_PI2() {
  }
  ;
  NUMCAT_PI2.value = new NUMCAT_PI2();
  return NUMCAT_PI2;
}();
var NUMCAT_PF = /* @__PURE__ */ function() {
  function NUMCAT_PF2() {
  }
  ;
  NUMCAT_PF2.value = new NUMCAT_PF2();
  return NUMCAT_PF2;
}();
var NUMCAT_PO = /* @__PURE__ */ function() {
  function NUMCAT_PO2() {
  }
  ;
  NUMCAT_PO2.value = new NUMCAT_PO2();
  return NUMCAT_PO2;
}();
var NUMCAT_SM = /* @__PURE__ */ function() {
  function NUMCAT_SM2() {
  }
  ;
  NUMCAT_SM2.value = new NUMCAT_SM2();
  return NUMCAT_SM2;
}();
var NUMCAT_SC = /* @__PURE__ */ function() {
  function NUMCAT_SC2() {
  }
  ;
  NUMCAT_SC2.value = new NUMCAT_SC2();
  return NUMCAT_SC2;
}();
var NUMCAT_SK = /* @__PURE__ */ function() {
  function NUMCAT_SK2() {
  }
  ;
  NUMCAT_SK2.value = new NUMCAT_SK2();
  return NUMCAT_SK2;
}();
var NUMCAT_SO = /* @__PURE__ */ function() {
  function NUMCAT_SO2() {
  }
  ;
  NUMCAT_SO2.value = new NUMCAT_SO2();
  return NUMCAT_SO2;
}();
var NUMCAT_ZS = /* @__PURE__ */ function() {
  function NUMCAT_ZS2() {
  }
  ;
  NUMCAT_ZS2.value = new NUMCAT_ZS2();
  return NUMCAT_ZS2;
}();
var NUMCAT_ZL = /* @__PURE__ */ function() {
  function NUMCAT_ZL2() {
  }
  ;
  NUMCAT_ZL2.value = new NUMCAT_ZL2();
  return NUMCAT_ZL2;
}();
var NUMCAT_ZP = /* @__PURE__ */ function() {
  function NUMCAT_ZP2() {
  }
  ;
  NUMCAT_ZP2.value = new NUMCAT_ZP2();
  return NUMCAT_ZP2;
}();
var NUMCAT_CC = /* @__PURE__ */ function() {
  function NUMCAT_CC2() {
  }
  ;
  NUMCAT_CC2.value = new NUMCAT_CC2();
  return NUMCAT_CC2;
}();
var NUMCAT_CF = /* @__PURE__ */ function() {
  function NUMCAT_CF2() {
  }
  ;
  NUMCAT_CF2.value = new NUMCAT_CF2();
  return NUMCAT_CF2;
}();
var NUMCAT_CS = /* @__PURE__ */ function() {
  function NUMCAT_CS2() {
  }
  ;
  NUMCAT_CS2.value = new NUMCAT_CS2();
  return NUMCAT_CS2;
}();
var NUMCAT_CO = /* @__PURE__ */ function() {
  function NUMCAT_CO2() {
  }
  ;
  NUMCAT_CO2.value = new NUMCAT_CO2();
  return NUMCAT_CO2;
}();
var NUMCAT_CN = /* @__PURE__ */ function() {
  function NUMCAT_CN2() {
  }
  ;
  NUMCAT_CN2.value = new NUMCAT_CN2();
  return NUMCAT_CN2;
}();
var numSpaceBlocks = 7;
var numLat1Blocks = 63;
var numConvBlocks = 1332;
var numBlocks = 3396;
var gencatZS = 2;
var rule1 = /* @__PURE__ */ function() {
  return {
    category: gencatZS,
    unicodeCat: NUMCAT_ZS.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var spacechars = [{
  start: 32,
  length: 1,
  convRule: rule1
}, {
  start: 160,
  length: 1,
  convRule: rule1
}, {
  start: 5760,
  length: 1,
  convRule: rule1
}, {
  start: 8192,
  length: 11,
  convRule: rule1
}, {
  start: 8239,
  length: 1,
  convRule: rule1
}, {
  start: 8287,
  length: 1,
  convRule: rule1
}, {
  start: 12288,
  length: 1,
  convRule: rule1
}];
var gencatZP = 67108864;
var rule162 = /* @__PURE__ */ function() {
  return {
    category: gencatZP,
    unicodeCat: NUMCAT_ZP.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatZL = 33554432;
var rule161 = /* @__PURE__ */ function() {
  return {
    category: gencatZL,
    unicodeCat: NUMCAT_ZL.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatSO = 8192;
var rule13 = /* @__PURE__ */ function() {
  return {
    category: gencatSO,
    unicodeCat: NUMCAT_SO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule170 = /* @__PURE__ */ function() {
  return {
    category: gencatSO,
    unicodeCat: NUMCAT_SO.value,
    possible: 1,
    updist: 0,
    lowdist: 26,
    titledist: 0
  };
}();
var rule171 = /* @__PURE__ */ function() {
  return {
    category: gencatSO,
    unicodeCat: NUMCAT_SO.value,
    possible: 1,
    updist: -26 | 0,
    lowdist: 0,
    titledist: -26 | 0
  };
}();
var gencatSM = 64;
var rule6 = /* @__PURE__ */ function() {
  return {
    category: gencatSM,
    unicodeCat: NUMCAT_SM.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatSK = 1024;
var rule10 = /* @__PURE__ */ function() {
  return {
    category: gencatSK,
    unicodeCat: NUMCAT_SK.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatSC = 8;
var rule3 = /* @__PURE__ */ function() {
  return {
    category: gencatSC,
    unicodeCat: NUMCAT_SC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPS = 16;
var rule4 = /* @__PURE__ */ function() {
  return {
    category: gencatPS,
    unicodeCat: NUMCAT_PS.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPO = 4;
var rule2 = /* @__PURE__ */ function() {
  return {
    category: gencatPO,
    unicodeCat: NUMCAT_PO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPI = 32768;
var rule15 = /* @__PURE__ */ function() {
  return {
    category: gencatPI,
    unicodeCat: NUMCAT_PI.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPF = 262144;
var rule19 = /* @__PURE__ */ function() {
  return {
    category: gencatPF,
    unicodeCat: NUMCAT_PF.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPE = 32;
var rule5 = /* @__PURE__ */ function() {
  return {
    category: gencatPE,
    unicodeCat: NUMCAT_PE.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPD = 128;
var rule7 = /* @__PURE__ */ function() {
  return {
    category: gencatPD,
    unicodeCat: NUMCAT_PD.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatPC = 2048;
var rule11 = /* @__PURE__ */ function() {
  return {
    category: gencatPC,
    unicodeCat: NUMCAT_PC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatNO = 131072;
var rule17 = /* @__PURE__ */ function() {
  return {
    category: gencatNO,
    unicodeCat: NUMCAT_NO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatNL = 16777216;
var rule128 = /* @__PURE__ */ function() {
  return {
    category: gencatNL,
    unicodeCat: NUMCAT_NL.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule168 = /* @__PURE__ */ function() {
  return {
    category: gencatNL,
    unicodeCat: NUMCAT_NL.value,
    possible: 1,
    updist: 0,
    lowdist: 16,
    titledist: 0
  };
}();
var rule169 = /* @__PURE__ */ function() {
  return {
    category: gencatNL,
    unicodeCat: NUMCAT_NL.value,
    possible: 1,
    updist: -16 | 0,
    lowdist: 0,
    titledist: -16 | 0
  };
}();
var gencatND = 256;
var rule8 = /* @__PURE__ */ function() {
  return {
    category: gencatND,
    unicodeCat: NUMCAT_ND.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatMN = 2097152;
var rule92 = /* @__PURE__ */ function() {
  return {
    category: gencatMN,
    unicodeCat: NUMCAT_MN.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule93 = /* @__PURE__ */ function() {
  return {
    category: gencatMN,
    unicodeCat: NUMCAT_MN.value,
    possible: 1,
    updist: 84,
    lowdist: 0,
    titledist: 84
  };
}();
var gencatME = 4194304;
var rule119 = /* @__PURE__ */ function() {
  return {
    category: gencatME,
    unicodeCat: NUMCAT_ME.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatMC = 8388608;
var rule124 = /* @__PURE__ */ function() {
  return {
    category: gencatMC,
    unicodeCat: NUMCAT_MC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatLU = 512;
var nullrule = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_CN.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule104 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 8,
    titledist: 0
  };
}();
var rule107 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule115 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -60 | 0,
    titledist: 0
  };
}();
var rule117 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -7 | 0,
    titledist: 0
  };
}();
var rule118 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 80,
    titledist: 0
  };
}();
var rule120 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 15,
    titledist: 0
  };
}();
var rule122 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 48,
    titledist: 0
  };
}();
var rule125 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 7264,
    titledist: 0
  };
}();
var rule127 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 38864,
    titledist: 0
  };
}();
var rule137 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -3008 | 0,
    titledist: 0
  };
}();
var rule142 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -7615 | 0,
    titledist: 0
  };
}();
var rule144 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -8 | 0,
    titledist: 0
  };
}();
var rule153 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -74 | 0,
    titledist: 0
  };
}();
var rule156 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -86 | 0,
    titledist: 0
  };
}();
var rule157 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -100 | 0,
    titledist: 0
  };
}();
var rule158 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -112 | 0,
    titledist: 0
  };
}();
var rule159 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -128 | 0,
    titledist: 0
  };
}();
var rule160 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -126 | 0,
    titledist: 0
  };
}();
var rule163 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -7517 | 0,
    titledist: 0
  };
}();
var rule164 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -8383 | 0,
    titledist: 0
  };
}();
var rule165 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -8262 | 0,
    titledist: 0
  };
}();
var rule166 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 28,
    titledist: 0
  };
}();
var rule172 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10743 | 0,
    titledist: 0
  };
}();
var rule173 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -3814 | 0,
    titledist: 0
  };
}();
var rule174 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10727 | 0,
    titledist: 0
  };
}();
var rule177 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10780 | 0,
    titledist: 0
  };
}();
var rule178 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10749 | 0,
    titledist: 0
  };
}();
var rule179 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10783 | 0,
    titledist: 0
  };
}();
var rule180 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10782 | 0,
    titledist: 0
  };
}();
var rule181 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -10815 | 0,
    titledist: 0
  };
}();
var rule183 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -35332 | 0,
    titledist: 0
  };
}();
var rule184 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42280 | 0,
    titledist: 0
  };
}();
var rule186 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42308 | 0,
    titledist: 0
  };
}();
var rule187 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42319 | 0,
    titledist: 0
  };
}();
var rule188 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42315 | 0,
    titledist: 0
  };
}();
var rule189 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42305 | 0,
    titledist: 0
  };
}();
var rule190 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42258 | 0,
    titledist: 0
  };
}();
var rule191 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42282 | 0,
    titledist: 0
  };
}();
var rule192 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42261 | 0,
    titledist: 0
  };
}();
var rule193 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 928,
    titledist: 0
  };
}();
var rule194 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -48 | 0,
    titledist: 0
  };
}();
var rule195 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -42307 | 0,
    titledist: 0
  };
}();
var rule196 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -35384 | 0,
    titledist: 0
  };
}();
var rule201 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 40,
    titledist: 0
  };
}();
var rule203 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 34,
    titledist: 0
  };
}();
var rule22 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 1,
    titledist: 0
  };
}();
var rule24 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -199 | 0,
    titledist: 0
  };
}();
var rule26 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -121 | 0,
    titledist: 0
  };
}();
var rule29 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 210,
    titledist: 0
  };
}();
var rule30 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 206,
    titledist: 0
  };
}();
var rule31 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 205,
    titledist: 0
  };
}();
var rule32 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 79,
    titledist: 0
  };
}();
var rule33 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 202,
    titledist: 0
  };
}();
var rule34 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 203,
    titledist: 0
  };
}();
var rule35 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 207,
    titledist: 0
  };
}();
var rule37 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 211,
    titledist: 0
  };
}();
var rule38 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 209,
    titledist: 0
  };
}();
var rule40 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 213,
    titledist: 0
  };
}();
var rule42 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 214,
    titledist: 0
  };
}();
var rule43 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 218,
    titledist: 0
  };
}();
var rule44 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 217,
    titledist: 0
  };
}();
var rule45 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 219,
    titledist: 0
  };
}();
var rule47 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 2,
    titledist: 1
  };
}();
var rule51 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -97 | 0,
    titledist: 0
  };
}();
var rule52 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -56 | 0,
    titledist: 0
  };
}();
var rule53 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -130 | 0,
    titledist: 0
  };
}();
var rule54 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 10795,
    titledist: 0
  };
}();
var rule55 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -163 | 0,
    titledist: 0
  };
}();
var rule56 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 10792,
    titledist: 0
  };
}();
var rule58 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: -195 | 0,
    titledist: 0
  };
}();
var rule59 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 69,
    titledist: 0
  };
}();
var rule60 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 71,
    titledist: 0
  };
}();
var rule9 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 32,
    titledist: 0
  };
}();
var rule94 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 116,
    titledist: 0
  };
}();
var rule95 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 38,
    titledist: 0
  };
}();
var rule96 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 37,
    titledist: 0
  };
}();
var rule97 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 64,
    titledist: 0
  };
}();
var rule98 = /* @__PURE__ */ function() {
  return {
    category: gencatLU,
    unicodeCat: NUMCAT_LU.value,
    possible: 1,
    updist: 0,
    lowdist: 63,
    titledist: 0
  };
}();
var gencatLT = 524288;
var rule151 = /* @__PURE__ */ function() {
  return {
    category: gencatLT,
    unicodeCat: NUMCAT_LT.value,
    possible: 1,
    updist: 0,
    lowdist: -8 | 0,
    titledist: 0
  };
}();
var rule154 = /* @__PURE__ */ function() {
  return {
    category: gencatLT,
    unicodeCat: NUMCAT_LT.value,
    possible: 1,
    updist: 0,
    lowdist: -9 | 0,
    titledist: 0
  };
}();
var rule48 = /* @__PURE__ */ function() {
  return {
    category: gencatLT,
    unicodeCat: NUMCAT_LT.value,
    possible: 1,
    updist: -1 | 0,
    lowdist: 1,
    titledist: 0
  };
}();
var gencatLO = 16384;
var rule14 = /* @__PURE__ */ function() {
  return {
    category: gencatLO,
    unicodeCat: NUMCAT_LO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatLM = 1048576;
var rule91 = /* @__PURE__ */ function() {
  return {
    category: gencatLM,
    unicodeCat: NUMCAT_LM.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatLL = 4096;
var rule100 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -37 | 0,
    lowdist: 0,
    titledist: -37 | 0
  };
}();
var rule101 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -31 | 0,
    lowdist: 0,
    titledist: -31 | 0
  };
}();
var rule102 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -64 | 0,
    lowdist: 0,
    titledist: -64 | 0
  };
}();
var rule103 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -63 | 0,
    lowdist: 0,
    titledist: -63 | 0
  };
}();
var rule105 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -62 | 0,
    lowdist: 0,
    titledist: -62 | 0
  };
}();
var rule106 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -57 | 0,
    lowdist: 0,
    titledist: -57 | 0
  };
}();
var rule108 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -47 | 0,
    lowdist: 0,
    titledist: -47 | 0
  };
}();
var rule109 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -54 | 0,
    lowdist: 0,
    titledist: -54 | 0
  };
}();
var rule110 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -8 | 0,
    lowdist: 0,
    titledist: -8 | 0
  };
}();
var rule111 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -86 | 0,
    lowdist: 0,
    titledist: -86 | 0
  };
}();
var rule112 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -80 | 0,
    lowdist: 0,
    titledist: -80 | 0
  };
}();
var rule113 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 7,
    lowdist: 0,
    titledist: 7
  };
}();
var rule114 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -116 | 0,
    lowdist: 0,
    titledist: -116 | 0
  };
}();
var rule116 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -96 | 0,
    lowdist: 0,
    titledist: -96 | 0
  };
}();
var rule12 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -32 | 0,
    lowdist: 0,
    titledist: -32 | 0
  };
}();
var rule121 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -15 | 0,
    lowdist: 0,
    titledist: -15 | 0
  };
}();
var rule123 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -48 | 0,
    lowdist: 0,
    titledist: -48 | 0
  };
}();
var rule126 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 3008,
    lowdist: 0,
    titledist: 0
  };
}();
var rule129 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6254 | 0,
    lowdist: 0,
    titledist: -6254 | 0
  };
}();
var rule130 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6253 | 0,
    lowdist: 0,
    titledist: -6253 | 0
  };
}();
var rule131 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6244 | 0,
    lowdist: 0,
    titledist: -6244 | 0
  };
}();
var rule132 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6242 | 0,
    lowdist: 0,
    titledist: -6242 | 0
  };
}();
var rule133 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6243 | 0,
    lowdist: 0,
    titledist: -6243 | 0
  };
}();
var rule134 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6236 | 0,
    lowdist: 0,
    titledist: -6236 | 0
  };
}();
var rule135 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -6181 | 0,
    lowdist: 0,
    titledist: -6181 | 0
  };
}();
var rule136 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 35266,
    lowdist: 0,
    titledist: 35266
  };
}();
var rule138 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 35332,
    lowdist: 0,
    titledist: 35332
  };
}();
var rule139 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 3814,
    lowdist: 0,
    titledist: 3814
  };
}();
var rule140 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 35384,
    lowdist: 0,
    titledist: 35384
  };
}();
var rule141 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -59 | 0,
    lowdist: 0,
    titledist: -59 | 0
  };
}();
var rule143 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 8,
    lowdist: 0,
    titledist: 8
  };
}();
var rule145 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 74,
    lowdist: 0,
    titledist: 74
  };
}();
var rule146 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 86,
    lowdist: 0,
    titledist: 86
  };
}();
var rule147 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 100,
    lowdist: 0,
    titledist: 100
  };
}();
var rule148 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 128,
    lowdist: 0,
    titledist: 128
  };
}();
var rule149 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 112,
    lowdist: 0,
    titledist: 112
  };
}();
var rule150 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 126,
    lowdist: 0,
    titledist: 126
  };
}();
var rule152 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 9,
    lowdist: 0,
    titledist: 9
  };
}();
var rule155 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -7205 | 0,
    lowdist: 0,
    titledist: -7205 | 0
  };
}();
var rule167 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -28 | 0,
    lowdist: 0,
    titledist: -28 | 0
  };
}();
var rule175 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -10795 | 0,
    lowdist: 0,
    titledist: -10795 | 0
  };
}();
var rule176 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -10792 | 0,
    lowdist: 0,
    titledist: -10792 | 0
  };
}();
var rule18 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 743,
    lowdist: 0,
    titledist: 743
  };
}();
var rule182 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -7264 | 0,
    lowdist: 0,
    titledist: -7264 | 0
  };
}();
var rule185 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 48,
    lowdist: 0,
    titledist: 48
  };
}();
var rule197 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -928 | 0,
    lowdist: 0,
    titledist: -928 | 0
  };
}();
var rule198 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -38864 | 0,
    lowdist: 0,
    titledist: -38864 | 0
  };
}();
var rule20 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var rule202 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -40 | 0,
    lowdist: 0,
    titledist: -40 | 0
  };
}();
var rule204 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -34 | 0,
    lowdist: 0,
    titledist: -34 | 0
  };
}();
var rule21 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 121,
    lowdist: 0,
    titledist: 121
  };
}();
var rule23 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -1 | 0,
    lowdist: 0,
    titledist: -1 | 0
  };
}();
var rule25 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -232 | 0,
    lowdist: 0,
    titledist: -232 | 0
  };
}();
var rule27 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -300 | 0,
    lowdist: 0,
    titledist: -300 | 0
  };
}();
var rule28 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 195,
    lowdist: 0,
    titledist: 195
  };
}();
var rule36 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 97,
    lowdist: 0,
    titledist: 97
  };
}();
var rule39 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 163,
    lowdist: 0,
    titledist: 163
  };
}();
var rule41 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 130,
    lowdist: 0,
    titledist: 130
  };
}();
var rule46 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 56,
    lowdist: 0,
    titledist: 56
  };
}();
var rule49 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -2 | 0,
    lowdist: 0,
    titledist: -1 | 0
  };
}();
var rule50 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -79 | 0,
    lowdist: 0,
    titledist: -79 | 0
  };
}();
var rule57 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10815,
    lowdist: 0,
    titledist: 10815
  };
}();
var rule61 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10783,
    lowdist: 0,
    titledist: 10783
  };
}();
var rule62 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10780,
    lowdist: 0,
    titledist: 10780
  };
}();
var rule63 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10782,
    lowdist: 0,
    titledist: 10782
  };
}();
var rule64 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -210 | 0,
    lowdist: 0,
    titledist: -210 | 0
  };
}();
var rule65 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -206 | 0,
    lowdist: 0,
    titledist: -206 | 0
  };
}();
var rule66 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -205 | 0,
    lowdist: 0,
    titledist: -205 | 0
  };
}();
var rule67 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -202 | 0,
    lowdist: 0,
    titledist: -202 | 0
  };
}();
var rule68 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -203 | 0,
    lowdist: 0,
    titledist: -203 | 0
  };
}();
var rule69 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42319,
    lowdist: 0,
    titledist: 42319
  };
}();
var rule70 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42315,
    lowdist: 0,
    titledist: 42315
  };
}();
var rule71 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -207 | 0,
    lowdist: 0,
    titledist: -207 | 0
  };
}();
var rule72 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42280,
    lowdist: 0,
    titledist: 42280
  };
}();
var rule73 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42308,
    lowdist: 0,
    titledist: 42308
  };
}();
var rule74 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -209 | 0,
    lowdist: 0,
    titledist: -209 | 0
  };
}();
var rule75 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -211 | 0,
    lowdist: 0,
    titledist: -211 | 0
  };
}();
var rule76 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10743,
    lowdist: 0,
    titledist: 10743
  };
}();
var rule77 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42305,
    lowdist: 0,
    titledist: 42305
  };
}();
var rule78 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10749,
    lowdist: 0,
    titledist: 10749
  };
}();
var rule79 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -213 | 0,
    lowdist: 0,
    titledist: -213 | 0
  };
}();
var rule80 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -214 | 0,
    lowdist: 0,
    titledist: -214 | 0
  };
}();
var rule81 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 10727,
    lowdist: 0,
    titledist: 10727
  };
}();
var rule82 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -218 | 0,
    lowdist: 0,
    titledist: -218 | 0
  };
}();
var rule83 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42307,
    lowdist: 0,
    titledist: 42307
  };
}();
var rule84 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42282,
    lowdist: 0,
    titledist: 42282
  };
}();
var rule85 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -69 | 0,
    lowdist: 0,
    titledist: -69 | 0
  };
}();
var rule86 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -217 | 0,
    lowdist: 0,
    titledist: -217 | 0
  };
}();
var rule87 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -71 | 0,
    lowdist: 0,
    titledist: -71 | 0
  };
}();
var rule88 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -219 | 0,
    lowdist: 0,
    titledist: -219 | 0
  };
}();
var rule89 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42261,
    lowdist: 0,
    titledist: 42261
  };
}();
var rule90 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: 42258,
    lowdist: 0,
    titledist: 42258
  };
}();
var rule99 = /* @__PURE__ */ function() {
  return {
    category: gencatLL,
    unicodeCat: NUMCAT_LL.value,
    possible: 1,
    updist: -38 | 0,
    lowdist: 0,
    titledist: -38 | 0
  };
}();
var gencatCS = 134217728;
var rule199 = /* @__PURE__ */ function() {
  return {
    category: gencatCS,
    unicodeCat: NUMCAT_CS.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatCO = 268435456;
var rule200 = /* @__PURE__ */ function() {
  return {
    category: gencatCO,
    unicodeCat: NUMCAT_CO.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatCF = 65536;
var rule16 = /* @__PURE__ */ function() {
  return {
    category: gencatCF,
    unicodeCat: NUMCAT_CF.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var gencatCC = 1;
var rule0 = /* @__PURE__ */ function() {
  return {
    category: gencatCC,
    unicodeCat: NUMCAT_CC.value,
    possible: 0,
    updist: 0,
    lowdist: 0,
    titledist: 0
  };
}();
var convchars = [{
  start: 65,
  length: 26,
  convRule: rule9
}, {
  start: 97,
  length: 26,
  convRule: rule12
}, {
  start: 181,
  length: 1,
  convRule: rule18
}, {
  start: 192,
  length: 23,
  convRule: rule9
}, {
  start: 216,
  length: 7,
  convRule: rule9
}, {
  start: 224,
  length: 23,
  convRule: rule12
}, {
  start: 248,
  length: 7,
  convRule: rule12
}, {
  start: 255,
  length: 1,
  convRule: rule21
}, {
  start: 256,
  length: 1,
  convRule: rule22
}, {
  start: 257,
  length: 1,
  convRule: rule23
}, {
  start: 258,
  length: 1,
  convRule: rule22
}, {
  start: 259,
  length: 1,
  convRule: rule23
}, {
  start: 260,
  length: 1,
  convRule: rule22
}, {
  start: 261,
  length: 1,
  convRule: rule23
}, {
  start: 262,
  length: 1,
  convRule: rule22
}, {
  start: 263,
  length: 1,
  convRule: rule23
}, {
  start: 264,
  length: 1,
  convRule: rule22
}, {
  start: 265,
  length: 1,
  convRule: rule23
}, {
  start: 266,
  length: 1,
  convRule: rule22
}, {
  start: 267,
  length: 1,
  convRule: rule23
}, {
  start: 268,
  length: 1,
  convRule: rule22
}, {
  start: 269,
  length: 1,
  convRule: rule23
}, {
  start: 270,
  length: 1,
  convRule: rule22
}, {
  start: 271,
  length: 1,
  convRule: rule23
}, {
  start: 272,
  length: 1,
  convRule: rule22
}, {
  start: 273,
  length: 1,
  convRule: rule23
}, {
  start: 274,
  length: 1,
  convRule: rule22
}, {
  start: 275,
  length: 1,
  convRule: rule23
}, {
  start: 276,
  length: 1,
  convRule: rule22
}, {
  start: 277,
  length: 1,
  convRule: rule23
}, {
  start: 278,
  length: 1,
  convRule: rule22
}, {
  start: 279,
  length: 1,
  convRule: rule23
}, {
  start: 280,
  length: 1,
  convRule: rule22
}, {
  start: 281,
  length: 1,
  convRule: rule23
}, {
  start: 282,
  length: 1,
  convRule: rule22
}, {
  start: 283,
  length: 1,
  convRule: rule23
}, {
  start: 284,
  length: 1,
  convRule: rule22
}, {
  start: 285,
  length: 1,
  convRule: rule23
}, {
  start: 286,
  length: 1,
  convRule: rule22
}, {
  start: 287,
  length: 1,
  convRule: rule23
}, {
  start: 288,
  length: 1,
  convRule: rule22
}, {
  start: 289,
  length: 1,
  convRule: rule23
}, {
  start: 290,
  length: 1,
  convRule: rule22
}, {
  start: 291,
  length: 1,
  convRule: rule23
}, {
  start: 292,
  length: 1,
  convRule: rule22
}, {
  start: 293,
  length: 1,
  convRule: rule23
}, {
  start: 294,
  length: 1,
  convRule: rule22
}, {
  start: 295,
  length: 1,
  convRule: rule23
}, {
  start: 296,
  length: 1,
  convRule: rule22
}, {
  start: 297,
  length: 1,
  convRule: rule23
}, {
  start: 298,
  length: 1,
  convRule: rule22
}, {
  start: 299,
  length: 1,
  convRule: rule23
}, {
  start: 300,
  length: 1,
  convRule: rule22
}, {
  start: 301,
  length: 1,
  convRule: rule23
}, {
  start: 302,
  length: 1,
  convRule: rule22
}, {
  start: 303,
  length: 1,
  convRule: rule23
}, {
  start: 304,
  length: 1,
  convRule: rule24
}, {
  start: 305,
  length: 1,
  convRule: rule25
}, {
  start: 306,
  length: 1,
  convRule: rule22
}, {
  start: 307,
  length: 1,
  convRule: rule23
}, {
  start: 308,
  length: 1,
  convRule: rule22
}, {
  start: 309,
  length: 1,
  convRule: rule23
}, {
  start: 310,
  length: 1,
  convRule: rule22
}, {
  start: 311,
  length: 1,
  convRule: rule23
}, {
  start: 313,
  length: 1,
  convRule: rule22
}, {
  start: 314,
  length: 1,
  convRule: rule23
}, {
  start: 315,
  length: 1,
  convRule: rule22
}, {
  start: 316,
  length: 1,
  convRule: rule23
}, {
  start: 317,
  length: 1,
  convRule: rule22
}, {
  start: 318,
  length: 1,
  convRule: rule23
}, {
  start: 319,
  length: 1,
  convRule: rule22
}, {
  start: 320,
  length: 1,
  convRule: rule23
}, {
  start: 321,
  length: 1,
  convRule: rule22
}, {
  start: 322,
  length: 1,
  convRule: rule23
}, {
  start: 323,
  length: 1,
  convRule: rule22
}, {
  start: 324,
  length: 1,
  convRule: rule23
}, {
  start: 325,
  length: 1,
  convRule: rule22
}, {
  start: 326,
  length: 1,
  convRule: rule23
}, {
  start: 327,
  length: 1,
  convRule: rule22
}, {
  start: 328,
  length: 1,
  convRule: rule23
}, {
  start: 330,
  length: 1,
  convRule: rule22
}, {
  start: 331,
  length: 1,
  convRule: rule23
}, {
  start: 332,
  length: 1,
  convRule: rule22
}, {
  start: 333,
  length: 1,
  convRule: rule23
}, {
  start: 334,
  length: 1,
  convRule: rule22
}, {
  start: 335,
  length: 1,
  convRule: rule23
}, {
  start: 336,
  length: 1,
  convRule: rule22
}, {
  start: 337,
  length: 1,
  convRule: rule23
}, {
  start: 338,
  length: 1,
  convRule: rule22
}, {
  start: 339,
  length: 1,
  convRule: rule23
}, {
  start: 340,
  length: 1,
  convRule: rule22
}, {
  start: 341,
  length: 1,
  convRule: rule23
}, {
  start: 342,
  length: 1,
  convRule: rule22
}, {
  start: 343,
  length: 1,
  convRule: rule23
}, {
  start: 344,
  length: 1,
  convRule: rule22
}, {
  start: 345,
  length: 1,
  convRule: rule23
}, {
  start: 346,
  length: 1,
  convRule: rule22
}, {
  start: 347,
  length: 1,
  convRule: rule23
}, {
  start: 348,
  length: 1,
  convRule: rule22
}, {
  start: 349,
  length: 1,
  convRule: rule23
}, {
  start: 350,
  length: 1,
  convRule: rule22
}, {
  start: 351,
  length: 1,
  convRule: rule23
}, {
  start: 352,
  length: 1,
  convRule: rule22
}, {
  start: 353,
  length: 1,
  convRule: rule23
}, {
  start: 354,
  length: 1,
  convRule: rule22
}, {
  start: 355,
  length: 1,
  convRule: rule23
}, {
  start: 356,
  length: 1,
  convRule: rule22
}, {
  start: 357,
  length: 1,
  convRule: rule23
}, {
  start: 358,
  length: 1,
  convRule: rule22
}, {
  start: 359,
  length: 1,
  convRule: rule23
}, {
  start: 360,
  length: 1,
  convRule: rule22
}, {
  start: 361,
  length: 1,
  convRule: rule23
}, {
  start: 362,
  length: 1,
  convRule: rule22
}, {
  start: 363,
  length: 1,
  convRule: rule23
}, {
  start: 364,
  length: 1,
  convRule: rule22
}, {
  start: 365,
  length: 1,
  convRule: rule23
}, {
  start: 366,
  length: 1,
  convRule: rule22
}, {
  start: 367,
  length: 1,
  convRule: rule23
}, {
  start: 368,
  length: 1,
  convRule: rule22
}, {
  start: 369,
  length: 1,
  convRule: rule23
}, {
  start: 370,
  length: 1,
  convRule: rule22
}, {
  start: 371,
  length: 1,
  convRule: rule23
}, {
  start: 372,
  length: 1,
  convRule: rule22
}, {
  start: 373,
  length: 1,
  convRule: rule23
}, {
  start: 374,
  length: 1,
  convRule: rule22
}, {
  start: 375,
  length: 1,
  convRule: rule23
}, {
  start: 376,
  length: 1,
  convRule: rule26
}, {
  start: 377,
  length: 1,
  convRule: rule22
}, {
  start: 378,
  length: 1,
  convRule: rule23
}, {
  start: 379,
  length: 1,
  convRule: rule22
}, {
  start: 380,
  length: 1,
  convRule: rule23
}, {
  start: 381,
  length: 1,
  convRule: rule22
}, {
  start: 382,
  length: 1,
  convRule: rule23
}, {
  start: 383,
  length: 1,
  convRule: rule27
}, {
  start: 384,
  length: 1,
  convRule: rule28
}, {
  start: 385,
  length: 1,
  convRule: rule29
}, {
  start: 386,
  length: 1,
  convRule: rule22
}, {
  start: 387,
  length: 1,
  convRule: rule23
}, {
  start: 388,
  length: 1,
  convRule: rule22
}, {
  start: 389,
  length: 1,
  convRule: rule23
}, {
  start: 390,
  length: 1,
  convRule: rule30
}, {
  start: 391,
  length: 1,
  convRule: rule22
}, {
  start: 392,
  length: 1,
  convRule: rule23
}, {
  start: 393,
  length: 2,
  convRule: rule31
}, {
  start: 395,
  length: 1,
  convRule: rule22
}, {
  start: 396,
  length: 1,
  convRule: rule23
}, {
  start: 398,
  length: 1,
  convRule: rule32
}, {
  start: 399,
  length: 1,
  convRule: rule33
}, {
  start: 400,
  length: 1,
  convRule: rule34
}, {
  start: 401,
  length: 1,
  convRule: rule22
}, {
  start: 402,
  length: 1,
  convRule: rule23
}, {
  start: 403,
  length: 1,
  convRule: rule31
}, {
  start: 404,
  length: 1,
  convRule: rule35
}, {
  start: 405,
  length: 1,
  convRule: rule36
}, {
  start: 406,
  length: 1,
  convRule: rule37
}, {
  start: 407,
  length: 1,
  convRule: rule38
}, {
  start: 408,
  length: 1,
  convRule: rule22
}, {
  start: 409,
  length: 1,
  convRule: rule23
}, {
  start: 410,
  length: 1,
  convRule: rule39
}, {
  start: 412,
  length: 1,
  convRule: rule37
}, {
  start: 413,
  length: 1,
  convRule: rule40
}, {
  start: 414,
  length: 1,
  convRule: rule41
}, {
  start: 415,
  length: 1,
  convRule: rule42
}, {
  start: 416,
  length: 1,
  convRule: rule22
}, {
  start: 417,
  length: 1,
  convRule: rule23
}, {
  start: 418,
  length: 1,
  convRule: rule22
}, {
  start: 419,
  length: 1,
  convRule: rule23
}, {
  start: 420,
  length: 1,
  convRule: rule22
}, {
  start: 421,
  length: 1,
  convRule: rule23
}, {
  start: 422,
  length: 1,
  convRule: rule43
}, {
  start: 423,
  length: 1,
  convRule: rule22
}, {
  start: 424,
  length: 1,
  convRule: rule23
}, {
  start: 425,
  length: 1,
  convRule: rule43
}, {
  start: 428,
  length: 1,
  convRule: rule22
}, {
  start: 429,
  length: 1,
  convRule: rule23
}, {
  start: 430,
  length: 1,
  convRule: rule43
}, {
  start: 431,
  length: 1,
  convRule: rule22
}, {
  start: 432,
  length: 1,
  convRule: rule23
}, {
  start: 433,
  length: 2,
  convRule: rule44
}, {
  start: 435,
  length: 1,
  convRule: rule22
}, {
  start: 436,
  length: 1,
  convRule: rule23
}, {
  start: 437,
  length: 1,
  convRule: rule22
}, {
  start: 438,
  length: 1,
  convRule: rule23
}, {
  start: 439,
  length: 1,
  convRule: rule45
}, {
  start: 440,
  length: 1,
  convRule: rule22
}, {
  start: 441,
  length: 1,
  convRule: rule23
}, {
  start: 444,
  length: 1,
  convRule: rule22
}, {
  start: 445,
  length: 1,
  convRule: rule23
}, {
  start: 447,
  length: 1,
  convRule: rule46
}, {
  start: 452,
  length: 1,
  convRule: rule47
}, {
  start: 453,
  length: 1,
  convRule: rule48
}, {
  start: 454,
  length: 1,
  convRule: rule49
}, {
  start: 455,
  length: 1,
  convRule: rule47
}, {
  start: 456,
  length: 1,
  convRule: rule48
}, {
  start: 457,
  length: 1,
  convRule: rule49
}, {
  start: 458,
  length: 1,
  convRule: rule47
}, {
  start: 459,
  length: 1,
  convRule: rule48
}, {
  start: 460,
  length: 1,
  convRule: rule49
}, {
  start: 461,
  length: 1,
  convRule: rule22
}, {
  start: 462,
  length: 1,
  convRule: rule23
}, {
  start: 463,
  length: 1,
  convRule: rule22
}, {
  start: 464,
  length: 1,
  convRule: rule23
}, {
  start: 465,
  length: 1,
  convRule: rule22
}, {
  start: 466,
  length: 1,
  convRule: rule23
}, {
  start: 467,
  length: 1,
  convRule: rule22
}, {
  start: 468,
  length: 1,
  convRule: rule23
}, {
  start: 469,
  length: 1,
  convRule: rule22
}, {
  start: 470,
  length: 1,
  convRule: rule23
}, {
  start: 471,
  length: 1,
  convRule: rule22
}, {
  start: 472,
  length: 1,
  convRule: rule23
}, {
  start: 473,
  length: 1,
  convRule: rule22
}, {
  start: 474,
  length: 1,
  convRule: rule23
}, {
  start: 475,
  length: 1,
  convRule: rule22
}, {
  start: 476,
  length: 1,
  convRule: rule23
}, {
  start: 477,
  length: 1,
  convRule: rule50
}, {
  start: 478,
  length: 1,
  convRule: rule22
}, {
  start: 479,
  length: 1,
  convRule: rule23
}, {
  start: 480,
  length: 1,
  convRule: rule22
}, {
  start: 481,
  length: 1,
  convRule: rule23
}, {
  start: 482,
  length: 1,
  convRule: rule22
}, {
  start: 483,
  length: 1,
  convRule: rule23
}, {
  start: 484,
  length: 1,
  convRule: rule22
}, {
  start: 485,
  length: 1,
  convRule: rule23
}, {
  start: 486,
  length: 1,
  convRule: rule22
}, {
  start: 487,
  length: 1,
  convRule: rule23
}, {
  start: 488,
  length: 1,
  convRule: rule22
}, {
  start: 489,
  length: 1,
  convRule: rule23
}, {
  start: 490,
  length: 1,
  convRule: rule22
}, {
  start: 491,
  length: 1,
  convRule: rule23
}, {
  start: 492,
  length: 1,
  convRule: rule22
}, {
  start: 493,
  length: 1,
  convRule: rule23
}, {
  start: 494,
  length: 1,
  convRule: rule22
}, {
  start: 495,
  length: 1,
  convRule: rule23
}, {
  start: 497,
  length: 1,
  convRule: rule47
}, {
  start: 498,
  length: 1,
  convRule: rule48
}, {
  start: 499,
  length: 1,
  convRule: rule49
}, {
  start: 500,
  length: 1,
  convRule: rule22
}, {
  start: 501,
  length: 1,
  convRule: rule23
}, {
  start: 502,
  length: 1,
  convRule: rule51
}, {
  start: 503,
  length: 1,
  convRule: rule52
}, {
  start: 504,
  length: 1,
  convRule: rule22
}, {
  start: 505,
  length: 1,
  convRule: rule23
}, {
  start: 506,
  length: 1,
  convRule: rule22
}, {
  start: 507,
  length: 1,
  convRule: rule23
}, {
  start: 508,
  length: 1,
  convRule: rule22
}, {
  start: 509,
  length: 1,
  convRule: rule23
}, {
  start: 510,
  length: 1,
  convRule: rule22
}, {
  start: 511,
  length: 1,
  convRule: rule23
}, {
  start: 512,
  length: 1,
  convRule: rule22
}, {
  start: 513,
  length: 1,
  convRule: rule23
}, {
  start: 514,
  length: 1,
  convRule: rule22
}, {
  start: 515,
  length: 1,
  convRule: rule23
}, {
  start: 516,
  length: 1,
  convRule: rule22
}, {
  start: 517,
  length: 1,
  convRule: rule23
}, {
  start: 518,
  length: 1,
  convRule: rule22
}, {
  start: 519,
  length: 1,
  convRule: rule23
}, {
  start: 520,
  length: 1,
  convRule: rule22
}, {
  start: 521,
  length: 1,
  convRule: rule23
}, {
  start: 522,
  length: 1,
  convRule: rule22
}, {
  start: 523,
  length: 1,
  convRule: rule23
}, {
  start: 524,
  length: 1,
  convRule: rule22
}, {
  start: 525,
  length: 1,
  convRule: rule23
}, {
  start: 526,
  length: 1,
  convRule: rule22
}, {
  start: 527,
  length: 1,
  convRule: rule23
}, {
  start: 528,
  length: 1,
  convRule: rule22
}, {
  start: 529,
  length: 1,
  convRule: rule23
}, {
  start: 530,
  length: 1,
  convRule: rule22
}, {
  start: 531,
  length: 1,
  convRule: rule23
}, {
  start: 532,
  length: 1,
  convRule: rule22
}, {
  start: 533,
  length: 1,
  convRule: rule23
}, {
  start: 534,
  length: 1,
  convRule: rule22
}, {
  start: 535,
  length: 1,
  convRule: rule23
}, {
  start: 536,
  length: 1,
  convRule: rule22
}, {
  start: 537,
  length: 1,
  convRule: rule23
}, {
  start: 538,
  length: 1,
  convRule: rule22
}, {
  start: 539,
  length: 1,
  convRule: rule23
}, {
  start: 540,
  length: 1,
  convRule: rule22
}, {
  start: 541,
  length: 1,
  convRule: rule23
}, {
  start: 542,
  length: 1,
  convRule: rule22
}, {
  start: 543,
  length: 1,
  convRule: rule23
}, {
  start: 544,
  length: 1,
  convRule: rule53
}, {
  start: 546,
  length: 1,
  convRule: rule22
}, {
  start: 547,
  length: 1,
  convRule: rule23
}, {
  start: 548,
  length: 1,
  convRule: rule22
}, {
  start: 549,
  length: 1,
  convRule: rule23
}, {
  start: 550,
  length: 1,
  convRule: rule22
}, {
  start: 551,
  length: 1,
  convRule: rule23
}, {
  start: 552,
  length: 1,
  convRule: rule22
}, {
  start: 553,
  length: 1,
  convRule: rule23
}, {
  start: 554,
  length: 1,
  convRule: rule22
}, {
  start: 555,
  length: 1,
  convRule: rule23
}, {
  start: 556,
  length: 1,
  convRule: rule22
}, {
  start: 557,
  length: 1,
  convRule: rule23
}, {
  start: 558,
  length: 1,
  convRule: rule22
}, {
  start: 559,
  length: 1,
  convRule: rule23
}, {
  start: 560,
  length: 1,
  convRule: rule22
}, {
  start: 561,
  length: 1,
  convRule: rule23
}, {
  start: 562,
  length: 1,
  convRule: rule22
}, {
  start: 563,
  length: 1,
  convRule: rule23
}, {
  start: 570,
  length: 1,
  convRule: rule54
}, {
  start: 571,
  length: 1,
  convRule: rule22
}, {
  start: 572,
  length: 1,
  convRule: rule23
}, {
  start: 573,
  length: 1,
  convRule: rule55
}, {
  start: 574,
  length: 1,
  convRule: rule56
}, {
  start: 575,
  length: 2,
  convRule: rule57
}, {
  start: 577,
  length: 1,
  convRule: rule22
}, {
  start: 578,
  length: 1,
  convRule: rule23
}, {
  start: 579,
  length: 1,
  convRule: rule58
}, {
  start: 580,
  length: 1,
  convRule: rule59
}, {
  start: 581,
  length: 1,
  convRule: rule60
}, {
  start: 582,
  length: 1,
  convRule: rule22
}, {
  start: 583,
  length: 1,
  convRule: rule23
}, {
  start: 584,
  length: 1,
  convRule: rule22
}, {
  start: 585,
  length: 1,
  convRule: rule23
}, {
  start: 586,
  length: 1,
  convRule: rule22
}, {
  start: 587,
  length: 1,
  convRule: rule23
}, {
  start: 588,
  length: 1,
  convRule: rule22
}, {
  start: 589,
  length: 1,
  convRule: rule23
}, {
  start: 590,
  length: 1,
  convRule: rule22
}, {
  start: 591,
  length: 1,
  convRule: rule23
}, {
  start: 592,
  length: 1,
  convRule: rule61
}, {
  start: 593,
  length: 1,
  convRule: rule62
}, {
  start: 594,
  length: 1,
  convRule: rule63
}, {
  start: 595,
  length: 1,
  convRule: rule64
}, {
  start: 596,
  length: 1,
  convRule: rule65
}, {
  start: 598,
  length: 2,
  convRule: rule66
}, {
  start: 601,
  length: 1,
  convRule: rule67
}, {
  start: 603,
  length: 1,
  convRule: rule68
}, {
  start: 604,
  length: 1,
  convRule: rule69
}, {
  start: 608,
  length: 1,
  convRule: rule66
}, {
  start: 609,
  length: 1,
  convRule: rule70
}, {
  start: 611,
  length: 1,
  convRule: rule71
}, {
  start: 613,
  length: 1,
  convRule: rule72
}, {
  start: 614,
  length: 1,
  convRule: rule73
}, {
  start: 616,
  length: 1,
  convRule: rule74
}, {
  start: 617,
  length: 1,
  convRule: rule75
}, {
  start: 618,
  length: 1,
  convRule: rule73
}, {
  start: 619,
  length: 1,
  convRule: rule76
}, {
  start: 620,
  length: 1,
  convRule: rule77
}, {
  start: 623,
  length: 1,
  convRule: rule75
}, {
  start: 625,
  length: 1,
  convRule: rule78
}, {
  start: 626,
  length: 1,
  convRule: rule79
}, {
  start: 629,
  length: 1,
  convRule: rule80
}, {
  start: 637,
  length: 1,
  convRule: rule81
}, {
  start: 640,
  length: 1,
  convRule: rule82
}, {
  start: 642,
  length: 1,
  convRule: rule83
}, {
  start: 643,
  length: 1,
  convRule: rule82
}, {
  start: 647,
  length: 1,
  convRule: rule84
}, {
  start: 648,
  length: 1,
  convRule: rule82
}, {
  start: 649,
  length: 1,
  convRule: rule85
}, {
  start: 650,
  length: 2,
  convRule: rule86
}, {
  start: 652,
  length: 1,
  convRule: rule87
}, {
  start: 658,
  length: 1,
  convRule: rule88
}, {
  start: 669,
  length: 1,
  convRule: rule89
}, {
  start: 670,
  length: 1,
  convRule: rule90
}, {
  start: 837,
  length: 1,
  convRule: rule93
}, {
  start: 880,
  length: 1,
  convRule: rule22
}, {
  start: 881,
  length: 1,
  convRule: rule23
}, {
  start: 882,
  length: 1,
  convRule: rule22
}, {
  start: 883,
  length: 1,
  convRule: rule23
}, {
  start: 886,
  length: 1,
  convRule: rule22
}, {
  start: 887,
  length: 1,
  convRule: rule23
}, {
  start: 891,
  length: 3,
  convRule: rule41
}, {
  start: 895,
  length: 1,
  convRule: rule94
}, {
  start: 902,
  length: 1,
  convRule: rule95
}, {
  start: 904,
  length: 3,
  convRule: rule96
}, {
  start: 908,
  length: 1,
  convRule: rule97
}, {
  start: 910,
  length: 2,
  convRule: rule98
}, {
  start: 913,
  length: 17,
  convRule: rule9
}, {
  start: 931,
  length: 9,
  convRule: rule9
}, {
  start: 940,
  length: 1,
  convRule: rule99
}, {
  start: 941,
  length: 3,
  convRule: rule100
}, {
  start: 945,
  length: 17,
  convRule: rule12
}, {
  start: 962,
  length: 1,
  convRule: rule101
}, {
  start: 963,
  length: 9,
  convRule: rule12
}, {
  start: 972,
  length: 1,
  convRule: rule102
}, {
  start: 973,
  length: 2,
  convRule: rule103
}, {
  start: 975,
  length: 1,
  convRule: rule104
}, {
  start: 976,
  length: 1,
  convRule: rule105
}, {
  start: 977,
  length: 1,
  convRule: rule106
}, {
  start: 981,
  length: 1,
  convRule: rule108
}, {
  start: 982,
  length: 1,
  convRule: rule109
}, {
  start: 983,
  length: 1,
  convRule: rule110
}, {
  start: 984,
  length: 1,
  convRule: rule22
}, {
  start: 985,
  length: 1,
  convRule: rule23
}, {
  start: 986,
  length: 1,
  convRule: rule22
}, {
  start: 987,
  length: 1,
  convRule: rule23
}, {
  start: 988,
  length: 1,
  convRule: rule22
}, {
  start: 989,
  length: 1,
  convRule: rule23
}, {
  start: 990,
  length: 1,
  convRule: rule22
}, {
  start: 991,
  length: 1,
  convRule: rule23
}, {
  start: 992,
  length: 1,
  convRule: rule22
}, {
  start: 993,
  length: 1,
  convRule: rule23
}, {
  start: 994,
  length: 1,
  convRule: rule22
}, {
  start: 995,
  length: 1,
  convRule: rule23
}, {
  start: 996,
  length: 1,
  convRule: rule22
}, {
  start: 997,
  length: 1,
  convRule: rule23
}, {
  start: 998,
  length: 1,
  convRule: rule22
}, {
  start: 999,
  length: 1,
  convRule: rule23
}, {
  start: 1e3,
  length: 1,
  convRule: rule22
}, {
  start: 1001,
  length: 1,
  convRule: rule23
}, {
  start: 1002,
  length: 1,
  convRule: rule22
}, {
  start: 1003,
  length: 1,
  convRule: rule23
}, {
  start: 1004,
  length: 1,
  convRule: rule22
}, {
  start: 1005,
  length: 1,
  convRule: rule23
}, {
  start: 1006,
  length: 1,
  convRule: rule22
}, {
  start: 1007,
  length: 1,
  convRule: rule23
}, {
  start: 1008,
  length: 1,
  convRule: rule111
}, {
  start: 1009,
  length: 1,
  convRule: rule112
}, {
  start: 1010,
  length: 1,
  convRule: rule113
}, {
  start: 1011,
  length: 1,
  convRule: rule114
}, {
  start: 1012,
  length: 1,
  convRule: rule115
}, {
  start: 1013,
  length: 1,
  convRule: rule116
}, {
  start: 1015,
  length: 1,
  convRule: rule22
}, {
  start: 1016,
  length: 1,
  convRule: rule23
}, {
  start: 1017,
  length: 1,
  convRule: rule117
}, {
  start: 1018,
  length: 1,
  convRule: rule22
}, {
  start: 1019,
  length: 1,
  convRule: rule23
}, {
  start: 1021,
  length: 3,
  convRule: rule53
}, {
  start: 1024,
  length: 16,
  convRule: rule118
}, {
  start: 1040,
  length: 32,
  convRule: rule9
}, {
  start: 1072,
  length: 32,
  convRule: rule12
}, {
  start: 1104,
  length: 16,
  convRule: rule112
}, {
  start: 1120,
  length: 1,
  convRule: rule22
}, {
  start: 1121,
  length: 1,
  convRule: rule23
}, {
  start: 1122,
  length: 1,
  convRule: rule22
}, {
  start: 1123,
  length: 1,
  convRule: rule23
}, {
  start: 1124,
  length: 1,
  convRule: rule22
}, {
  start: 1125,
  length: 1,
  convRule: rule23
}, {
  start: 1126,
  length: 1,
  convRule: rule22
}, {
  start: 1127,
  length: 1,
  convRule: rule23
}, {
  start: 1128,
  length: 1,
  convRule: rule22
}, {
  start: 1129,
  length: 1,
  convRule: rule23
}, {
  start: 1130,
  length: 1,
  convRule: rule22
}, {
  start: 1131,
  length: 1,
  convRule: rule23
}, {
  start: 1132,
  length: 1,
  convRule: rule22
}, {
  start: 1133,
  length: 1,
  convRule: rule23
}, {
  start: 1134,
  length: 1,
  convRule: rule22
}, {
  start: 1135,
  length: 1,
  convRule: rule23
}, {
  start: 1136,
  length: 1,
  convRule: rule22
}, {
  start: 1137,
  length: 1,
  convRule: rule23
}, {
  start: 1138,
  length: 1,
  convRule: rule22
}, {
  start: 1139,
  length: 1,
  convRule: rule23
}, {
  start: 1140,
  length: 1,
  convRule: rule22
}, {
  start: 1141,
  length: 1,
  convRule: rule23
}, {
  start: 1142,
  length: 1,
  convRule: rule22
}, {
  start: 1143,
  length: 1,
  convRule: rule23
}, {
  start: 1144,
  length: 1,
  convRule: rule22
}, {
  start: 1145,
  length: 1,
  convRule: rule23
}, {
  start: 1146,
  length: 1,
  convRule: rule22
}, {
  start: 1147,
  length: 1,
  convRule: rule23
}, {
  start: 1148,
  length: 1,
  convRule: rule22
}, {
  start: 1149,
  length: 1,
  convRule: rule23
}, {
  start: 1150,
  length: 1,
  convRule: rule22
}, {
  start: 1151,
  length: 1,
  convRule: rule23
}, {
  start: 1152,
  length: 1,
  convRule: rule22
}, {
  start: 1153,
  length: 1,
  convRule: rule23
}, {
  start: 1162,
  length: 1,
  convRule: rule22
}, {
  start: 1163,
  length: 1,
  convRule: rule23
}, {
  start: 1164,
  length: 1,
  convRule: rule22
}, {
  start: 1165,
  length: 1,
  convRule: rule23
}, {
  start: 1166,
  length: 1,
  convRule: rule22
}, {
  start: 1167,
  length: 1,
  convRule: rule23
}, {
  start: 1168,
  length: 1,
  convRule: rule22
}, {
  start: 1169,
  length: 1,
  convRule: rule23
}, {
  start: 1170,
  length: 1,
  convRule: rule22
}, {
  start: 1171,
  length: 1,
  convRule: rule23
}, {
  start: 1172,
  length: 1,
  convRule: rule22
}, {
  start: 1173,
  length: 1,
  convRule: rule23
}, {
  start: 1174,
  length: 1,
  convRule: rule22
}, {
  start: 1175,
  length: 1,
  convRule: rule23
}, {
  start: 1176,
  length: 1,
  convRule: rule22
}, {
  start: 1177,
  length: 1,
  convRule: rule23
}, {
  start: 1178,
  length: 1,
  convRule: rule22
}, {
  start: 1179,
  length: 1,
  convRule: rule23
}, {
  start: 1180,
  length: 1,
  convRule: rule22
}, {
  start: 1181,
  length: 1,
  convRule: rule23
}, {
  start: 1182,
  length: 1,
  convRule: rule22
}, {
  start: 1183,
  length: 1,
  convRule: rule23
}, {
  start: 1184,
  length: 1,
  convRule: rule22
}, {
  start: 1185,
  length: 1,
  convRule: rule23
}, {
  start: 1186,
  length: 1,
  convRule: rule22
}, {
  start: 1187,
  length: 1,
  convRule: rule23
}, {
  start: 1188,
  length: 1,
  convRule: rule22
}, {
  start: 1189,
  length: 1,
  convRule: rule23
}, {
  start: 1190,
  length: 1,
  convRule: rule22
}, {
  start: 1191,
  length: 1,
  convRule: rule23
}, {
  start: 1192,
  length: 1,
  convRule: rule22
}, {
  start: 1193,
  length: 1,
  convRule: rule23
}, {
  start: 1194,
  length: 1,
  convRule: rule22
}, {
  start: 1195,
  length: 1,
  convRule: rule23
}, {
  start: 1196,
  length: 1,
  convRule: rule22
}, {
  start: 1197,
  length: 1,
  convRule: rule23
}, {
  start: 1198,
  length: 1,
  convRule: rule22
}, {
  start: 1199,
  length: 1,
  convRule: rule23
}, {
  start: 1200,
  length: 1,
  convRule: rule22
}, {
  start: 1201,
  length: 1,
  convRule: rule23
}, {
  start: 1202,
  length: 1,
  convRule: rule22
}, {
  start: 1203,
  length: 1,
  convRule: rule23
}, {
  start: 1204,
  length: 1,
  convRule: rule22
}, {
  start: 1205,
  length: 1,
  convRule: rule23
}, {
  start: 1206,
  length: 1,
  convRule: rule22
}, {
  start: 1207,
  length: 1,
  convRule: rule23
}, {
  start: 1208,
  length: 1,
  convRule: rule22
}, {
  start: 1209,
  length: 1,
  convRule: rule23
}, {
  start: 1210,
  length: 1,
  convRule: rule22
}, {
  start: 1211,
  length: 1,
  convRule: rule23
}, {
  start: 1212,
  length: 1,
  convRule: rule22
}, {
  start: 1213,
  length: 1,
  convRule: rule23
}, {
  start: 1214,
  length: 1,
  convRule: rule22
}, {
  start: 1215,
  length: 1,
  convRule: rule23
}, {
  start: 1216,
  length: 1,
  convRule: rule120
}, {
  start: 1217,
  length: 1,
  convRule: rule22
}, {
  start: 1218,
  length: 1,
  convRule: rule23
}, {
  start: 1219,
  length: 1,
  convRule: rule22
}, {
  start: 1220,
  length: 1,
  convRule: rule23
}, {
  start: 1221,
  length: 1,
  convRule: rule22
}, {
  start: 1222,
  length: 1,
  convRule: rule23
}, {
  start: 1223,
  length: 1,
  convRule: rule22
}, {
  start: 1224,
  length: 1,
  convRule: rule23
}, {
  start: 1225,
  length: 1,
  convRule: rule22
}, {
  start: 1226,
  length: 1,
  convRule: rule23
}, {
  start: 1227,
  length: 1,
  convRule: rule22
}, {
  start: 1228,
  length: 1,
  convRule: rule23
}, {
  start: 1229,
  length: 1,
  convRule: rule22
}, {
  start: 1230,
  length: 1,
  convRule: rule23
}, {
  start: 1231,
  length: 1,
  convRule: rule121
}, {
  start: 1232,
  length: 1,
  convRule: rule22
}, {
  start: 1233,
  length: 1,
  convRule: rule23
}, {
  start: 1234,
  length: 1,
  convRule: rule22
}, {
  start: 1235,
  length: 1,
  convRule: rule23
}, {
  start: 1236,
  length: 1,
  convRule: rule22
}, {
  start: 1237,
  length: 1,
  convRule: rule23
}, {
  start: 1238,
  length: 1,
  convRule: rule22
}, {
  start: 1239,
  length: 1,
  convRule: rule23
}, {
  start: 1240,
  length: 1,
  convRule: rule22
}, {
  start: 1241,
  length: 1,
  convRule: rule23
}, {
  start: 1242,
  length: 1,
  convRule: rule22
}, {
  start: 1243,
  length: 1,
  convRule: rule23
}, {
  start: 1244,
  length: 1,
  convRule: rule22
}, {
  start: 1245,
  length: 1,
  convRule: rule23
}, {
  start: 1246,
  length: 1,
  convRule: rule22
}, {
  start: 1247,
  length: 1,
  convRule: rule23
}, {
  start: 1248,
  length: 1,
  convRule: rule22
}, {
  start: 1249,
  length: 1,
  convRule: rule23
}, {
  start: 1250,
  length: 1,
  convRule: rule22
}, {
  start: 1251,
  length: 1,
  convRule: rule23
}, {
  start: 1252,
  length: 1,
  convRule: rule22
}, {
  start: 1253,
  length: 1,
  convRule: rule23
}, {
  start: 1254,
  length: 1,
  convRule: rule22
}, {
  start: 1255,
  length: 1,
  convRule: rule23
}, {
  start: 1256,
  length: 1,
  convRule: rule22
}, {
  start: 1257,
  length: 1,
  convRule: rule23
}, {
  start: 1258,
  length: 1,
  convRule: rule22
}, {
  start: 1259,
  length: 1,
  convRule: rule23
}, {
  start: 1260,
  length: 1,
  convRule: rule22
}, {
  start: 1261,
  length: 1,
  convRule: rule23
}, {
  start: 1262,
  length: 1,
  convRule: rule22
}, {
  start: 1263,
  length: 1,
  convRule: rule23
}, {
  start: 1264,
  length: 1,
  convRule: rule22
}, {
  start: 1265,
  length: 1,
  convRule: rule23
}, {
  start: 1266,
  length: 1,
  convRule: rule22
}, {
  start: 1267,
  length: 1,
  convRule: rule23
}, {
  start: 1268,
  length: 1,
  convRule: rule22
}, {
  start: 1269,
  length: 1,
  convRule: rule23
}, {
  start: 1270,
  length: 1,
  convRule: rule22
}, {
  start: 1271,
  length: 1,
  convRule: rule23
}, {
  start: 1272,
  length: 1,
  convRule: rule22
}, {
  start: 1273,
  length: 1,
  convRule: rule23
}, {
  start: 1274,
  length: 1,
  convRule: rule22
}, {
  start: 1275,
  length: 1,
  convRule: rule23
}, {
  start: 1276,
  length: 1,
  convRule: rule22
}, {
  start: 1277,
  length: 1,
  convRule: rule23
}, {
  start: 1278,
  length: 1,
  convRule: rule22
}, {
  start: 1279,
  length: 1,
  convRule: rule23
}, {
  start: 1280,
  length: 1,
  convRule: rule22
}, {
  start: 1281,
  length: 1,
  convRule: rule23
}, {
  start: 1282,
  length: 1,
  convRule: rule22
}, {
  start: 1283,
  length: 1,
  convRule: rule23
}, {
  start: 1284,
  length: 1,
  convRule: rule22
}, {
  start: 1285,
  length: 1,
  convRule: rule23
}, {
  start: 1286,
  length: 1,
  convRule: rule22
}, {
  start: 1287,
  length: 1,
  convRule: rule23
}, {
  start: 1288,
  length: 1,
  convRule: rule22
}, {
  start: 1289,
  length: 1,
  convRule: rule23
}, {
  start: 1290,
  length: 1,
  convRule: rule22
}, {
  start: 1291,
  length: 1,
  convRule: rule23
}, {
  start: 1292,
  length: 1,
  convRule: rule22
}, {
  start: 1293,
  length: 1,
  convRule: rule23
}, {
  start: 1294,
  length: 1,
  convRule: rule22
}, {
  start: 1295,
  length: 1,
  convRule: rule23
}, {
  start: 1296,
  length: 1,
  convRule: rule22
}, {
  start: 1297,
  length: 1,
  convRule: rule23
}, {
  start: 1298,
  length: 1,
  convRule: rule22
}, {
  start: 1299,
  length: 1,
  convRule: rule23
}, {
  start: 1300,
  length: 1,
  convRule: rule22
}, {
  start: 1301,
  length: 1,
  convRule: rule23
}, {
  start: 1302,
  length: 1,
  convRule: rule22
}, {
  start: 1303,
  length: 1,
  convRule: rule23
}, {
  start: 1304,
  length: 1,
  convRule: rule22
}, {
  start: 1305,
  length: 1,
  convRule: rule23
}, {
  start: 1306,
  length: 1,
  convRule: rule22
}, {
  start: 1307,
  length: 1,
  convRule: rule23
}, {
  start: 1308,
  length: 1,
  convRule: rule22
}, {
  start: 1309,
  length: 1,
  convRule: rule23
}, {
  start: 1310,
  length: 1,
  convRule: rule22
}, {
  start: 1311,
  length: 1,
  convRule: rule23
}, {
  start: 1312,
  length: 1,
  convRule: rule22
}, {
  start: 1313,
  length: 1,
  convRule: rule23
}, {
  start: 1314,
  length: 1,
  convRule: rule22
}, {
  start: 1315,
  length: 1,
  convRule: rule23
}, {
  start: 1316,
  length: 1,
  convRule: rule22
}, {
  start: 1317,
  length: 1,
  convRule: rule23
}, {
  start: 1318,
  length: 1,
  convRule: rule22
}, {
  start: 1319,
  length: 1,
  convRule: rule23
}, {
  start: 1320,
  length: 1,
  convRule: rule22
}, {
  start: 1321,
  length: 1,
  convRule: rule23
}, {
  start: 1322,
  length: 1,
  convRule: rule22
}, {
  start: 1323,
  length: 1,
  convRule: rule23
}, {
  start: 1324,
  length: 1,
  convRule: rule22
}, {
  start: 1325,
  length: 1,
  convRule: rule23
}, {
  start: 1326,
  length: 1,
  convRule: rule22
}, {
  start: 1327,
  length: 1,
  convRule: rule23
}, {
  start: 1329,
  length: 38,
  convRule: rule122
}, {
  start: 1377,
  length: 38,
  convRule: rule123
}, {
  start: 4256,
  length: 38,
  convRule: rule125
}, {
  start: 4295,
  length: 1,
  convRule: rule125
}, {
  start: 4301,
  length: 1,
  convRule: rule125
}, {
  start: 4304,
  length: 43,
  convRule: rule126
}, {
  start: 4349,
  length: 3,
  convRule: rule126
}, {
  start: 5024,
  length: 80,
  convRule: rule127
}, {
  start: 5104,
  length: 6,
  convRule: rule104
}, {
  start: 5112,
  length: 6,
  convRule: rule110
}, {
  start: 7296,
  length: 1,
  convRule: rule129
}, {
  start: 7297,
  length: 1,
  convRule: rule130
}, {
  start: 7298,
  length: 1,
  convRule: rule131
}, {
  start: 7299,
  length: 2,
  convRule: rule132
}, {
  start: 7301,
  length: 1,
  convRule: rule133
}, {
  start: 7302,
  length: 1,
  convRule: rule134
}, {
  start: 7303,
  length: 1,
  convRule: rule135
}, {
  start: 7304,
  length: 1,
  convRule: rule136
}, {
  start: 7312,
  length: 43,
  convRule: rule137
}, {
  start: 7357,
  length: 3,
  convRule: rule137
}, {
  start: 7545,
  length: 1,
  convRule: rule138
}, {
  start: 7549,
  length: 1,
  convRule: rule139
}, {
  start: 7566,
  length: 1,
  convRule: rule140
}, {
  start: 7680,
  length: 1,
  convRule: rule22
}, {
  start: 7681,
  length: 1,
  convRule: rule23
}, {
  start: 7682,
  length: 1,
  convRule: rule22
}, {
  start: 7683,
  length: 1,
  convRule: rule23
}, {
  start: 7684,
  length: 1,
  convRule: rule22
}, {
  start: 7685,
  length: 1,
  convRule: rule23
}, {
  start: 7686,
  length: 1,
  convRule: rule22
}, {
  start: 7687,
  length: 1,
  convRule: rule23
}, {
  start: 7688,
  length: 1,
  convRule: rule22
}, {
  start: 7689,
  length: 1,
  convRule: rule23
}, {
  start: 7690,
  length: 1,
  convRule: rule22
}, {
  start: 7691,
  length: 1,
  convRule: rule23
}, {
  start: 7692,
  length: 1,
  convRule: rule22
}, {
  start: 7693,
  length: 1,
  convRule: rule23
}, {
  start: 7694,
  length: 1,
  convRule: rule22
}, {
  start: 7695,
  length: 1,
  convRule: rule23
}, {
  start: 7696,
  length: 1,
  convRule: rule22
}, {
  start: 7697,
  length: 1,
  convRule: rule23
}, {
  start: 7698,
  length: 1,
  convRule: rule22
}, {
  start: 7699,
  length: 1,
  convRule: rule23
}, {
  start: 7700,
  length: 1,
  convRule: rule22
}, {
  start: 7701,
  length: 1,
  convRule: rule23
}, {
  start: 7702,
  length: 1,
  convRule: rule22
}, {
  start: 7703,
  length: 1,
  convRule: rule23
}, {
  start: 7704,
  length: 1,
  convRule: rule22
}, {
  start: 7705,
  length: 1,
  convRule: rule23
}, {
  start: 7706,
  length: 1,
  convRule: rule22
}, {
  start: 7707,
  length: 1,
  convRule: rule23
}, {
  start: 7708,
  length: 1,
  convRule: rule22
}, {
  start: 7709,
  length: 1,
  convRule: rule23
}, {
  start: 7710,
  length: 1,
  convRule: rule22
}, {
  start: 7711,
  length: 1,
  convRule: rule23
}, {
  start: 7712,
  length: 1,
  convRule: rule22
}, {
  start: 7713,
  length: 1,
  convRule: rule23
}, {
  start: 7714,
  length: 1,
  convRule: rule22
}, {
  start: 7715,
  length: 1,
  convRule: rule23
}, {
  start: 7716,
  length: 1,
  convRule: rule22
}, {
  start: 7717,
  length: 1,
  convRule: rule23
}, {
  start: 7718,
  length: 1,
  convRule: rule22
}, {
  start: 7719,
  length: 1,
  convRule: rule23
}, {
  start: 7720,
  length: 1,
  convRule: rule22
}, {
  start: 7721,
  length: 1,
  convRule: rule23
}, {
  start: 7722,
  length: 1,
  convRule: rule22
}, {
  start: 7723,
  length: 1,
  convRule: rule23
}, {
  start: 7724,
  length: 1,
  convRule: rule22
}, {
  start: 7725,
  length: 1,
  convRule: rule23
}, {
  start: 7726,
  length: 1,
  convRule: rule22
}, {
  start: 7727,
  length: 1,
  convRule: rule23
}, {
  start: 7728,
  length: 1,
  convRule: rule22
}, {
  start: 7729,
  length: 1,
  convRule: rule23
}, {
  start: 7730,
  length: 1,
  convRule: rule22
}, {
  start: 7731,
  length: 1,
  convRule: rule23
}, {
  start: 7732,
  length: 1,
  convRule: rule22
}, {
  start: 7733,
  length: 1,
  convRule: rule23
}, {
  start: 7734,
  length: 1,
  convRule: rule22
}, {
  start: 7735,
  length: 1,
  convRule: rule23
}, {
  start: 7736,
  length: 1,
  convRule: rule22
}, {
  start: 7737,
  length: 1,
  convRule: rule23
}, {
  start: 7738,
  length: 1,
  convRule: rule22
}, {
  start: 7739,
  length: 1,
  convRule: rule23
}, {
  start: 7740,
  length: 1,
  convRule: rule22
}, {
  start: 7741,
  length: 1,
  convRule: rule23
}, {
  start: 7742,
  length: 1,
  convRule: rule22
}, {
  start: 7743,
  length: 1,
  convRule: rule23
}, {
  start: 7744,
  length: 1,
  convRule: rule22
}, {
  start: 7745,
  length: 1,
  convRule: rule23
}, {
  start: 7746,
  length: 1,
  convRule: rule22
}, {
  start: 7747,
  length: 1,
  convRule: rule23
}, {
  start: 7748,
  length: 1,
  convRule: rule22
}, {
  start: 7749,
  length: 1,
  convRule: rule23
}, {
  start: 7750,
  length: 1,
  convRule: rule22
}, {
  start: 7751,
  length: 1,
  convRule: rule23
}, {
  start: 7752,
  length: 1,
  convRule: rule22
}, {
  start: 7753,
  length: 1,
  convRule: rule23
}, {
  start: 7754,
  length: 1,
  convRule: rule22
}, {
  start: 7755,
  length: 1,
  convRule: rule23
}, {
  start: 7756,
  length: 1,
  convRule: rule22
}, {
  start: 7757,
  length: 1,
  convRule: rule23
}, {
  start: 7758,
  length: 1,
  convRule: rule22
}, {
  start: 7759,
  length: 1,
  convRule: rule23
}, {
  start: 7760,
  length: 1,
  convRule: rule22
}, {
  start: 7761,
  length: 1,
  convRule: rule23
}, {
  start: 7762,
  length: 1,
  convRule: rule22
}, {
  start: 7763,
  length: 1,
  convRule: rule23
}, {
  start: 7764,
  length: 1,
  convRule: rule22
}, {
  start: 7765,
  length: 1,
  convRule: rule23
}, {
  start: 7766,
  length: 1,
  convRule: rule22
}, {
  start: 7767,
  length: 1,
  convRule: rule23
}, {
  start: 7768,
  length: 1,
  convRule: rule22
}, {
  start: 7769,
  length: 1,
  convRule: rule23
}, {
  start: 7770,
  length: 1,
  convRule: rule22
}, {
  start: 7771,
  length: 1,
  convRule: rule23
}, {
  start: 7772,
  length: 1,
  convRule: rule22
}, {
  start: 7773,
  length: 1,
  convRule: rule23
}, {
  start: 7774,
  length: 1,
  convRule: rule22
}, {
  start: 7775,
  length: 1,
  convRule: rule23
}, {
  start: 7776,
  length: 1,
  convRule: rule22
}, {
  start: 7777,
  length: 1,
  convRule: rule23
}, {
  start: 7778,
  length: 1,
  convRule: rule22
}, {
  start: 7779,
  length: 1,
  convRule: rule23
}, {
  start: 7780,
  length: 1,
  convRule: rule22
}, {
  start: 7781,
  length: 1,
  convRule: rule23
}, {
  start: 7782,
  length: 1,
  convRule: rule22
}, {
  start: 7783,
  length: 1,
  convRule: rule23
}, {
  start: 7784,
  length: 1,
  convRule: rule22
}, {
  start: 7785,
  length: 1,
  convRule: rule23
}, {
  start: 7786,
  length: 1,
  convRule: rule22
}, {
  start: 7787,
  length: 1,
  convRule: rule23
}, {
  start: 7788,
  length: 1,
  convRule: rule22
}, {
  start: 7789,
  length: 1,
  convRule: rule23
}, {
  start: 7790,
  length: 1,
  convRule: rule22
}, {
  start: 7791,
  length: 1,
  convRule: rule23
}, {
  start: 7792,
  length: 1,
  convRule: rule22
}, {
  start: 7793,
  length: 1,
  convRule: rule23
}, {
  start: 7794,
  length: 1,
  convRule: rule22
}, {
  start: 7795,
  length: 1,
  convRule: rule23
}, {
  start: 7796,
  length: 1,
  convRule: rule22
}, {
  start: 7797,
  length: 1,
  convRule: rule23
}, {
  start: 7798,
  length: 1,
  convRule: rule22
}, {
  start: 7799,
  length: 1,
  convRule: rule23
}, {
  start: 7800,
  length: 1,
  convRule: rule22
}, {
  start: 7801,
  length: 1,
  convRule: rule23
}, {
  start: 7802,
  length: 1,
  convRule: rule22
}, {
  start: 7803,
  length: 1,
  convRule: rule23
}, {
  start: 7804,
  length: 1,
  convRule: rule22
}, {
  start: 7805,
  length: 1,
  convRule: rule23
}, {
  start: 7806,
  length: 1,
  convRule: rule22
}, {
  start: 7807,
  length: 1,
  convRule: rule23
}, {
  start: 7808,
  length: 1,
  convRule: rule22
}, {
  start: 7809,
  length: 1,
  convRule: rule23
}, {
  start: 7810,
  length: 1,
  convRule: rule22
}, {
  start: 7811,
  length: 1,
  convRule: rule23
}, {
  start: 7812,
  length: 1,
  convRule: rule22
}, {
  start: 7813,
  length: 1,
  convRule: rule23
}, {
  start: 7814,
  length: 1,
  convRule: rule22
}, {
  start: 7815,
  length: 1,
  convRule: rule23
}, {
  start: 7816,
  length: 1,
  convRule: rule22
}, {
  start: 7817,
  length: 1,
  convRule: rule23
}, {
  start: 7818,
  length: 1,
  convRule: rule22
}, {
  start: 7819,
  length: 1,
  convRule: rule23
}, {
  start: 7820,
  length: 1,
  convRule: rule22
}, {
  start: 7821,
  length: 1,
  convRule: rule23
}, {
  start: 7822,
  length: 1,
  convRule: rule22
}, {
  start: 7823,
  length: 1,
  convRule: rule23
}, {
  start: 7824,
  length: 1,
  convRule: rule22
}, {
  start: 7825,
  length: 1,
  convRule: rule23
}, {
  start: 7826,
  length: 1,
  convRule: rule22
}, {
  start: 7827,
  length: 1,
  convRule: rule23
}, {
  start: 7828,
  length: 1,
  convRule: rule22
}, {
  start: 7829,
  length: 1,
  convRule: rule23
}, {
  start: 7835,
  length: 1,
  convRule: rule141
}, {
  start: 7838,
  length: 1,
  convRule: rule142
}, {
  start: 7840,
  length: 1,
  convRule: rule22
}, {
  start: 7841,
  length: 1,
  convRule: rule23
}, {
  start: 7842,
  length: 1,
  convRule: rule22
}, {
  start: 7843,
  length: 1,
  convRule: rule23
}, {
  start: 7844,
  length: 1,
  convRule: rule22
}, {
  start: 7845,
  length: 1,
  convRule: rule23
}, {
  start: 7846,
  length: 1,
  convRule: rule22
}, {
  start: 7847,
  length: 1,
  convRule: rule23
}, {
  start: 7848,
  length: 1,
  convRule: rule22
}, {
  start: 7849,
  length: 1,
  convRule: rule23
}, {
  start: 7850,
  length: 1,
  convRule: rule22
}, {
  start: 7851,
  length: 1,
  convRule: rule23
}, {
  start: 7852,
  length: 1,
  convRule: rule22
}, {
  start: 7853,
  length: 1,
  convRule: rule23
}, {
  start: 7854,
  length: 1,
  convRule: rule22
}, {
  start: 7855,
  length: 1,
  convRule: rule23
}, {
  start: 7856,
  length: 1,
  convRule: rule22
}, {
  start: 7857,
  length: 1,
  convRule: rule23
}, {
  start: 7858,
  length: 1,
  convRule: rule22
}, {
  start: 7859,
  length: 1,
  convRule: rule23
}, {
  start: 7860,
  length: 1,
  convRule: rule22
}, {
  start: 7861,
  length: 1,
  convRule: rule23
}, {
  start: 7862,
  length: 1,
  convRule: rule22
}, {
  start: 7863,
  length: 1,
  convRule: rule23
}, {
  start: 7864,
  length: 1,
  convRule: rule22
}, {
  start: 7865,
  length: 1,
  convRule: rule23
}, {
  start: 7866,
  length: 1,
  convRule: rule22
}, {
  start: 7867,
  length: 1,
  convRule: rule23
}, {
  start: 7868,
  length: 1,
  convRule: rule22
}, {
  start: 7869,
  length: 1,
  convRule: rule23
}, {
  start: 7870,
  length: 1,
  convRule: rule22
}, {
  start: 7871,
  length: 1,
  convRule: rule23
}, {
  start: 7872,
  length: 1,
  convRule: rule22
}, {
  start: 7873,
  length: 1,
  convRule: rule23
}, {
  start: 7874,
  length: 1,
  convRule: rule22
}, {
  start: 7875,
  length: 1,
  convRule: rule23
}, {
  start: 7876,
  length: 1,
  convRule: rule22
}, {
  start: 7877,
  length: 1,
  convRule: rule23
}, {
  start: 7878,
  length: 1,
  convRule: rule22
}, {
  start: 7879,
  length: 1,
  convRule: rule23
}, {
  start: 7880,
  length: 1,
  convRule: rule22
}, {
  start: 7881,
  length: 1,
  convRule: rule23
}, {
  start: 7882,
  length: 1,
  convRule: rule22
}, {
  start: 7883,
  length: 1,
  convRule: rule23
}, {
  start: 7884,
  length: 1,
  convRule: rule22
}, {
  start: 7885,
  length: 1,
  convRule: rule23
}, {
  start: 7886,
  length: 1,
  convRule: rule22
}, {
  start: 7887,
  length: 1,
  convRule: rule23
}, {
  start: 7888,
  length: 1,
  convRule: rule22
}, {
  start: 7889,
  length: 1,
  convRule: rule23
}, {
  start: 7890,
  length: 1,
  convRule: rule22
}, {
  start: 7891,
  length: 1,
  convRule: rule23
}, {
  start: 7892,
  length: 1,
  convRule: rule22
}, {
  start: 7893,
  length: 1,
  convRule: rule23
}, {
  start: 7894,
  length: 1,
  convRule: rule22
}, {
  start: 7895,
  length: 1,
  convRule: rule23
}, {
  start: 7896,
  length: 1,
  convRule: rule22
}, {
  start: 7897,
  length: 1,
  convRule: rule23
}, {
  start: 7898,
  length: 1,
  convRule: rule22
}, {
  start: 7899,
  length: 1,
  convRule: rule23
}, {
  start: 7900,
  length: 1,
  convRule: rule22
}, {
  start: 7901,
  length: 1,
  convRule: rule23
}, {
  start: 7902,
  length: 1,
  convRule: rule22
}, {
  start: 7903,
  length: 1,
  convRule: rule23
}, {
  start: 7904,
  length: 1,
  convRule: rule22
}, {
  start: 7905,
  length: 1,
  convRule: rule23
}, {
  start: 7906,
  length: 1,
  convRule: rule22
}, {
  start: 7907,
  length: 1,
  convRule: rule23
}, {
  start: 7908,
  length: 1,
  convRule: rule22
}, {
  start: 7909,
  length: 1,
  convRule: rule23
}, {
  start: 7910,
  length: 1,
  convRule: rule22
}, {
  start: 7911,
  length: 1,
  convRule: rule23
}, {
  start: 7912,
  length: 1,
  convRule: rule22
}, {
  start: 7913,
  length: 1,
  convRule: rule23
}, {
  start: 7914,
  length: 1,
  convRule: rule22
}, {
  start: 7915,
  length: 1,
  convRule: rule23
}, {
  start: 7916,
  length: 1,
  convRule: rule22
}, {
  start: 7917,
  length: 1,
  convRule: rule23
}, {
  start: 7918,
  length: 1,
  convRule: rule22
}, {
  start: 7919,
  length: 1,
  convRule: rule23
}, {
  start: 7920,
  length: 1,
  convRule: rule22
}, {
  start: 7921,
  length: 1,
  convRule: rule23
}, {
  start: 7922,
  length: 1,
  convRule: rule22
}, {
  start: 7923,
  length: 1,
  convRule: rule23
}, {
  start: 7924,
  length: 1,
  convRule: rule22
}, {
  start: 7925,
  length: 1,
  convRule: rule23
}, {
  start: 7926,
  length: 1,
  convRule: rule22
}, {
  start: 7927,
  length: 1,
  convRule: rule23
}, {
  start: 7928,
  length: 1,
  convRule: rule22
}, {
  start: 7929,
  length: 1,
  convRule: rule23
}, {
  start: 7930,
  length: 1,
  convRule: rule22
}, {
  start: 7931,
  length: 1,
  convRule: rule23
}, {
  start: 7932,
  length: 1,
  convRule: rule22
}, {
  start: 7933,
  length: 1,
  convRule: rule23
}, {
  start: 7934,
  length: 1,
  convRule: rule22
}, {
  start: 7935,
  length: 1,
  convRule: rule23
}, {
  start: 7936,
  length: 8,
  convRule: rule143
}, {
  start: 7944,
  length: 8,
  convRule: rule144
}, {
  start: 7952,
  length: 6,
  convRule: rule143
}, {
  start: 7960,
  length: 6,
  convRule: rule144
}, {
  start: 7968,
  length: 8,
  convRule: rule143
}, {
  start: 7976,
  length: 8,
  convRule: rule144
}, {
  start: 7984,
  length: 8,
  convRule: rule143
}, {
  start: 7992,
  length: 8,
  convRule: rule144
}, {
  start: 8e3,
  length: 6,
  convRule: rule143
}, {
  start: 8008,
  length: 6,
  convRule: rule144
}, {
  start: 8017,
  length: 1,
  convRule: rule143
}, {
  start: 8019,
  length: 1,
  convRule: rule143
}, {
  start: 8021,
  length: 1,
  convRule: rule143
}, {
  start: 8023,
  length: 1,
  convRule: rule143
}, {
  start: 8025,
  length: 1,
  convRule: rule144
}, {
  start: 8027,
  length: 1,
  convRule: rule144
}, {
  start: 8029,
  length: 1,
  convRule: rule144
}, {
  start: 8031,
  length: 1,
  convRule: rule144
}, {
  start: 8032,
  length: 8,
  convRule: rule143
}, {
  start: 8040,
  length: 8,
  convRule: rule144
}, {
  start: 8048,
  length: 2,
  convRule: rule145
}, {
  start: 8050,
  length: 4,
  convRule: rule146
}, {
  start: 8054,
  length: 2,
  convRule: rule147
}, {
  start: 8056,
  length: 2,
  convRule: rule148
}, {
  start: 8058,
  length: 2,
  convRule: rule149
}, {
  start: 8060,
  length: 2,
  convRule: rule150
}, {
  start: 8064,
  length: 8,
  convRule: rule143
}, {
  start: 8072,
  length: 8,
  convRule: rule151
}, {
  start: 8080,
  length: 8,
  convRule: rule143
}, {
  start: 8088,
  length: 8,
  convRule: rule151
}, {
  start: 8096,
  length: 8,
  convRule: rule143
}, {
  start: 8104,
  length: 8,
  convRule: rule151
}, {
  start: 8112,
  length: 2,
  convRule: rule143
}, {
  start: 8115,
  length: 1,
  convRule: rule152
}, {
  start: 8120,
  length: 2,
  convRule: rule144
}, {
  start: 8122,
  length: 2,
  convRule: rule153
}, {
  start: 8124,
  length: 1,
  convRule: rule154
}, {
  start: 8126,
  length: 1,
  convRule: rule155
}, {
  start: 8131,
  length: 1,
  convRule: rule152
}, {
  start: 8136,
  length: 4,
  convRule: rule156
}, {
  start: 8140,
  length: 1,
  convRule: rule154
}, {
  start: 8144,
  length: 2,
  convRule: rule143
}, {
  start: 8152,
  length: 2,
  convRule: rule144
}, {
  start: 8154,
  length: 2,
  convRule: rule157
}, {
  start: 8160,
  length: 2,
  convRule: rule143
}, {
  start: 8165,
  length: 1,
  convRule: rule113
}, {
  start: 8168,
  length: 2,
  convRule: rule144
}, {
  start: 8170,
  length: 2,
  convRule: rule158
}, {
  start: 8172,
  length: 1,
  convRule: rule117
}, {
  start: 8179,
  length: 1,
  convRule: rule152
}, {
  start: 8184,
  length: 2,
  convRule: rule159
}, {
  start: 8186,
  length: 2,
  convRule: rule160
}, {
  start: 8188,
  length: 1,
  convRule: rule154
}, {
  start: 8486,
  length: 1,
  convRule: rule163
}, {
  start: 8490,
  length: 1,
  convRule: rule164
}, {
  start: 8491,
  length: 1,
  convRule: rule165
}, {
  start: 8498,
  length: 1,
  convRule: rule166
}, {
  start: 8526,
  length: 1,
  convRule: rule167
}, {
  start: 8544,
  length: 16,
  convRule: rule168
}, {
  start: 8560,
  length: 16,
  convRule: rule169
}, {
  start: 8579,
  length: 1,
  convRule: rule22
}, {
  start: 8580,
  length: 1,
  convRule: rule23
}, {
  start: 9398,
  length: 26,
  convRule: rule170
}, {
  start: 9424,
  length: 26,
  convRule: rule171
}, {
  start: 11264,
  length: 47,
  convRule: rule122
}, {
  start: 11312,
  length: 47,
  convRule: rule123
}, {
  start: 11360,
  length: 1,
  convRule: rule22
}, {
  start: 11361,
  length: 1,
  convRule: rule23
}, {
  start: 11362,
  length: 1,
  convRule: rule172
}, {
  start: 11363,
  length: 1,
  convRule: rule173
}, {
  start: 11364,
  length: 1,
  convRule: rule174
}, {
  start: 11365,
  length: 1,
  convRule: rule175
}, {
  start: 11366,
  length: 1,
  convRule: rule176
}, {
  start: 11367,
  length: 1,
  convRule: rule22
}, {
  start: 11368,
  length: 1,
  convRule: rule23
}, {
  start: 11369,
  length: 1,
  convRule: rule22
}, {
  start: 11370,
  length: 1,
  convRule: rule23
}, {
  start: 11371,
  length: 1,
  convRule: rule22
}, {
  start: 11372,
  length: 1,
  convRule: rule23
}, {
  start: 11373,
  length: 1,
  convRule: rule177
}, {
  start: 11374,
  length: 1,
  convRule: rule178
}, {
  start: 11375,
  length: 1,
  convRule: rule179
}, {
  start: 11376,
  length: 1,
  convRule: rule180
}, {
  start: 11378,
  length: 1,
  convRule: rule22
}, {
  start: 11379,
  length: 1,
  convRule: rule23
}, {
  start: 11381,
  length: 1,
  convRule: rule22
}, {
  start: 11382,
  length: 1,
  convRule: rule23
}, {
  start: 11390,
  length: 2,
  convRule: rule181
}, {
  start: 11392,
  length: 1,
  convRule: rule22
}, {
  start: 11393,
  length: 1,
  convRule: rule23
}, {
  start: 11394,
  length: 1,
  convRule: rule22
}, {
  start: 11395,
  length: 1,
  convRule: rule23
}, {
  start: 11396,
  length: 1,
  convRule: rule22
}, {
  start: 11397,
  length: 1,
  convRule: rule23
}, {
  start: 11398,
  length: 1,
  convRule: rule22
}, {
  start: 11399,
  length: 1,
  convRule: rule23
}, {
  start: 11400,
  length: 1,
  convRule: rule22
}, {
  start: 11401,
  length: 1,
  convRule: rule23
}, {
  start: 11402,
  length: 1,
  convRule: rule22
}, {
  start: 11403,
  length: 1,
  convRule: rule23
}, {
  start: 11404,
  length: 1,
  convRule: rule22
}, {
  start: 11405,
  length: 1,
  convRule: rule23
}, {
  start: 11406,
  length: 1,
  convRule: rule22
}, {
  start: 11407,
  length: 1,
  convRule: rule23
}, {
  start: 11408,
  length: 1,
  convRule: rule22
}, {
  start: 11409,
  length: 1,
  convRule: rule23
}, {
  start: 11410,
  length: 1,
  convRule: rule22
}, {
  start: 11411,
  length: 1,
  convRule: rule23
}, {
  start: 11412,
  length: 1,
  convRule: rule22
}, {
  start: 11413,
  length: 1,
  convRule: rule23
}, {
  start: 11414,
  length: 1,
  convRule: rule22
}, {
  start: 11415,
  length: 1,
  convRule: rule23
}, {
  start: 11416,
  length: 1,
  convRule: rule22
}, {
  start: 11417,
  length: 1,
  convRule: rule23
}, {
  start: 11418,
  length: 1,
  convRule: rule22
}, {
  start: 11419,
  length: 1,
  convRule: rule23
}, {
  start: 11420,
  length: 1,
  convRule: rule22
}, {
  start: 11421,
  length: 1,
  convRule: rule23
}, {
  start: 11422,
  length: 1,
  convRule: rule22
}, {
  start: 11423,
  length: 1,
  convRule: rule23
}, {
  start: 11424,
  length: 1,
  convRule: rule22
}, {
  start: 11425,
  length: 1,
  convRule: rule23
}, {
  start: 11426,
  length: 1,
  convRule: rule22
}, {
  start: 11427,
  length: 1,
  convRule: rule23
}, {
  start: 11428,
  length: 1,
  convRule: rule22
}, {
  start: 11429,
  length: 1,
  convRule: rule23
}, {
  start: 11430,
  length: 1,
  convRule: rule22
}, {
  start: 11431,
  length: 1,
  convRule: rule23
}, {
  start: 11432,
  length: 1,
  convRule: rule22
}, {
  start: 11433,
  length: 1,
  convRule: rule23
}, {
  start: 11434,
  length: 1,
  convRule: rule22
}, {
  start: 11435,
  length: 1,
  convRule: rule23
}, {
  start: 11436,
  length: 1,
  convRule: rule22
}, {
  start: 11437,
  length: 1,
  convRule: rule23
}, {
  start: 11438,
  length: 1,
  convRule: rule22
}, {
  start: 11439,
  length: 1,
  convRule: rule23
}, {
  start: 11440,
  length: 1,
  convRule: rule22
}, {
  start: 11441,
  length: 1,
  convRule: rule23
}, {
  start: 11442,
  length: 1,
  convRule: rule22
}, {
  start: 11443,
  length: 1,
  convRule: rule23
}, {
  start: 11444,
  length: 1,
  convRule: rule22
}, {
  start: 11445,
  length: 1,
  convRule: rule23
}, {
  start: 11446,
  length: 1,
  convRule: rule22
}, {
  start: 11447,
  length: 1,
  convRule: rule23
}, {
  start: 11448,
  length: 1,
  convRule: rule22
}, {
  start: 11449,
  length: 1,
  convRule: rule23
}, {
  start: 11450,
  length: 1,
  convRule: rule22
}, {
  start: 11451,
  length: 1,
  convRule: rule23
}, {
  start: 11452,
  length: 1,
  convRule: rule22
}, {
  start: 11453,
  length: 1,
  convRule: rule23
}, {
  start: 11454,
  length: 1,
  convRule: rule22
}, {
  start: 11455,
  length: 1,
  convRule: rule23
}, {
  start: 11456,
  length: 1,
  convRule: rule22
}, {
  start: 11457,
  length: 1,
  convRule: rule23
}, {
  start: 11458,
  length: 1,
  convRule: rule22
}, {
  start: 11459,
  length: 1,
  convRule: rule23
}, {
  start: 11460,
  length: 1,
  convRule: rule22
}, {
  start: 11461,
  length: 1,
  convRule: rule23
}, {
  start: 11462,
  length: 1,
  convRule: rule22
}, {
  start: 11463,
  length: 1,
  convRule: rule23
}, {
  start: 11464,
  length: 1,
  convRule: rule22
}, {
  start: 11465,
  length: 1,
  convRule: rule23
}, {
  start: 11466,
  length: 1,
  convRule: rule22
}, {
  start: 11467,
  length: 1,
  convRule: rule23
}, {
  start: 11468,
  length: 1,
  convRule: rule22
}, {
  start: 11469,
  length: 1,
  convRule: rule23
}, {
  start: 11470,
  length: 1,
  convRule: rule22
}, {
  start: 11471,
  length: 1,
  convRule: rule23
}, {
  start: 11472,
  length: 1,
  convRule: rule22
}, {
  start: 11473,
  length: 1,
  convRule: rule23
}, {
  start: 11474,
  length: 1,
  convRule: rule22
}, {
  start: 11475,
  length: 1,
  convRule: rule23
}, {
  start: 11476,
  length: 1,
  convRule: rule22
}, {
  start: 11477,
  length: 1,
  convRule: rule23
}, {
  start: 11478,
  length: 1,
  convRule: rule22
}, {
  start: 11479,
  length: 1,
  convRule: rule23
}, {
  start: 11480,
  length: 1,
  convRule: rule22
}, {
  start: 11481,
  length: 1,
  convRule: rule23
}, {
  start: 11482,
  length: 1,
  convRule: rule22
}, {
  start: 11483,
  length: 1,
  convRule: rule23
}, {
  start: 11484,
  length: 1,
  convRule: rule22
}, {
  start: 11485,
  length: 1,
  convRule: rule23
}, {
  start: 11486,
  length: 1,
  convRule: rule22
}, {
  start: 11487,
  length: 1,
  convRule: rule23
}, {
  start: 11488,
  length: 1,
  convRule: rule22
}, {
  start: 11489,
  length: 1,
  convRule: rule23
}, {
  start: 11490,
  length: 1,
  convRule: rule22
}, {
  start: 11491,
  length: 1,
  convRule: rule23
}, {
  start: 11499,
  length: 1,
  convRule: rule22
}, {
  start: 11500,
  length: 1,
  convRule: rule23
}, {
  start: 11501,
  length: 1,
  convRule: rule22
}, {
  start: 11502,
  length: 1,
  convRule: rule23
}, {
  start: 11506,
  length: 1,
  convRule: rule22
}, {
  start: 11507,
  length: 1,
  convRule: rule23
}, {
  start: 11520,
  length: 38,
  convRule: rule182
}, {
  start: 11559,
  length: 1,
  convRule: rule182
}, {
  start: 11565,
  length: 1,
  convRule: rule182
}, {
  start: 42560,
  length: 1,
  convRule: rule22
}, {
  start: 42561,
  length: 1,
  convRule: rule23
}, {
  start: 42562,
  length: 1,
  convRule: rule22
}, {
  start: 42563,
  length: 1,
  convRule: rule23
}, {
  start: 42564,
  length: 1,
  convRule: rule22
}, {
  start: 42565,
  length: 1,
  convRule: rule23
}, {
  start: 42566,
  length: 1,
  convRule: rule22
}, {
  start: 42567,
  length: 1,
  convRule: rule23
}, {
  start: 42568,
  length: 1,
  convRule: rule22
}, {
  start: 42569,
  length: 1,
  convRule: rule23
}, {
  start: 42570,
  length: 1,
  convRule: rule22
}, {
  start: 42571,
  length: 1,
  convRule: rule23
}, {
  start: 42572,
  length: 1,
  convRule: rule22
}, {
  start: 42573,
  length: 1,
  convRule: rule23
}, {
  start: 42574,
  length: 1,
  convRule: rule22
}, {
  start: 42575,
  length: 1,
  convRule: rule23
}, {
  start: 42576,
  length: 1,
  convRule: rule22
}, {
  start: 42577,
  length: 1,
  convRule: rule23
}, {
  start: 42578,
  length: 1,
  convRule: rule22
}, {
  start: 42579,
  length: 1,
  convRule: rule23
}, {
  start: 42580,
  length: 1,
  convRule: rule22
}, {
  start: 42581,
  length: 1,
  convRule: rule23
}, {
  start: 42582,
  length: 1,
  convRule: rule22
}, {
  start: 42583,
  length: 1,
  convRule: rule23
}, {
  start: 42584,
  length: 1,
  convRule: rule22
}, {
  start: 42585,
  length: 1,
  convRule: rule23
}, {
  start: 42586,
  length: 1,
  convRule: rule22
}, {
  start: 42587,
  length: 1,
  convRule: rule23
}, {
  start: 42588,
  length: 1,
  convRule: rule22
}, {
  start: 42589,
  length: 1,
  convRule: rule23
}, {
  start: 42590,
  length: 1,
  convRule: rule22
}, {
  start: 42591,
  length: 1,
  convRule: rule23
}, {
  start: 42592,
  length: 1,
  convRule: rule22
}, {
  start: 42593,
  length: 1,
  convRule: rule23
}, {
  start: 42594,
  length: 1,
  convRule: rule22
}, {
  start: 42595,
  length: 1,
  convRule: rule23
}, {
  start: 42596,
  length: 1,
  convRule: rule22
}, {
  start: 42597,
  length: 1,
  convRule: rule23
}, {
  start: 42598,
  length: 1,
  convRule: rule22
}, {
  start: 42599,
  length: 1,
  convRule: rule23
}, {
  start: 42600,
  length: 1,
  convRule: rule22
}, {
  start: 42601,
  length: 1,
  convRule: rule23
}, {
  start: 42602,
  length: 1,
  convRule: rule22
}, {
  start: 42603,
  length: 1,
  convRule: rule23
}, {
  start: 42604,
  length: 1,
  convRule: rule22
}, {
  start: 42605,
  length: 1,
  convRule: rule23
}, {
  start: 42624,
  length: 1,
  convRule: rule22
}, {
  start: 42625,
  length: 1,
  convRule: rule23
}, {
  start: 42626,
  length: 1,
  convRule: rule22
}, {
  start: 42627,
  length: 1,
  convRule: rule23
}, {
  start: 42628,
  length: 1,
  convRule: rule22
}, {
  start: 42629,
  length: 1,
  convRule: rule23
}, {
  start: 42630,
  length: 1,
  convRule: rule22
}, {
  start: 42631,
  length: 1,
  convRule: rule23
}, {
  start: 42632,
  length: 1,
  convRule: rule22
}, {
  start: 42633,
  length: 1,
  convRule: rule23
}, {
  start: 42634,
  length: 1,
  convRule: rule22
}, {
  start: 42635,
  length: 1,
  convRule: rule23
}, {
  start: 42636,
  length: 1,
  convRule: rule22
}, {
  start: 42637,
  length: 1,
  convRule: rule23
}, {
  start: 42638,
  length: 1,
  convRule: rule22
}, {
  start: 42639,
  length: 1,
  convRule: rule23
}, {
  start: 42640,
  length: 1,
  convRule: rule22
}, {
  start: 42641,
  length: 1,
  convRule: rule23
}, {
  start: 42642,
  length: 1,
  convRule: rule22
}, {
  start: 42643,
  length: 1,
  convRule: rule23
}, {
  start: 42644,
  length: 1,
  convRule: rule22
}, {
  start: 42645,
  length: 1,
  convRule: rule23
}, {
  start: 42646,
  length: 1,
  convRule: rule22
}, {
  start: 42647,
  length: 1,
  convRule: rule23
}, {
  start: 42648,
  length: 1,
  convRule: rule22
}, {
  start: 42649,
  length: 1,
  convRule: rule23
}, {
  start: 42650,
  length: 1,
  convRule: rule22
}, {
  start: 42651,
  length: 1,
  convRule: rule23
}, {
  start: 42786,
  length: 1,
  convRule: rule22
}, {
  start: 42787,
  length: 1,
  convRule: rule23
}, {
  start: 42788,
  length: 1,
  convRule: rule22
}, {
  start: 42789,
  length: 1,
  convRule: rule23
}, {
  start: 42790,
  length: 1,
  convRule: rule22
}, {
  start: 42791,
  length: 1,
  convRule: rule23
}, {
  start: 42792,
  length: 1,
  convRule: rule22
}, {
  start: 42793,
  length: 1,
  convRule: rule23
}, {
  start: 42794,
  length: 1,
  convRule: rule22
}, {
  start: 42795,
  length: 1,
  convRule: rule23
}, {
  start: 42796,
  length: 1,
  convRule: rule22
}, {
  start: 42797,
  length: 1,
  convRule: rule23
}, {
  start: 42798,
  length: 1,
  convRule: rule22
}, {
  start: 42799,
  length: 1,
  convRule: rule23
}, {
  start: 42802,
  length: 1,
  convRule: rule22
}, {
  start: 42803,
  length: 1,
  convRule: rule23
}, {
  start: 42804,
  length: 1,
  convRule: rule22
}, {
  start: 42805,
  length: 1,
  convRule: rule23
}, {
  start: 42806,
  length: 1,
  convRule: rule22
}, {
  start: 42807,
  length: 1,
  convRule: rule23
}, {
  start: 42808,
  length: 1,
  convRule: rule22
}, {
  start: 42809,
  length: 1,
  convRule: rule23
}, {
  start: 42810,
  length: 1,
  convRule: rule22
}, {
  start: 42811,
  length: 1,
  convRule: rule23
}, {
  start: 42812,
  length: 1,
  convRule: rule22
}, {
  start: 42813,
  length: 1,
  convRule: rule23
}, {
  start: 42814,
  length: 1,
  convRule: rule22
}, {
  start: 42815,
  length: 1,
  convRule: rule23
}, {
  start: 42816,
  length: 1,
  convRule: rule22
}, {
  start: 42817,
  length: 1,
  convRule: rule23
}, {
  start: 42818,
  length: 1,
  convRule: rule22
}, {
  start: 42819,
  length: 1,
  convRule: rule23
}, {
  start: 42820,
  length: 1,
  convRule: rule22
}, {
  start: 42821,
  length: 1,
  convRule: rule23
}, {
  start: 42822,
  length: 1,
  convRule: rule22
}, {
  start: 42823,
  length: 1,
  convRule: rule23
}, {
  start: 42824,
  length: 1,
  convRule: rule22
}, {
  start: 42825,
  length: 1,
  convRule: rule23
}, {
  start: 42826,
  length: 1,
  convRule: rule22
}, {
  start: 42827,
  length: 1,
  convRule: rule23
}, {
  start: 42828,
  length: 1,
  convRule: rule22
}, {
  start: 42829,
  length: 1,
  convRule: rule23
}, {
  start: 42830,
  length: 1,
  convRule: rule22
}, {
  start: 42831,
  length: 1,
  convRule: rule23
}, {
  start: 42832,
  length: 1,
  convRule: rule22
}, {
  start: 42833,
  length: 1,
  convRule: rule23
}, {
  start: 42834,
  length: 1,
  convRule: rule22
}, {
  start: 42835,
  length: 1,
  convRule: rule23
}, {
  start: 42836,
  length: 1,
  convRule: rule22
}, {
  start: 42837,
  length: 1,
  convRule: rule23
}, {
  start: 42838,
  length: 1,
  convRule: rule22
}, {
  start: 42839,
  length: 1,
  convRule: rule23
}, {
  start: 42840,
  length: 1,
  convRule: rule22
}, {
  start: 42841,
  length: 1,
  convRule: rule23
}, {
  start: 42842,
  length: 1,
  convRule: rule22
}, {
  start: 42843,
  length: 1,
  convRule: rule23
}, {
  start: 42844,
  length: 1,
  convRule: rule22
}, {
  start: 42845,
  length: 1,
  convRule: rule23
}, {
  start: 42846,
  length: 1,
  convRule: rule22
}, {
  start: 42847,
  length: 1,
  convRule: rule23
}, {
  start: 42848,
  length: 1,
  convRule: rule22
}, {
  start: 42849,
  length: 1,
  convRule: rule23
}, {
  start: 42850,
  length: 1,
  convRule: rule22
}, {
  start: 42851,
  length: 1,
  convRule: rule23
}, {
  start: 42852,
  length: 1,
  convRule: rule22
}, {
  start: 42853,
  length: 1,
  convRule: rule23
}, {
  start: 42854,
  length: 1,
  convRule: rule22
}, {
  start: 42855,
  length: 1,
  convRule: rule23
}, {
  start: 42856,
  length: 1,
  convRule: rule22
}, {
  start: 42857,
  length: 1,
  convRule: rule23
}, {
  start: 42858,
  length: 1,
  convRule: rule22
}, {
  start: 42859,
  length: 1,
  convRule: rule23
}, {
  start: 42860,
  length: 1,
  convRule: rule22
}, {
  start: 42861,
  length: 1,
  convRule: rule23
}, {
  start: 42862,
  length: 1,
  convRule: rule22
}, {
  start: 42863,
  length: 1,
  convRule: rule23
}, {
  start: 42873,
  length: 1,
  convRule: rule22
}, {
  start: 42874,
  length: 1,
  convRule: rule23
}, {
  start: 42875,
  length: 1,
  convRule: rule22
}, {
  start: 42876,
  length: 1,
  convRule: rule23
}, {
  start: 42877,
  length: 1,
  convRule: rule183
}, {
  start: 42878,
  length: 1,
  convRule: rule22
}, {
  start: 42879,
  length: 1,
  convRule: rule23
}, {
  start: 42880,
  length: 1,
  convRule: rule22
}, {
  start: 42881,
  length: 1,
  convRule: rule23
}, {
  start: 42882,
  length: 1,
  convRule: rule22
}, {
  start: 42883,
  length: 1,
  convRule: rule23
}, {
  start: 42884,
  length: 1,
  convRule: rule22
}, {
  start: 42885,
  length: 1,
  convRule: rule23
}, {
  start: 42886,
  length: 1,
  convRule: rule22
}, {
  start: 42887,
  length: 1,
  convRule: rule23
}, {
  start: 42891,
  length: 1,
  convRule: rule22
}, {
  start: 42892,
  length: 1,
  convRule: rule23
}, {
  start: 42893,
  length: 1,
  convRule: rule184
}, {
  start: 42896,
  length: 1,
  convRule: rule22
}, {
  start: 42897,
  length: 1,
  convRule: rule23
}, {
  start: 42898,
  length: 1,
  convRule: rule22
}, {
  start: 42899,
  length: 1,
  convRule: rule23
}, {
  start: 42900,
  length: 1,
  convRule: rule185
}, {
  start: 42902,
  length: 1,
  convRule: rule22
}, {
  start: 42903,
  length: 1,
  convRule: rule23
}, {
  start: 42904,
  length: 1,
  convRule: rule22
}, {
  start: 42905,
  length: 1,
  convRule: rule23
}, {
  start: 42906,
  length: 1,
  convRule: rule22
}, {
  start: 42907,
  length: 1,
  convRule: rule23
}, {
  start: 42908,
  length: 1,
  convRule: rule22
}, {
  start: 42909,
  length: 1,
  convRule: rule23
}, {
  start: 42910,
  length: 1,
  convRule: rule22
}, {
  start: 42911,
  length: 1,
  convRule: rule23
}, {
  start: 42912,
  length: 1,
  convRule: rule22
}, {
  start: 42913,
  length: 1,
  convRule: rule23
}, {
  start: 42914,
  length: 1,
  convRule: rule22
}, {
  start: 42915,
  length: 1,
  convRule: rule23
}, {
  start: 42916,
  length: 1,
  convRule: rule22
}, {
  start: 42917,
  length: 1,
  convRule: rule23
}, {
  start: 42918,
  length: 1,
  convRule: rule22
}, {
  start: 42919,
  length: 1,
  convRule: rule23
}, {
  start: 42920,
  length: 1,
  convRule: rule22
}, {
  start: 42921,
  length: 1,
  convRule: rule23
}, {
  start: 42922,
  length: 1,
  convRule: rule186
}, {
  start: 42923,
  length: 1,
  convRule: rule187
}, {
  start: 42924,
  length: 1,
  convRule: rule188
}, {
  start: 42925,
  length: 1,
  convRule: rule189
}, {
  start: 42926,
  length: 1,
  convRule: rule186
}, {
  start: 42928,
  length: 1,
  convRule: rule190
}, {
  start: 42929,
  length: 1,
  convRule: rule191
}, {
  start: 42930,
  length: 1,
  convRule: rule192
}, {
  start: 42931,
  length: 1,
  convRule: rule193
}, {
  start: 42932,
  length: 1,
  convRule: rule22
}, {
  start: 42933,
  length: 1,
  convRule: rule23
}, {
  start: 42934,
  length: 1,
  convRule: rule22
}, {
  start: 42935,
  length: 1,
  convRule: rule23
}, {
  start: 42936,
  length: 1,
  convRule: rule22
}, {
  start: 42937,
  length: 1,
  convRule: rule23
}, {
  start: 42938,
  length: 1,
  convRule: rule22
}, {
  start: 42939,
  length: 1,
  convRule: rule23
}, {
  start: 42940,
  length: 1,
  convRule: rule22
}, {
  start: 42941,
  length: 1,
  convRule: rule23
}, {
  start: 42942,
  length: 1,
  convRule: rule22
}, {
  start: 42943,
  length: 1,
  convRule: rule23
}, {
  start: 42946,
  length: 1,
  convRule: rule22
}, {
  start: 42947,
  length: 1,
  convRule: rule23
}, {
  start: 42948,
  length: 1,
  convRule: rule194
}, {
  start: 42949,
  length: 1,
  convRule: rule195
}, {
  start: 42950,
  length: 1,
  convRule: rule196
}, {
  start: 42951,
  length: 1,
  convRule: rule22
}, {
  start: 42952,
  length: 1,
  convRule: rule23
}, {
  start: 42953,
  length: 1,
  convRule: rule22
}, {
  start: 42954,
  length: 1,
  convRule: rule23
}, {
  start: 42997,
  length: 1,
  convRule: rule22
}, {
  start: 42998,
  length: 1,
  convRule: rule23
}, {
  start: 43859,
  length: 1,
  convRule: rule197
}, {
  start: 43888,
  length: 80,
  convRule: rule198
}, {
  start: 65313,
  length: 26,
  convRule: rule9
}, {
  start: 65345,
  length: 26,
  convRule: rule12
}, {
  start: 66560,
  length: 40,
  convRule: rule201
}, {
  start: 66600,
  length: 40,
  convRule: rule202
}, {
  start: 66736,
  length: 36,
  convRule: rule201
}, {
  start: 66776,
  length: 36,
  convRule: rule202
}, {
  start: 68736,
  length: 51,
  convRule: rule97
}, {
  start: 68800,
  length: 51,
  convRule: rule102
}, {
  start: 71840,
  length: 32,
  convRule: rule9
}, {
  start: 71872,
  length: 32,
  convRule: rule12
}, {
  start: 93760,
  length: 32,
  convRule: rule9
}, {
  start: 93792,
  length: 32,
  convRule: rule12
}, {
  start: 125184,
  length: 34,
  convRule: rule203
}, {
  start: 125218,
  length: 34,
  convRule: rule204
}];
var bsearch = function(a) {
  return function(array) {
    return function(size) {
      return function(compare2) {
        var go = function($copy_i) {
          return function($copy_k) {
            var $tco_var_i = $copy_i;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(i, k) {
              if (i > k || i >= length2(array)) {
                $tco_done = true;
                return Nothing.value;
              }
              ;
              if (otherwise) {
                var j = floor2(toNumber(i + k | 0) / 2);
                var v = compare2(a)(array[j]);
                if (v instanceof EQ) {
                  $tco_done = true;
                  return new Just(array[j]);
                }
                ;
                if (v instanceof GT) {
                  $tco_var_i = j + 1 | 0;
                  $copy_k = k;
                  return;
                }
                ;
                $tco_var_i = i;
                $copy_k = j - 1 | 0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i.constructor.name, k.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_i, $copy_k);
            }
            ;
            return $tco_result;
          };
        };
        return go(0)(size);
      };
    };
  };
};
var blkCmp = function(v) {
  return function(v1) {
    if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
      return EQ.value;
    }
    ;
    if (v.start > v1.start) {
      return GT.value;
    }
    ;
    if (otherwise) {
      return LT.value;
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
  };
};
var getRule = function(blocks) {
  return function(unichar) {
    return function(size) {
      var key = {
        start: unichar,
        length: 1,
        convRule: nullrule
      };
      var maybeCharBlock = bsearch(key)(blocks)(size)(blkCmp);
      if (maybeCharBlock instanceof Nothing) {
        return Nothing.value;
      }
      ;
      if (maybeCharBlock instanceof Just) {
        return new Just(maybeCharBlock.value0.convRule);
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
    };
  };
};
var caseConv = function(f) {
  return function($$char2) {
    var maybeConversionRule = getRule(convchars)($$char2)(numConvBlocks);
    if (maybeConversionRule instanceof Nothing) {
      return $$char2;
    }
    ;
    if (maybeConversionRule instanceof Just) {
      return $$char2 + f(maybeConversionRule.value0) | 0;
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5727, column 5 - line 5729, column 53): " + [maybeConversionRule.constructor.name]);
  };
};
var uTowlower = /* @__PURE__ */ caseConv(function(v) {
  return v.lowdist;
});
var uTowupper = /* @__PURE__ */ caseConv(function(v) {
  return v.updist;
});
var checkAttrS = function(categories) {
  return function($$char2) {
    var maybeConversionRule = getRule(spacechars)($$char2)(numSpaceBlocks);
    if (maybeConversionRule instanceof Nothing) {
      return false;
    }
    ;
    if (maybeConversionRule instanceof Just) {
      return isJust(elemIndex(eqInt)(maybeConversionRule.value0.category)(categories));
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5654, column 5 - line 5656, column 86): " + [maybeConversionRule.constructor.name]);
  };
};
var uIswspace = /* @__PURE__ */ checkAttrS([gencatZS]);
var allchars = [{
  start: 0,
  length: 32,
  convRule: rule0
}, {
  start: 32,
  length: 1,
  convRule: rule1
}, {
  start: 33,
  length: 3,
  convRule: rule2
}, {
  start: 36,
  length: 1,
  convRule: rule3
}, {
  start: 37,
  length: 3,
  convRule: rule2
}, {
  start: 40,
  length: 1,
  convRule: rule4
}, {
  start: 41,
  length: 1,
  convRule: rule5
}, {
  start: 42,
  length: 1,
  convRule: rule2
}, {
  start: 43,
  length: 1,
  convRule: rule6
}, {
  start: 44,
  length: 1,
  convRule: rule2
}, {
  start: 45,
  length: 1,
  convRule: rule7
}, {
  start: 46,
  length: 2,
  convRule: rule2
}, {
  start: 48,
  length: 10,
  convRule: rule8
}, {
  start: 58,
  length: 2,
  convRule: rule2
}, {
  start: 60,
  length: 3,
  convRule: rule6
}, {
  start: 63,
  length: 2,
  convRule: rule2
}, {
  start: 65,
  length: 26,
  convRule: rule9
}, {
  start: 91,
  length: 1,
  convRule: rule4
}, {
  start: 92,
  length: 1,
  convRule: rule2
}, {
  start: 93,
  length: 1,
  convRule: rule5
}, {
  start: 94,
  length: 1,
  convRule: rule10
}, {
  start: 95,
  length: 1,
  convRule: rule11
}, {
  start: 96,
  length: 1,
  convRule: rule10
}, {
  start: 97,
  length: 26,
  convRule: rule12
}, {
  start: 123,
  length: 1,
  convRule: rule4
}, {
  start: 124,
  length: 1,
  convRule: rule6
}, {
  start: 125,
  length: 1,
  convRule: rule5
}, {
  start: 126,
  length: 1,
  convRule: rule6
}, {
  start: 127,
  length: 33,
  convRule: rule0
}, {
  start: 160,
  length: 1,
  convRule: rule1
}, {
  start: 161,
  length: 1,
  convRule: rule2
}, {
  start: 162,
  length: 4,
  convRule: rule3
}, {
  start: 166,
  length: 1,
  convRule: rule13
}, {
  start: 167,
  length: 1,
  convRule: rule2
}, {
  start: 168,
  length: 1,
  convRule: rule10
}, {
  start: 169,
  length: 1,
  convRule: rule13
}, {
  start: 170,
  length: 1,
  convRule: rule14
}, {
  start: 171,
  length: 1,
  convRule: rule15
}, {
  start: 172,
  length: 1,
  convRule: rule6
}, {
  start: 173,
  length: 1,
  convRule: rule16
}, {
  start: 174,
  length: 1,
  convRule: rule13
}, {
  start: 175,
  length: 1,
  convRule: rule10
}, {
  start: 176,
  length: 1,
  convRule: rule13
}, {
  start: 177,
  length: 1,
  convRule: rule6
}, {
  start: 178,
  length: 2,
  convRule: rule17
}, {
  start: 180,
  length: 1,
  convRule: rule10
}, {
  start: 181,
  length: 1,
  convRule: rule18
}, {
  start: 182,
  length: 2,
  convRule: rule2
}, {
  start: 184,
  length: 1,
  convRule: rule10
}, {
  start: 185,
  length: 1,
  convRule: rule17
}, {
  start: 186,
  length: 1,
  convRule: rule14
}, {
  start: 187,
  length: 1,
  convRule: rule19
}, {
  start: 188,
  length: 3,
  convRule: rule17
}, {
  start: 191,
  length: 1,
  convRule: rule2
}, {
  start: 192,
  length: 23,
  convRule: rule9
}, {
  start: 215,
  length: 1,
  convRule: rule6
}, {
  start: 216,
  length: 7,
  convRule: rule9
}, {
  start: 223,
  length: 1,
  convRule: rule20
}, {
  start: 224,
  length: 23,
  convRule: rule12
}, {
  start: 247,
  length: 1,
  convRule: rule6
}, {
  start: 248,
  length: 7,
  convRule: rule12
}, {
  start: 255,
  length: 1,
  convRule: rule21
}, {
  start: 256,
  length: 1,
  convRule: rule22
}, {
  start: 257,
  length: 1,
  convRule: rule23
}, {
  start: 258,
  length: 1,
  convRule: rule22
}, {
  start: 259,
  length: 1,
  convRule: rule23
}, {
  start: 260,
  length: 1,
  convRule: rule22
}, {
  start: 261,
  length: 1,
  convRule: rule23
}, {
  start: 262,
  length: 1,
  convRule: rule22
}, {
  start: 263,
  length: 1,
  convRule: rule23
}, {
  start: 264,
  length: 1,
  convRule: rule22
}, {
  start: 265,
  length: 1,
  convRule: rule23
}, {
  start: 266,
  length: 1,
  convRule: rule22
}, {
  start: 267,
  length: 1,
  convRule: rule23
}, {
  start: 268,
  length: 1,
  convRule: rule22
}, {
  start: 269,
  length: 1,
  convRule: rule23
}, {
  start: 270,
  length: 1,
  convRule: rule22
}, {
  start: 271,
  length: 1,
  convRule: rule23
}, {
  start: 272,
  length: 1,
  convRule: rule22
}, {
  start: 273,
  length: 1,
  convRule: rule23
}, {
  start: 274,
  length: 1,
  convRule: rule22
}, {
  start: 275,
  length: 1,
  convRule: rule23
}, {
  start: 276,
  length: 1,
  convRule: rule22
}, {
  start: 277,
  length: 1,
  convRule: rule23
}, {
  start: 278,
  length: 1,
  convRule: rule22
}, {
  start: 279,
  length: 1,
  convRule: rule23
}, {
  start: 280,
  length: 1,
  convRule: rule22
}, {
  start: 281,
  length: 1,
  convRule: rule23
}, {
  start: 282,
  length: 1,
  convRule: rule22
}, {
  start: 283,
  length: 1,
  convRule: rule23
}, {
  start: 284,
  length: 1,
  convRule: rule22
}, {
  start: 285,
  length: 1,
  convRule: rule23
}, {
  start: 286,
  length: 1,
  convRule: rule22
}, {
  start: 287,
  length: 1,
  convRule: rule23
}, {
  start: 288,
  length: 1,
  convRule: rule22
}, {
  start: 289,
  length: 1,
  convRule: rule23
}, {
  start: 290,
  length: 1,
  convRule: rule22
}, {
  start: 291,
  length: 1,
  convRule: rule23
}, {
  start: 292,
  length: 1,
  convRule: rule22
}, {
  start: 293,
  length: 1,
  convRule: rule23
}, {
  start: 294,
  length: 1,
  convRule: rule22
}, {
  start: 295,
  length: 1,
  convRule: rule23
}, {
  start: 296,
  length: 1,
  convRule: rule22
}, {
  start: 297,
  length: 1,
  convRule: rule23
}, {
  start: 298,
  length: 1,
  convRule: rule22
}, {
  start: 299,
  length: 1,
  convRule: rule23
}, {
  start: 300,
  length: 1,
  convRule: rule22
}, {
  start: 301,
  length: 1,
  convRule: rule23
}, {
  start: 302,
  length: 1,
  convRule: rule22
}, {
  start: 303,
  length: 1,
  convRule: rule23
}, {
  start: 304,
  length: 1,
  convRule: rule24
}, {
  start: 305,
  length: 1,
  convRule: rule25
}, {
  start: 306,
  length: 1,
  convRule: rule22
}, {
  start: 307,
  length: 1,
  convRule: rule23
}, {
  start: 308,
  length: 1,
  convRule: rule22
}, {
  start: 309,
  length: 1,
  convRule: rule23
}, {
  start: 310,
  length: 1,
  convRule: rule22
}, {
  start: 311,
  length: 1,
  convRule: rule23
}, {
  start: 312,
  length: 1,
  convRule: rule20
}, {
  start: 313,
  length: 1,
  convRule: rule22
}, {
  start: 314,
  length: 1,
  convRule: rule23
}, {
  start: 315,
  length: 1,
  convRule: rule22
}, {
  start: 316,
  length: 1,
  convRule: rule23
}, {
  start: 317,
  length: 1,
  convRule: rule22
}, {
  start: 318,
  length: 1,
  convRule: rule23
}, {
  start: 319,
  length: 1,
  convRule: rule22
}, {
  start: 320,
  length: 1,
  convRule: rule23
}, {
  start: 321,
  length: 1,
  convRule: rule22
}, {
  start: 322,
  length: 1,
  convRule: rule23
}, {
  start: 323,
  length: 1,
  convRule: rule22
}, {
  start: 324,
  length: 1,
  convRule: rule23
}, {
  start: 325,
  length: 1,
  convRule: rule22
}, {
  start: 326,
  length: 1,
  convRule: rule23
}, {
  start: 327,
  length: 1,
  convRule: rule22
}, {
  start: 328,
  length: 1,
  convRule: rule23
}, {
  start: 329,
  length: 1,
  convRule: rule20
}, {
  start: 330,
  length: 1,
  convRule: rule22
}, {
  start: 331,
  length: 1,
  convRule: rule23
}, {
  start: 332,
  length: 1,
  convRule: rule22
}, {
  start: 333,
  length: 1,
  convRule: rule23
}, {
  start: 334,
  length: 1,
  convRule: rule22
}, {
  start: 335,
  length: 1,
  convRule: rule23
}, {
  start: 336,
  length: 1,
  convRule: rule22
}, {
  start: 337,
  length: 1,
  convRule: rule23
}, {
  start: 338,
  length: 1,
  convRule: rule22
}, {
  start: 339,
  length: 1,
  convRule: rule23
}, {
  start: 340,
  length: 1,
  convRule: rule22
}, {
  start: 341,
  length: 1,
  convRule: rule23
}, {
  start: 342,
  length: 1,
  convRule: rule22
}, {
  start: 343,
  length: 1,
  convRule: rule23
}, {
  start: 344,
  length: 1,
  convRule: rule22
}, {
  start: 345,
  length: 1,
  convRule: rule23
}, {
  start: 346,
  length: 1,
  convRule: rule22
}, {
  start: 347,
  length: 1,
  convRule: rule23
}, {
  start: 348,
  length: 1,
  convRule: rule22
}, {
  start: 349,
  length: 1,
  convRule: rule23
}, {
  start: 350,
  length: 1,
  convRule: rule22
}, {
  start: 351,
  length: 1,
  convRule: rule23
}, {
  start: 352,
  length: 1,
  convRule: rule22
}, {
  start: 353,
  length: 1,
  convRule: rule23
}, {
  start: 354,
  length: 1,
  convRule: rule22
}, {
  start: 355,
  length: 1,
  convRule: rule23
}, {
  start: 356,
  length: 1,
  convRule: rule22
}, {
  start: 357,
  length: 1,
  convRule: rule23
}, {
  start: 358,
  length: 1,
  convRule: rule22
}, {
  start: 359,
  length: 1,
  convRule: rule23
}, {
  start: 360,
  length: 1,
  convRule: rule22
}, {
  start: 361,
  length: 1,
  convRule: rule23
}, {
  start: 362,
  length: 1,
  convRule: rule22
}, {
  start: 363,
  length: 1,
  convRule: rule23
}, {
  start: 364,
  length: 1,
  convRule: rule22
}, {
  start: 365,
  length: 1,
  convRule: rule23
}, {
  start: 366,
  length: 1,
  convRule: rule22
}, {
  start: 367,
  length: 1,
  convRule: rule23
}, {
  start: 368,
  length: 1,
  convRule: rule22
}, {
  start: 369,
  length: 1,
  convRule: rule23
}, {
  start: 370,
  length: 1,
  convRule: rule22
}, {
  start: 371,
  length: 1,
  convRule: rule23
}, {
  start: 372,
  length: 1,
  convRule: rule22
}, {
  start: 373,
  length: 1,
  convRule: rule23
}, {
  start: 374,
  length: 1,
  convRule: rule22
}, {
  start: 375,
  length: 1,
  convRule: rule23
}, {
  start: 376,
  length: 1,
  convRule: rule26
}, {
  start: 377,
  length: 1,
  convRule: rule22
}, {
  start: 378,
  length: 1,
  convRule: rule23
}, {
  start: 379,
  length: 1,
  convRule: rule22
}, {
  start: 380,
  length: 1,
  convRule: rule23
}, {
  start: 381,
  length: 1,
  convRule: rule22
}, {
  start: 382,
  length: 1,
  convRule: rule23
}, {
  start: 383,
  length: 1,
  convRule: rule27
}, {
  start: 384,
  length: 1,
  convRule: rule28
}, {
  start: 385,
  length: 1,
  convRule: rule29
}, {
  start: 386,
  length: 1,
  convRule: rule22
}, {
  start: 387,
  length: 1,
  convRule: rule23
}, {
  start: 388,
  length: 1,
  convRule: rule22
}, {
  start: 389,
  length: 1,
  convRule: rule23
}, {
  start: 390,
  length: 1,
  convRule: rule30
}, {
  start: 391,
  length: 1,
  convRule: rule22
}, {
  start: 392,
  length: 1,
  convRule: rule23
}, {
  start: 393,
  length: 2,
  convRule: rule31
}, {
  start: 395,
  length: 1,
  convRule: rule22
}, {
  start: 396,
  length: 1,
  convRule: rule23
}, {
  start: 397,
  length: 1,
  convRule: rule20
}, {
  start: 398,
  length: 1,
  convRule: rule32
}, {
  start: 399,
  length: 1,
  convRule: rule33
}, {
  start: 400,
  length: 1,
  convRule: rule34
}, {
  start: 401,
  length: 1,
  convRule: rule22
}, {
  start: 402,
  length: 1,
  convRule: rule23
}, {
  start: 403,
  length: 1,
  convRule: rule31
}, {
  start: 404,
  length: 1,
  convRule: rule35
}, {
  start: 405,
  length: 1,
  convRule: rule36
}, {
  start: 406,
  length: 1,
  convRule: rule37
}, {
  start: 407,
  length: 1,
  convRule: rule38
}, {
  start: 408,
  length: 1,
  convRule: rule22
}, {
  start: 409,
  length: 1,
  convRule: rule23
}, {
  start: 410,
  length: 1,
  convRule: rule39
}, {
  start: 411,
  length: 1,
  convRule: rule20
}, {
  start: 412,
  length: 1,
  convRule: rule37
}, {
  start: 413,
  length: 1,
  convRule: rule40
}, {
  start: 414,
  length: 1,
  convRule: rule41
}, {
  start: 415,
  length: 1,
  convRule: rule42
}, {
  start: 416,
  length: 1,
  convRule: rule22
}, {
  start: 417,
  length: 1,
  convRule: rule23
}, {
  start: 418,
  length: 1,
  convRule: rule22
}, {
  start: 419,
  length: 1,
  convRule: rule23
}, {
  start: 420,
  length: 1,
  convRule: rule22
}, {
  start: 421,
  length: 1,
  convRule: rule23
}, {
  start: 422,
  length: 1,
  convRule: rule43
}, {
  start: 423,
  length: 1,
  convRule: rule22
}, {
  start: 424,
  length: 1,
  convRule: rule23
}, {
  start: 425,
  length: 1,
  convRule: rule43
}, {
  start: 426,
  length: 2,
  convRule: rule20
}, {
  start: 428,
  length: 1,
  convRule: rule22
}, {
  start: 429,
  length: 1,
  convRule: rule23
}, {
  start: 430,
  length: 1,
  convRule: rule43
}, {
  start: 431,
  length: 1,
  convRule: rule22
}, {
  start: 432,
  length: 1,
  convRule: rule23
}, {
  start: 433,
  length: 2,
  convRule: rule44
}, {
  start: 435,
  length: 1,
  convRule: rule22
}, {
  start: 436,
  length: 1,
  convRule: rule23
}, {
  start: 437,
  length: 1,
  convRule: rule22
}, {
  start: 438,
  length: 1,
  convRule: rule23
}, {
  start: 439,
  length: 1,
  convRule: rule45
}, {
  start: 440,
  length: 1,
  convRule: rule22
}, {
  start: 441,
  length: 1,
  convRule: rule23
}, {
  start: 442,
  length: 1,
  convRule: rule20
}, {
  start: 443,
  length: 1,
  convRule: rule14
}, {
  start: 444,
  length: 1,
  convRule: rule22
}, {
  start: 445,
  length: 1,
  convRule: rule23
}, {
  start: 446,
  length: 1,
  convRule: rule20
}, {
  start: 447,
  length: 1,
  convRule: rule46
}, {
  start: 448,
  length: 4,
  convRule: rule14
}, {
  start: 452,
  length: 1,
  convRule: rule47
}, {
  start: 453,
  length: 1,
  convRule: rule48
}, {
  start: 454,
  length: 1,
  convRule: rule49
}, {
  start: 455,
  length: 1,
  convRule: rule47
}, {
  start: 456,
  length: 1,
  convRule: rule48
}, {
  start: 457,
  length: 1,
  convRule: rule49
}, {
  start: 458,
  length: 1,
  convRule: rule47
}, {
  start: 459,
  length: 1,
  convRule: rule48
}, {
  start: 460,
  length: 1,
  convRule: rule49
}, {
  start: 461,
  length: 1,
  convRule: rule22
}, {
  start: 462,
  length: 1,
  convRule: rule23
}, {
  start: 463,
  length: 1,
  convRule: rule22
}, {
  start: 464,
  length: 1,
  convRule: rule23
}, {
  start: 465,
  length: 1,
  convRule: rule22
}, {
  start: 466,
  length: 1,
  convRule: rule23
}, {
  start: 467,
  length: 1,
  convRule: rule22
}, {
  start: 468,
  length: 1,
  convRule: rule23
}, {
  start: 469,
  length: 1,
  convRule: rule22
}, {
  start: 470,
  length: 1,
  convRule: rule23
}, {
  start: 471,
  length: 1,
  convRule: rule22
}, {
  start: 472,
  length: 1,
  convRule: rule23
}, {
  start: 473,
  length: 1,
  convRule: rule22
}, {
  start: 474,
  length: 1,
  convRule: rule23
}, {
  start: 475,
  length: 1,
  convRule: rule22
}, {
  start: 476,
  length: 1,
  convRule: rule23
}, {
  start: 477,
  length: 1,
  convRule: rule50
}, {
  start: 478,
  length: 1,
  convRule: rule22
}, {
  start: 479,
  length: 1,
  convRule: rule23
}, {
  start: 480,
  length: 1,
  convRule: rule22
}, {
  start: 481,
  length: 1,
  convRule: rule23
}, {
  start: 482,
  length: 1,
  convRule: rule22
}, {
  start: 483,
  length: 1,
  convRule: rule23
}, {
  start: 484,
  length: 1,
  convRule: rule22
}, {
  start: 485,
  length: 1,
  convRule: rule23
}, {
  start: 486,
  length: 1,
  convRule: rule22
}, {
  start: 487,
  length: 1,
  convRule: rule23
}, {
  start: 488,
  length: 1,
  convRule: rule22
}, {
  start: 489,
  length: 1,
  convRule: rule23
}, {
  start: 490,
  length: 1,
  convRule: rule22
}, {
  start: 491,
  length: 1,
  convRule: rule23
}, {
  start: 492,
  length: 1,
  convRule: rule22
}, {
  start: 493,
  length: 1,
  convRule: rule23
}, {
  start: 494,
  length: 1,
  convRule: rule22
}, {
  start: 495,
  length: 1,
  convRule: rule23
}, {
  start: 496,
  length: 1,
  convRule: rule20
}, {
  start: 497,
  length: 1,
  convRule: rule47
}, {
  start: 498,
  length: 1,
  convRule: rule48
}, {
  start: 499,
  length: 1,
  convRule: rule49
}, {
  start: 500,
  length: 1,
  convRule: rule22
}, {
  start: 501,
  length: 1,
  convRule: rule23
}, {
  start: 502,
  length: 1,
  convRule: rule51
}, {
  start: 503,
  length: 1,
  convRule: rule52
}, {
  start: 504,
  length: 1,
  convRule: rule22
}, {
  start: 505,
  length: 1,
  convRule: rule23
}, {
  start: 506,
  length: 1,
  convRule: rule22
}, {
  start: 507,
  length: 1,
  convRule: rule23
}, {
  start: 508,
  length: 1,
  convRule: rule22
}, {
  start: 509,
  length: 1,
  convRule: rule23
}, {
  start: 510,
  length: 1,
  convRule: rule22
}, {
  start: 511,
  length: 1,
  convRule: rule23
}, {
  start: 512,
  length: 1,
  convRule: rule22
}, {
  start: 513,
  length: 1,
  convRule: rule23
}, {
  start: 514,
  length: 1,
  convRule: rule22
}, {
  start: 515,
  length: 1,
  convRule: rule23
}, {
  start: 516,
  length: 1,
  convRule: rule22
}, {
  start: 517,
  length: 1,
  convRule: rule23
}, {
  start: 518,
  length: 1,
  convRule: rule22
}, {
  start: 519,
  length: 1,
  convRule: rule23
}, {
  start: 520,
  length: 1,
  convRule: rule22
}, {
  start: 521,
  length: 1,
  convRule: rule23
}, {
  start: 522,
  length: 1,
  convRule: rule22
}, {
  start: 523,
  length: 1,
  convRule: rule23
}, {
  start: 524,
  length: 1,
  convRule: rule22
}, {
  start: 525,
  length: 1,
  convRule: rule23
}, {
  start: 526,
  length: 1,
  convRule: rule22
}, {
  start: 527,
  length: 1,
  convRule: rule23
}, {
  start: 528,
  length: 1,
  convRule: rule22
}, {
  start: 529,
  length: 1,
  convRule: rule23
}, {
  start: 530,
  length: 1,
  convRule: rule22
}, {
  start: 531,
  length: 1,
  convRule: rule23
}, {
  start: 532,
  length: 1,
  convRule: rule22
}, {
  start: 533,
  length: 1,
  convRule: rule23
}, {
  start: 534,
  length: 1,
  convRule: rule22
}, {
  start: 535,
  length: 1,
  convRule: rule23
}, {
  start: 536,
  length: 1,
  convRule: rule22
}, {
  start: 537,
  length: 1,
  convRule: rule23
}, {
  start: 538,
  length: 1,
  convRule: rule22
}, {
  start: 539,
  length: 1,
  convRule: rule23
}, {
  start: 540,
  length: 1,
  convRule: rule22
}, {
  start: 541,
  length: 1,
  convRule: rule23
}, {
  start: 542,
  length: 1,
  convRule: rule22
}, {
  start: 543,
  length: 1,
  convRule: rule23
}, {
  start: 544,
  length: 1,
  convRule: rule53
}, {
  start: 545,
  length: 1,
  convRule: rule20
}, {
  start: 546,
  length: 1,
  convRule: rule22
}, {
  start: 547,
  length: 1,
  convRule: rule23
}, {
  start: 548,
  length: 1,
  convRule: rule22
}, {
  start: 549,
  length: 1,
  convRule: rule23
}, {
  start: 550,
  length: 1,
  convRule: rule22
}, {
  start: 551,
  length: 1,
  convRule: rule23
}, {
  start: 552,
  length: 1,
  convRule: rule22
}, {
  start: 553,
  length: 1,
  convRule: rule23
}, {
  start: 554,
  length: 1,
  convRule: rule22
}, {
  start: 555,
  length: 1,
  convRule: rule23
}, {
  start: 556,
  length: 1,
  convRule: rule22
}, {
  start: 557,
  length: 1,
  convRule: rule23
}, {
  start: 558,
  length: 1,
  convRule: rule22
}, {
  start: 559,
  length: 1,
  convRule: rule23
}, {
  start: 560,
  length: 1,
  convRule: rule22
}, {
  start: 561,
  length: 1,
  convRule: rule23
}, {
  start: 562,
  length: 1,
  convRule: rule22
}, {
  start: 563,
  length: 1,
  convRule: rule23
}, {
  start: 564,
  length: 6,
  convRule: rule20
}, {
  start: 570,
  length: 1,
  convRule: rule54
}, {
  start: 571,
  length: 1,
  convRule: rule22
}, {
  start: 572,
  length: 1,
  convRule: rule23
}, {
  start: 573,
  length: 1,
  convRule: rule55
}, {
  start: 574,
  length: 1,
  convRule: rule56
}, {
  start: 575,
  length: 2,
  convRule: rule57
}, {
  start: 577,
  length: 1,
  convRule: rule22
}, {
  start: 578,
  length: 1,
  convRule: rule23
}, {
  start: 579,
  length: 1,
  convRule: rule58
}, {
  start: 580,
  length: 1,
  convRule: rule59
}, {
  start: 581,
  length: 1,
  convRule: rule60
}, {
  start: 582,
  length: 1,
  convRule: rule22
}, {
  start: 583,
  length: 1,
  convRule: rule23
}, {
  start: 584,
  length: 1,
  convRule: rule22
}, {
  start: 585,
  length: 1,
  convRule: rule23
}, {
  start: 586,
  length: 1,
  convRule: rule22
}, {
  start: 587,
  length: 1,
  convRule: rule23
}, {
  start: 588,
  length: 1,
  convRule: rule22
}, {
  start: 589,
  length: 1,
  convRule: rule23
}, {
  start: 590,
  length: 1,
  convRule: rule22
}, {
  start: 591,
  length: 1,
  convRule: rule23
}, {
  start: 592,
  length: 1,
  convRule: rule61
}, {
  start: 593,
  length: 1,
  convRule: rule62
}, {
  start: 594,
  length: 1,
  convRule: rule63
}, {
  start: 595,
  length: 1,
  convRule: rule64
}, {
  start: 596,
  length: 1,
  convRule: rule65
}, {
  start: 597,
  length: 1,
  convRule: rule20
}, {
  start: 598,
  length: 2,
  convRule: rule66
}, {
  start: 600,
  length: 1,
  convRule: rule20
}, {
  start: 601,
  length: 1,
  convRule: rule67
}, {
  start: 602,
  length: 1,
  convRule: rule20
}, {
  start: 603,
  length: 1,
  convRule: rule68
}, {
  start: 604,
  length: 1,
  convRule: rule69
}, {
  start: 605,
  length: 3,
  convRule: rule20
}, {
  start: 608,
  length: 1,
  convRule: rule66
}, {
  start: 609,
  length: 1,
  convRule: rule70
}, {
  start: 610,
  length: 1,
  convRule: rule20
}, {
  start: 611,
  length: 1,
  convRule: rule71
}, {
  start: 612,
  length: 1,
  convRule: rule20
}, {
  start: 613,
  length: 1,
  convRule: rule72
}, {
  start: 614,
  length: 1,
  convRule: rule73
}, {
  start: 615,
  length: 1,
  convRule: rule20
}, {
  start: 616,
  length: 1,
  convRule: rule74
}, {
  start: 617,
  length: 1,
  convRule: rule75
}, {
  start: 618,
  length: 1,
  convRule: rule73
}, {
  start: 619,
  length: 1,
  convRule: rule76
}, {
  start: 620,
  length: 1,
  convRule: rule77
}, {
  start: 621,
  length: 2,
  convRule: rule20
}, {
  start: 623,
  length: 1,
  convRule: rule75
}, {
  start: 624,
  length: 1,
  convRule: rule20
}, {
  start: 625,
  length: 1,
  convRule: rule78
}, {
  start: 626,
  length: 1,
  convRule: rule79
}, {
  start: 627,
  length: 2,
  convRule: rule20
}, {
  start: 629,
  length: 1,
  convRule: rule80
}, {
  start: 630,
  length: 7,
  convRule: rule20
}, {
  start: 637,
  length: 1,
  convRule: rule81
}, {
  start: 638,
  length: 2,
  convRule: rule20
}, {
  start: 640,
  length: 1,
  convRule: rule82
}, {
  start: 641,
  length: 1,
  convRule: rule20
}, {
  start: 642,
  length: 1,
  convRule: rule83
}, {
  start: 643,
  length: 1,
  convRule: rule82
}, {
  start: 644,
  length: 3,
  convRule: rule20
}, {
  start: 647,
  length: 1,
  convRule: rule84
}, {
  start: 648,
  length: 1,
  convRule: rule82
}, {
  start: 649,
  length: 1,
  convRule: rule85
}, {
  start: 650,
  length: 2,
  convRule: rule86
}, {
  start: 652,
  length: 1,
  convRule: rule87
}, {
  start: 653,
  length: 5,
  convRule: rule20
}, {
  start: 658,
  length: 1,
  convRule: rule88
}, {
  start: 659,
  length: 1,
  convRule: rule20
}, {
  start: 660,
  length: 1,
  convRule: rule14
}, {
  start: 661,
  length: 8,
  convRule: rule20
}, {
  start: 669,
  length: 1,
  convRule: rule89
}, {
  start: 670,
  length: 1,
  convRule: rule90
}, {
  start: 671,
  length: 17,
  convRule: rule20
}, {
  start: 688,
  length: 18,
  convRule: rule91
}, {
  start: 706,
  length: 4,
  convRule: rule10
}, {
  start: 710,
  length: 12,
  convRule: rule91
}, {
  start: 722,
  length: 14,
  convRule: rule10
}, {
  start: 736,
  length: 5,
  convRule: rule91
}, {
  start: 741,
  length: 7,
  convRule: rule10
}, {
  start: 748,
  length: 1,
  convRule: rule91
}, {
  start: 749,
  length: 1,
  convRule: rule10
}, {
  start: 750,
  length: 1,
  convRule: rule91
}, {
  start: 751,
  length: 17,
  convRule: rule10
}, {
  start: 768,
  length: 69,
  convRule: rule92
}, {
  start: 837,
  length: 1,
  convRule: rule93
}, {
  start: 838,
  length: 42,
  convRule: rule92
}, {
  start: 880,
  length: 1,
  convRule: rule22
}, {
  start: 881,
  length: 1,
  convRule: rule23
}, {
  start: 882,
  length: 1,
  convRule: rule22
}, {
  start: 883,
  length: 1,
  convRule: rule23
}, {
  start: 884,
  length: 1,
  convRule: rule91
}, {
  start: 885,
  length: 1,
  convRule: rule10
}, {
  start: 886,
  length: 1,
  convRule: rule22
}, {
  start: 887,
  length: 1,
  convRule: rule23
}, {
  start: 890,
  length: 1,
  convRule: rule91
}, {
  start: 891,
  length: 3,
  convRule: rule41
}, {
  start: 894,
  length: 1,
  convRule: rule2
}, {
  start: 895,
  length: 1,
  convRule: rule94
}, {
  start: 900,
  length: 2,
  convRule: rule10
}, {
  start: 902,
  length: 1,
  convRule: rule95
}, {
  start: 903,
  length: 1,
  convRule: rule2
}, {
  start: 904,
  length: 3,
  convRule: rule96
}, {
  start: 908,
  length: 1,
  convRule: rule97
}, {
  start: 910,
  length: 2,
  convRule: rule98
}, {
  start: 912,
  length: 1,
  convRule: rule20
}, {
  start: 913,
  length: 17,
  convRule: rule9
}, {
  start: 931,
  length: 9,
  convRule: rule9
}, {
  start: 940,
  length: 1,
  convRule: rule99
}, {
  start: 941,
  length: 3,
  convRule: rule100
}, {
  start: 944,
  length: 1,
  convRule: rule20
}, {
  start: 945,
  length: 17,
  convRule: rule12
}, {
  start: 962,
  length: 1,
  convRule: rule101
}, {
  start: 963,
  length: 9,
  convRule: rule12
}, {
  start: 972,
  length: 1,
  convRule: rule102
}, {
  start: 973,
  length: 2,
  convRule: rule103
}, {
  start: 975,
  length: 1,
  convRule: rule104
}, {
  start: 976,
  length: 1,
  convRule: rule105
}, {
  start: 977,
  length: 1,
  convRule: rule106
}, {
  start: 978,
  length: 3,
  convRule: rule107
}, {
  start: 981,
  length: 1,
  convRule: rule108
}, {
  start: 982,
  length: 1,
  convRule: rule109
}, {
  start: 983,
  length: 1,
  convRule: rule110
}, {
  start: 984,
  length: 1,
  convRule: rule22
}, {
  start: 985,
  length: 1,
  convRule: rule23
}, {
  start: 986,
  length: 1,
  convRule: rule22
}, {
  start: 987,
  length: 1,
  convRule: rule23
}, {
  start: 988,
  length: 1,
  convRule: rule22
}, {
  start: 989,
  length: 1,
  convRule: rule23
}, {
  start: 990,
  length: 1,
  convRule: rule22
}, {
  start: 991,
  length: 1,
  convRule: rule23
}, {
  start: 992,
  length: 1,
  convRule: rule22
}, {
  start: 993,
  length: 1,
  convRule: rule23
}, {
  start: 994,
  length: 1,
  convRule: rule22
}, {
  start: 995,
  length: 1,
  convRule: rule23
}, {
  start: 996,
  length: 1,
  convRule: rule22
}, {
  start: 997,
  length: 1,
  convRule: rule23
}, {
  start: 998,
  length: 1,
  convRule: rule22
}, {
  start: 999,
  length: 1,
  convRule: rule23
}, {
  start: 1e3,
  length: 1,
  convRule: rule22
}, {
  start: 1001,
  length: 1,
  convRule: rule23
}, {
  start: 1002,
  length: 1,
  convRule: rule22
}, {
  start: 1003,
  length: 1,
  convRule: rule23
}, {
  start: 1004,
  length: 1,
  convRule: rule22
}, {
  start: 1005,
  length: 1,
  convRule: rule23
}, {
  start: 1006,
  length: 1,
  convRule: rule22
}, {
  start: 1007,
  length: 1,
  convRule: rule23
}, {
  start: 1008,
  length: 1,
  convRule: rule111
}, {
  start: 1009,
  length: 1,
  convRule: rule112
}, {
  start: 1010,
  length: 1,
  convRule: rule113
}, {
  start: 1011,
  length: 1,
  convRule: rule114
}, {
  start: 1012,
  length: 1,
  convRule: rule115
}, {
  start: 1013,
  length: 1,
  convRule: rule116
}, {
  start: 1014,
  length: 1,
  convRule: rule6
}, {
  start: 1015,
  length: 1,
  convRule: rule22
}, {
  start: 1016,
  length: 1,
  convRule: rule23
}, {
  start: 1017,
  length: 1,
  convRule: rule117
}, {
  start: 1018,
  length: 1,
  convRule: rule22
}, {
  start: 1019,
  length: 1,
  convRule: rule23
}, {
  start: 1020,
  length: 1,
  convRule: rule20
}, {
  start: 1021,
  length: 3,
  convRule: rule53
}, {
  start: 1024,
  length: 16,
  convRule: rule118
}, {
  start: 1040,
  length: 32,
  convRule: rule9
}, {
  start: 1072,
  length: 32,
  convRule: rule12
}, {
  start: 1104,
  length: 16,
  convRule: rule112
}, {
  start: 1120,
  length: 1,
  convRule: rule22
}, {
  start: 1121,
  length: 1,
  convRule: rule23
}, {
  start: 1122,
  length: 1,
  convRule: rule22
}, {
  start: 1123,
  length: 1,
  convRule: rule23
}, {
  start: 1124,
  length: 1,
  convRule: rule22
}, {
  start: 1125,
  length: 1,
  convRule: rule23
}, {
  start: 1126,
  length: 1,
  convRule: rule22
}, {
  start: 1127,
  length: 1,
  convRule: rule23
}, {
  start: 1128,
  length: 1,
  convRule: rule22
}, {
  start: 1129,
  length: 1,
  convRule: rule23
}, {
  start: 1130,
  length: 1,
  convRule: rule22
}, {
  start: 1131,
  length: 1,
  convRule: rule23
}, {
  start: 1132,
  length: 1,
  convRule: rule22
}, {
  start: 1133,
  length: 1,
  convRule: rule23
}, {
  start: 1134,
  length: 1,
  convRule: rule22
}, {
  start: 1135,
  length: 1,
  convRule: rule23
}, {
  start: 1136,
  length: 1,
  convRule: rule22
}, {
  start: 1137,
  length: 1,
  convRule: rule23
}, {
  start: 1138,
  length: 1,
  convRule: rule22
}, {
  start: 1139,
  length: 1,
  convRule: rule23
}, {
  start: 1140,
  length: 1,
  convRule: rule22
}, {
  start: 1141,
  length: 1,
  convRule: rule23
}, {
  start: 1142,
  length: 1,
  convRule: rule22
}, {
  start: 1143,
  length: 1,
  convRule: rule23
}, {
  start: 1144,
  length: 1,
  convRule: rule22
}, {
  start: 1145,
  length: 1,
  convRule: rule23
}, {
  start: 1146,
  length: 1,
  convRule: rule22
}, {
  start: 1147,
  length: 1,
  convRule: rule23
}, {
  start: 1148,
  length: 1,
  convRule: rule22
}, {
  start: 1149,
  length: 1,
  convRule: rule23
}, {
  start: 1150,
  length: 1,
  convRule: rule22
}, {
  start: 1151,
  length: 1,
  convRule: rule23
}, {
  start: 1152,
  length: 1,
  convRule: rule22
}, {
  start: 1153,
  length: 1,
  convRule: rule23
}, {
  start: 1154,
  length: 1,
  convRule: rule13
}, {
  start: 1155,
  length: 5,
  convRule: rule92
}, {
  start: 1160,
  length: 2,
  convRule: rule119
}, {
  start: 1162,
  length: 1,
  convRule: rule22
}, {
  start: 1163,
  length: 1,
  convRule: rule23
}, {
  start: 1164,
  length: 1,
  convRule: rule22
}, {
  start: 1165,
  length: 1,
  convRule: rule23
}, {
  start: 1166,
  length: 1,
  convRule: rule22
}, {
  start: 1167,
  length: 1,
  convRule: rule23
}, {
  start: 1168,
  length: 1,
  convRule: rule22
}, {
  start: 1169,
  length: 1,
  convRule: rule23
}, {
  start: 1170,
  length: 1,
  convRule: rule22
}, {
  start: 1171,
  length: 1,
  convRule: rule23
}, {
  start: 1172,
  length: 1,
  convRule: rule22
}, {
  start: 1173,
  length: 1,
  convRule: rule23
}, {
  start: 1174,
  length: 1,
  convRule: rule22
}, {
  start: 1175,
  length: 1,
  convRule: rule23
}, {
  start: 1176,
  length: 1,
  convRule: rule22
}, {
  start: 1177,
  length: 1,
  convRule: rule23
}, {
  start: 1178,
  length: 1,
  convRule: rule22
}, {
  start: 1179,
  length: 1,
  convRule: rule23
}, {
  start: 1180,
  length: 1,
  convRule: rule22
}, {
  start: 1181,
  length: 1,
  convRule: rule23
}, {
  start: 1182,
  length: 1,
  convRule: rule22
}, {
  start: 1183,
  length: 1,
  convRule: rule23
}, {
  start: 1184,
  length: 1,
  convRule: rule22
}, {
  start: 1185,
  length: 1,
  convRule: rule23
}, {
  start: 1186,
  length: 1,
  convRule: rule22
}, {
  start: 1187,
  length: 1,
  convRule: rule23
}, {
  start: 1188,
  length: 1,
  convRule: rule22
}, {
  start: 1189,
  length: 1,
  convRule: rule23
}, {
  start: 1190,
  length: 1,
  convRule: rule22
}, {
  start: 1191,
  length: 1,
  convRule: rule23
}, {
  start: 1192,
  length: 1,
  convRule: rule22
}, {
  start: 1193,
  length: 1,
  convRule: rule23
}, {
  start: 1194,
  length: 1,
  convRule: rule22
}, {
  start: 1195,
  length: 1,
  convRule: rule23
}, {
  start: 1196,
  length: 1,
  convRule: rule22
}, {
  start: 1197,
  length: 1,
  convRule: rule23
}, {
  start: 1198,
  length: 1,
  convRule: rule22
}, {
  start: 1199,
  length: 1,
  convRule: rule23
}, {
  start: 1200,
  length: 1,
  convRule: rule22
}, {
  start: 1201,
  length: 1,
  convRule: rule23
}, {
  start: 1202,
  length: 1,
  convRule: rule22
}, {
  start: 1203,
  length: 1,
  convRule: rule23
}, {
  start: 1204,
  length: 1,
  convRule: rule22
}, {
  start: 1205,
  length: 1,
  convRule: rule23
}, {
  start: 1206,
  length: 1,
  convRule: rule22
}, {
  start: 1207,
  length: 1,
  convRule: rule23
}, {
  start: 1208,
  length: 1,
  convRule: rule22
}, {
  start: 1209,
  length: 1,
  convRule: rule23
}, {
  start: 1210,
  length: 1,
  convRule: rule22
}, {
  start: 1211,
  length: 1,
  convRule: rule23
}, {
  start: 1212,
  length: 1,
  convRule: rule22
}, {
  start: 1213,
  length: 1,
  convRule: rule23
}, {
  start: 1214,
  length: 1,
  convRule: rule22
}, {
  start: 1215,
  length: 1,
  convRule: rule23
}, {
  start: 1216,
  length: 1,
  convRule: rule120
}, {
  start: 1217,
  length: 1,
  convRule: rule22
}, {
  start: 1218,
  length: 1,
  convRule: rule23
}, {
  start: 1219,
  length: 1,
  convRule: rule22
}, {
  start: 1220,
  length: 1,
  convRule: rule23
}, {
  start: 1221,
  length: 1,
  convRule: rule22
}, {
  start: 1222,
  length: 1,
  convRule: rule23
}, {
  start: 1223,
  length: 1,
  convRule: rule22
}, {
  start: 1224,
  length: 1,
  convRule: rule23
}, {
  start: 1225,
  length: 1,
  convRule: rule22
}, {
  start: 1226,
  length: 1,
  convRule: rule23
}, {
  start: 1227,
  length: 1,
  convRule: rule22
}, {
  start: 1228,
  length: 1,
  convRule: rule23
}, {
  start: 1229,
  length: 1,
  convRule: rule22
}, {
  start: 1230,
  length: 1,
  convRule: rule23
}, {
  start: 1231,
  length: 1,
  convRule: rule121
}, {
  start: 1232,
  length: 1,
  convRule: rule22
}, {
  start: 1233,
  length: 1,
  convRule: rule23
}, {
  start: 1234,
  length: 1,
  convRule: rule22
}, {
  start: 1235,
  length: 1,
  convRule: rule23
}, {
  start: 1236,
  length: 1,
  convRule: rule22
}, {
  start: 1237,
  length: 1,
  convRule: rule23
}, {
  start: 1238,
  length: 1,
  convRule: rule22
}, {
  start: 1239,
  length: 1,
  convRule: rule23
}, {
  start: 1240,
  length: 1,
  convRule: rule22
}, {
  start: 1241,
  length: 1,
  convRule: rule23
}, {
  start: 1242,
  length: 1,
  convRule: rule22
}, {
  start: 1243,
  length: 1,
  convRule: rule23
}, {
  start: 1244,
  length: 1,
  convRule: rule22
}, {
  start: 1245,
  length: 1,
  convRule: rule23
}, {
  start: 1246,
  length: 1,
  convRule: rule22
}, {
  start: 1247,
  length: 1,
  convRule: rule23
}, {
  start: 1248,
  length: 1,
  convRule: rule22
}, {
  start: 1249,
  length: 1,
  convRule: rule23
}, {
  start: 1250,
  length: 1,
  convRule: rule22
}, {
  start: 1251,
  length: 1,
  convRule: rule23
}, {
  start: 1252,
  length: 1,
  convRule: rule22
}, {
  start: 1253,
  length: 1,
  convRule: rule23
}, {
  start: 1254,
  length: 1,
  convRule: rule22
}, {
  start: 1255,
  length: 1,
  convRule: rule23
}, {
  start: 1256,
  length: 1,
  convRule: rule22
}, {
  start: 1257,
  length: 1,
  convRule: rule23
}, {
  start: 1258,
  length: 1,
  convRule: rule22
}, {
  start: 1259,
  length: 1,
  convRule: rule23
}, {
  start: 1260,
  length: 1,
  convRule: rule22
}, {
  start: 1261,
  length: 1,
  convRule: rule23
}, {
  start: 1262,
  length: 1,
  convRule: rule22
}, {
  start: 1263,
  length: 1,
  convRule: rule23
}, {
  start: 1264,
  length: 1,
  convRule: rule22
}, {
  start: 1265,
  length: 1,
  convRule: rule23
}, {
  start: 1266,
  length: 1,
  convRule: rule22
}, {
  start: 1267,
  length: 1,
  convRule: rule23
}, {
  start: 1268,
  length: 1,
  convRule: rule22
}, {
  start: 1269,
  length: 1,
  convRule: rule23
}, {
  start: 1270,
  length: 1,
  convRule: rule22
}, {
  start: 1271,
  length: 1,
  convRule: rule23
}, {
  start: 1272,
  length: 1,
  convRule: rule22
}, {
  start: 1273,
  length: 1,
  convRule: rule23
}, {
  start: 1274,
  length: 1,
  convRule: rule22
}, {
  start: 1275,
  length: 1,
  convRule: rule23
}, {
  start: 1276,
  length: 1,
  convRule: rule22
}, {
  start: 1277,
  length: 1,
  convRule: rule23
}, {
  start: 1278,
  length: 1,
  convRule: rule22
}, {
  start: 1279,
  length: 1,
  convRule: rule23
}, {
  start: 1280,
  length: 1,
  convRule: rule22
}, {
  start: 1281,
  length: 1,
  convRule: rule23
}, {
  start: 1282,
  length: 1,
  convRule: rule22
}, {
  start: 1283,
  length: 1,
  convRule: rule23
}, {
  start: 1284,
  length: 1,
  convRule: rule22
}, {
  start: 1285,
  length: 1,
  convRule: rule23
}, {
  start: 1286,
  length: 1,
  convRule: rule22
}, {
  start: 1287,
  length: 1,
  convRule: rule23
}, {
  start: 1288,
  length: 1,
  convRule: rule22
}, {
  start: 1289,
  length: 1,
  convRule: rule23
}, {
  start: 1290,
  length: 1,
  convRule: rule22
}, {
  start: 1291,
  length: 1,
  convRule: rule23
}, {
  start: 1292,
  length: 1,
  convRule: rule22
}, {
  start: 1293,
  length: 1,
  convRule: rule23
}, {
  start: 1294,
  length: 1,
  convRule: rule22
}, {
  start: 1295,
  length: 1,
  convRule: rule23
}, {
  start: 1296,
  length: 1,
  convRule: rule22
}, {
  start: 1297,
  length: 1,
  convRule: rule23
}, {
  start: 1298,
  length: 1,
  convRule: rule22
}, {
  start: 1299,
  length: 1,
  convRule: rule23
}, {
  start: 1300,
  length: 1,
  convRule: rule22
}, {
  start: 1301,
  length: 1,
  convRule: rule23
}, {
  start: 1302,
  length: 1,
  convRule: rule22
}, {
  start: 1303,
  length: 1,
  convRule: rule23
}, {
  start: 1304,
  length: 1,
  convRule: rule22
}, {
  start: 1305,
  length: 1,
  convRule: rule23
}, {
  start: 1306,
  length: 1,
  convRule: rule22
}, {
  start: 1307,
  length: 1,
  convRule: rule23
}, {
  start: 1308,
  length: 1,
  convRule: rule22
}, {
  start: 1309,
  length: 1,
  convRule: rule23
}, {
  start: 1310,
  length: 1,
  convRule: rule22
}, {
  start: 1311,
  length: 1,
  convRule: rule23
}, {
  start: 1312,
  length: 1,
  convRule: rule22
}, {
  start: 1313,
  length: 1,
  convRule: rule23
}, {
  start: 1314,
  length: 1,
  convRule: rule22
}, {
  start: 1315,
  length: 1,
  convRule: rule23
}, {
  start: 1316,
  length: 1,
  convRule: rule22
}, {
  start: 1317,
  length: 1,
  convRule: rule23
}, {
  start: 1318,
  length: 1,
  convRule: rule22
}, {
  start: 1319,
  length: 1,
  convRule: rule23
}, {
  start: 1320,
  length: 1,
  convRule: rule22
}, {
  start: 1321,
  length: 1,
  convRule: rule23
}, {
  start: 1322,
  length: 1,
  convRule: rule22
}, {
  start: 1323,
  length: 1,
  convRule: rule23
}, {
  start: 1324,
  length: 1,
  convRule: rule22
}, {
  start: 1325,
  length: 1,
  convRule: rule23
}, {
  start: 1326,
  length: 1,
  convRule: rule22
}, {
  start: 1327,
  length: 1,
  convRule: rule23
}, {
  start: 1329,
  length: 38,
  convRule: rule122
}, {
  start: 1369,
  length: 1,
  convRule: rule91
}, {
  start: 1370,
  length: 6,
  convRule: rule2
}, {
  start: 1376,
  length: 1,
  convRule: rule20
}, {
  start: 1377,
  length: 38,
  convRule: rule123
}, {
  start: 1415,
  length: 2,
  convRule: rule20
}, {
  start: 1417,
  length: 1,
  convRule: rule2
}, {
  start: 1418,
  length: 1,
  convRule: rule7
}, {
  start: 1421,
  length: 2,
  convRule: rule13
}, {
  start: 1423,
  length: 1,
  convRule: rule3
}, {
  start: 1425,
  length: 45,
  convRule: rule92
}, {
  start: 1470,
  length: 1,
  convRule: rule7
}, {
  start: 1471,
  length: 1,
  convRule: rule92
}, {
  start: 1472,
  length: 1,
  convRule: rule2
}, {
  start: 1473,
  length: 2,
  convRule: rule92
}, {
  start: 1475,
  length: 1,
  convRule: rule2
}, {
  start: 1476,
  length: 2,
  convRule: rule92
}, {
  start: 1478,
  length: 1,
  convRule: rule2
}, {
  start: 1479,
  length: 1,
  convRule: rule92
}, {
  start: 1488,
  length: 27,
  convRule: rule14
}, {
  start: 1519,
  length: 4,
  convRule: rule14
}, {
  start: 1523,
  length: 2,
  convRule: rule2
}, {
  start: 1536,
  length: 6,
  convRule: rule16
}, {
  start: 1542,
  length: 3,
  convRule: rule6
}, {
  start: 1545,
  length: 2,
  convRule: rule2
}, {
  start: 1547,
  length: 1,
  convRule: rule3
}, {
  start: 1548,
  length: 2,
  convRule: rule2
}, {
  start: 1550,
  length: 2,
  convRule: rule13
}, {
  start: 1552,
  length: 11,
  convRule: rule92
}, {
  start: 1563,
  length: 1,
  convRule: rule2
}, {
  start: 1564,
  length: 1,
  convRule: rule16
}, {
  start: 1566,
  length: 2,
  convRule: rule2
}, {
  start: 1568,
  length: 32,
  convRule: rule14
}, {
  start: 1600,
  length: 1,
  convRule: rule91
}, {
  start: 1601,
  length: 10,
  convRule: rule14
}, {
  start: 1611,
  length: 21,
  convRule: rule92
}, {
  start: 1632,
  length: 10,
  convRule: rule8
}, {
  start: 1642,
  length: 4,
  convRule: rule2
}, {
  start: 1646,
  length: 2,
  convRule: rule14
}, {
  start: 1648,
  length: 1,
  convRule: rule92
}, {
  start: 1649,
  length: 99,
  convRule: rule14
}, {
  start: 1748,
  length: 1,
  convRule: rule2
}, {
  start: 1749,
  length: 1,
  convRule: rule14
}, {
  start: 1750,
  length: 7,
  convRule: rule92
}, {
  start: 1757,
  length: 1,
  convRule: rule16
}, {
  start: 1758,
  length: 1,
  convRule: rule13
}, {
  start: 1759,
  length: 6,
  convRule: rule92
}, {
  start: 1765,
  length: 2,
  convRule: rule91
}, {
  start: 1767,
  length: 2,
  convRule: rule92
}, {
  start: 1769,
  length: 1,
  convRule: rule13
}, {
  start: 1770,
  length: 4,
  convRule: rule92
}, {
  start: 1774,
  length: 2,
  convRule: rule14
}, {
  start: 1776,
  length: 10,
  convRule: rule8
}, {
  start: 1786,
  length: 3,
  convRule: rule14
}, {
  start: 1789,
  length: 2,
  convRule: rule13
}, {
  start: 1791,
  length: 1,
  convRule: rule14
}, {
  start: 1792,
  length: 14,
  convRule: rule2
}, {
  start: 1807,
  length: 1,
  convRule: rule16
}, {
  start: 1808,
  length: 1,
  convRule: rule14
}, {
  start: 1809,
  length: 1,
  convRule: rule92
}, {
  start: 1810,
  length: 30,
  convRule: rule14
}, {
  start: 1840,
  length: 27,
  convRule: rule92
}, {
  start: 1869,
  length: 89,
  convRule: rule14
}, {
  start: 1958,
  length: 11,
  convRule: rule92
}, {
  start: 1969,
  length: 1,
  convRule: rule14
}, {
  start: 1984,
  length: 10,
  convRule: rule8
}, {
  start: 1994,
  length: 33,
  convRule: rule14
}, {
  start: 2027,
  length: 9,
  convRule: rule92
}, {
  start: 2036,
  length: 2,
  convRule: rule91
}, {
  start: 2038,
  length: 1,
  convRule: rule13
}, {
  start: 2039,
  length: 3,
  convRule: rule2
}, {
  start: 2042,
  length: 1,
  convRule: rule91
}, {
  start: 2045,
  length: 1,
  convRule: rule92
}, {
  start: 2046,
  length: 2,
  convRule: rule3
}, {
  start: 2048,
  length: 22,
  convRule: rule14
}, {
  start: 2070,
  length: 4,
  convRule: rule92
}, {
  start: 2074,
  length: 1,
  convRule: rule91
}, {
  start: 2075,
  length: 9,
  convRule: rule92
}, {
  start: 2084,
  length: 1,
  convRule: rule91
}, {
  start: 2085,
  length: 3,
  convRule: rule92
}, {
  start: 2088,
  length: 1,
  convRule: rule91
}, {
  start: 2089,
  length: 5,
  convRule: rule92
}, {
  start: 2096,
  length: 15,
  convRule: rule2
}, {
  start: 2112,
  length: 25,
  convRule: rule14
}, {
  start: 2137,
  length: 3,
  convRule: rule92
}, {
  start: 2142,
  length: 1,
  convRule: rule2
}, {
  start: 2144,
  length: 11,
  convRule: rule14
}, {
  start: 2208,
  length: 21,
  convRule: rule14
}, {
  start: 2230,
  length: 18,
  convRule: rule14
}, {
  start: 2259,
  length: 15,
  convRule: rule92
}, {
  start: 2274,
  length: 1,
  convRule: rule16
}, {
  start: 2275,
  length: 32,
  convRule: rule92
}, {
  start: 2307,
  length: 1,
  convRule: rule124
}, {
  start: 2308,
  length: 54,
  convRule: rule14
}, {
  start: 2362,
  length: 1,
  convRule: rule92
}, {
  start: 2363,
  length: 1,
  convRule: rule124
}, {
  start: 2364,
  length: 1,
  convRule: rule92
}, {
  start: 2365,
  length: 1,
  convRule: rule14
}, {
  start: 2366,
  length: 3,
  convRule: rule124
}, {
  start: 2369,
  length: 8,
  convRule: rule92
}, {
  start: 2377,
  length: 4,
  convRule: rule124
}, {
  start: 2381,
  length: 1,
  convRule: rule92
}, {
  start: 2382,
  length: 2,
  convRule: rule124
}, {
  start: 2384,
  length: 1,
  convRule: rule14
}, {
  start: 2385,
  length: 7,
  convRule: rule92
}, {
  start: 2392,
  length: 10,
  convRule: rule14
}, {
  start: 2402,
  length: 2,
  convRule: rule92
}, {
  start: 2404,
  length: 2,
  convRule: rule2
}, {
  start: 2406,
  length: 10,
  convRule: rule8
}, {
  start: 2416,
  length: 1,
  convRule: rule2
}, {
  start: 2417,
  length: 1,
  convRule: rule91
}, {
  start: 2418,
  length: 15,
  convRule: rule14
}, {
  start: 2433,
  length: 1,
  convRule: rule92
}, {
  start: 2434,
  length: 2,
  convRule: rule124
}, {
  start: 2437,
  length: 8,
  convRule: rule14
}, {
  start: 2447,
  length: 2,
  convRule: rule14
}, {
  start: 2451,
  length: 22,
  convRule: rule14
}, {
  start: 2474,
  length: 7,
  convRule: rule14
}, {
  start: 2482,
  length: 1,
  convRule: rule14
}, {
  start: 2486,
  length: 4,
  convRule: rule14
}, {
  start: 2492,
  length: 1,
  convRule: rule92
}, {
  start: 2493,
  length: 1,
  convRule: rule14
}, {
  start: 2494,
  length: 3,
  convRule: rule124
}, {
  start: 2497,
  length: 4,
  convRule: rule92
}, {
  start: 2503,
  length: 2,
  convRule: rule124
}, {
  start: 2507,
  length: 2,
  convRule: rule124
}, {
  start: 2509,
  length: 1,
  convRule: rule92
}, {
  start: 2510,
  length: 1,
  convRule: rule14
}, {
  start: 2519,
  length: 1,
  convRule: rule124
}, {
  start: 2524,
  length: 2,
  convRule: rule14
}, {
  start: 2527,
  length: 3,
  convRule: rule14
}, {
  start: 2530,
  length: 2,
  convRule: rule92
}, {
  start: 2534,
  length: 10,
  convRule: rule8
}, {
  start: 2544,
  length: 2,
  convRule: rule14
}, {
  start: 2546,
  length: 2,
  convRule: rule3
}, {
  start: 2548,
  length: 6,
  convRule: rule17
}, {
  start: 2554,
  length: 1,
  convRule: rule13
}, {
  start: 2555,
  length: 1,
  convRule: rule3
}, {
  start: 2556,
  length: 1,
  convRule: rule14
}, {
  start: 2557,
  length: 1,
  convRule: rule2
}, {
  start: 2558,
  length: 1,
  convRule: rule92
}, {
  start: 2561,
  length: 2,
  convRule: rule92
}, {
  start: 2563,
  length: 1,
  convRule: rule124
}, {
  start: 2565,
  length: 6,
  convRule: rule14
}, {
  start: 2575,
  length: 2,
  convRule: rule14
}, {
  start: 2579,
  length: 22,
  convRule: rule14
}, {
  start: 2602,
  length: 7,
  convRule: rule14
}, {
  start: 2610,
  length: 2,
  convRule: rule14
}, {
  start: 2613,
  length: 2,
  convRule: rule14
}, {
  start: 2616,
  length: 2,
  convRule: rule14
}, {
  start: 2620,
  length: 1,
  convRule: rule92
}, {
  start: 2622,
  length: 3,
  convRule: rule124
}, {
  start: 2625,
  length: 2,
  convRule: rule92
}, {
  start: 2631,
  length: 2,
  convRule: rule92
}, {
  start: 2635,
  length: 3,
  convRule: rule92
}, {
  start: 2641,
  length: 1,
  convRule: rule92
}, {
  start: 2649,
  length: 4,
  convRule: rule14
}, {
  start: 2654,
  length: 1,
  convRule: rule14
}, {
  start: 2662,
  length: 10,
  convRule: rule8
}, {
  start: 2672,
  length: 2,
  convRule: rule92
}, {
  start: 2674,
  length: 3,
  convRule: rule14
}, {
  start: 2677,
  length: 1,
  convRule: rule92
}, {
  start: 2678,
  length: 1,
  convRule: rule2
}, {
  start: 2689,
  length: 2,
  convRule: rule92
}, {
  start: 2691,
  length: 1,
  convRule: rule124
}, {
  start: 2693,
  length: 9,
  convRule: rule14
}, {
  start: 2703,
  length: 3,
  convRule: rule14
}, {
  start: 2707,
  length: 22,
  convRule: rule14
}, {
  start: 2730,
  length: 7,
  convRule: rule14
}, {
  start: 2738,
  length: 2,
  convRule: rule14
}, {
  start: 2741,
  length: 5,
  convRule: rule14
}, {
  start: 2748,
  length: 1,
  convRule: rule92
}, {
  start: 2749,
  length: 1,
  convRule: rule14
}, {
  start: 2750,
  length: 3,
  convRule: rule124
}, {
  start: 2753,
  length: 5,
  convRule: rule92
}, {
  start: 2759,
  length: 2,
  convRule: rule92
}, {
  start: 2761,
  length: 1,
  convRule: rule124
}, {
  start: 2763,
  length: 2,
  convRule: rule124
}, {
  start: 2765,
  length: 1,
  convRule: rule92
}, {
  start: 2768,
  length: 1,
  convRule: rule14
}, {
  start: 2784,
  length: 2,
  convRule: rule14
}, {
  start: 2786,
  length: 2,
  convRule: rule92
}, {
  start: 2790,
  length: 10,
  convRule: rule8
}, {
  start: 2800,
  length: 1,
  convRule: rule2
}, {
  start: 2801,
  length: 1,
  convRule: rule3
}, {
  start: 2809,
  length: 1,
  convRule: rule14
}, {
  start: 2810,
  length: 6,
  convRule: rule92
}, {
  start: 2817,
  length: 1,
  convRule: rule92
}, {
  start: 2818,
  length: 2,
  convRule: rule124
}, {
  start: 2821,
  length: 8,
  convRule: rule14
}, {
  start: 2831,
  length: 2,
  convRule: rule14
}, {
  start: 2835,
  length: 22,
  convRule: rule14
}, {
  start: 2858,
  length: 7,
  convRule: rule14
}, {
  start: 2866,
  length: 2,
  convRule: rule14
}, {
  start: 2869,
  length: 5,
  convRule: rule14
}, {
  start: 2876,
  length: 1,
  convRule: rule92
}, {
  start: 2877,
  length: 1,
  convRule: rule14
}, {
  start: 2878,
  length: 1,
  convRule: rule124
}, {
  start: 2879,
  length: 1,
  convRule: rule92
}, {
  start: 2880,
  length: 1,
  convRule: rule124
}, {
  start: 2881,
  length: 4,
  convRule: rule92
}, {
  start: 2887,
  length: 2,
  convRule: rule124
}, {
  start: 2891,
  length: 2,
  convRule: rule124
}, {
  start: 2893,
  length: 1,
  convRule: rule92
}, {
  start: 2901,
  length: 2,
  convRule: rule92
}, {
  start: 2903,
  length: 1,
  convRule: rule124
}, {
  start: 2908,
  length: 2,
  convRule: rule14
}, {
  start: 2911,
  length: 3,
  convRule: rule14
}, {
  start: 2914,
  length: 2,
  convRule: rule92
}, {
  start: 2918,
  length: 10,
  convRule: rule8
}, {
  start: 2928,
  length: 1,
  convRule: rule13
}, {
  start: 2929,
  length: 1,
  convRule: rule14
}, {
  start: 2930,
  length: 6,
  convRule: rule17
}, {
  start: 2946,
  length: 1,
  convRule: rule92
}, {
  start: 2947,
  length: 1,
  convRule: rule14
}, {
  start: 2949,
  length: 6,
  convRule: rule14
}, {
  start: 2958,
  length: 3,
  convRule: rule14
}, {
  start: 2962,
  length: 4,
  convRule: rule14
}, {
  start: 2969,
  length: 2,
  convRule: rule14
}, {
  start: 2972,
  length: 1,
  convRule: rule14
}, {
  start: 2974,
  length: 2,
  convRule: rule14
}, {
  start: 2979,
  length: 2,
  convRule: rule14
}, {
  start: 2984,
  length: 3,
  convRule: rule14
}, {
  start: 2990,
  length: 12,
  convRule: rule14
}, {
  start: 3006,
  length: 2,
  convRule: rule124
}, {
  start: 3008,
  length: 1,
  convRule: rule92
}, {
  start: 3009,
  length: 2,
  convRule: rule124
}, {
  start: 3014,
  length: 3,
  convRule: rule124
}, {
  start: 3018,
  length: 3,
  convRule: rule124
}, {
  start: 3021,
  length: 1,
  convRule: rule92
}, {
  start: 3024,
  length: 1,
  convRule: rule14
}, {
  start: 3031,
  length: 1,
  convRule: rule124
}, {
  start: 3046,
  length: 10,
  convRule: rule8
}, {
  start: 3056,
  length: 3,
  convRule: rule17
}, {
  start: 3059,
  length: 6,
  convRule: rule13
}, {
  start: 3065,
  length: 1,
  convRule: rule3
}, {
  start: 3066,
  length: 1,
  convRule: rule13
}, {
  start: 3072,
  length: 1,
  convRule: rule92
}, {
  start: 3073,
  length: 3,
  convRule: rule124
}, {
  start: 3076,
  length: 1,
  convRule: rule92
}, {
  start: 3077,
  length: 8,
  convRule: rule14
}, {
  start: 3086,
  length: 3,
  convRule: rule14
}, {
  start: 3090,
  length: 23,
  convRule: rule14
}, {
  start: 3114,
  length: 16,
  convRule: rule14
}, {
  start: 3133,
  length: 1,
  convRule: rule14
}, {
  start: 3134,
  length: 3,
  convRule: rule92
}, {
  start: 3137,
  length: 4,
  convRule: rule124
}, {
  start: 3142,
  length: 3,
  convRule: rule92
}, {
  start: 3146,
  length: 4,
  convRule: rule92
}, {
  start: 3157,
  length: 2,
  convRule: rule92
}, {
  start: 3160,
  length: 3,
  convRule: rule14
}, {
  start: 3168,
  length: 2,
  convRule: rule14
}, {
  start: 3170,
  length: 2,
  convRule: rule92
}, {
  start: 3174,
  length: 10,
  convRule: rule8
}, {
  start: 3191,
  length: 1,
  convRule: rule2
}, {
  start: 3192,
  length: 7,
  convRule: rule17
}, {
  start: 3199,
  length: 1,
  convRule: rule13
}, {
  start: 3200,
  length: 1,
  convRule: rule14
}, {
  start: 3201,
  length: 1,
  convRule: rule92
}, {
  start: 3202,
  length: 2,
  convRule: rule124
}, {
  start: 3204,
  length: 1,
  convRule: rule2
}, {
  start: 3205,
  length: 8,
  convRule: rule14
}, {
  start: 3214,
  length: 3,
  convRule: rule14
}, {
  start: 3218,
  length: 23,
  convRule: rule14
}, {
  start: 3242,
  length: 10,
  convRule: rule14
}, {
  start: 3253,
  length: 5,
  convRule: rule14
}, {
  start: 3260,
  length: 1,
  convRule: rule92
}, {
  start: 3261,
  length: 1,
  convRule: rule14
}, {
  start: 3262,
  length: 1,
  convRule: rule124
}, {
  start: 3263,
  length: 1,
  convRule: rule92
}, {
  start: 3264,
  length: 5,
  convRule: rule124
}, {
  start: 3270,
  length: 1,
  convRule: rule92
}, {
  start: 3271,
  length: 2,
  convRule: rule124
}, {
  start: 3274,
  length: 2,
  convRule: rule124
}, {
  start: 3276,
  length: 2,
  convRule: rule92
}, {
  start: 3285,
  length: 2,
  convRule: rule124
}, {
  start: 3294,
  length: 1,
  convRule: rule14
}, {
  start: 3296,
  length: 2,
  convRule: rule14
}, {
  start: 3298,
  length: 2,
  convRule: rule92
}, {
  start: 3302,
  length: 10,
  convRule: rule8
}, {
  start: 3313,
  length: 2,
  convRule: rule14
}, {
  start: 3328,
  length: 2,
  convRule: rule92
}, {
  start: 3330,
  length: 2,
  convRule: rule124
}, {
  start: 3332,
  length: 9,
  convRule: rule14
}, {
  start: 3342,
  length: 3,
  convRule: rule14
}, {
  start: 3346,
  length: 41,
  convRule: rule14
}, {
  start: 3387,
  length: 2,
  convRule: rule92
}, {
  start: 3389,
  length: 1,
  convRule: rule14
}, {
  start: 3390,
  length: 3,
  convRule: rule124
}, {
  start: 3393,
  length: 4,
  convRule: rule92
}, {
  start: 3398,
  length: 3,
  convRule: rule124
}, {
  start: 3402,
  length: 3,
  convRule: rule124
}, {
  start: 3405,
  length: 1,
  convRule: rule92
}, {
  start: 3406,
  length: 1,
  convRule: rule14
}, {
  start: 3407,
  length: 1,
  convRule: rule13
}, {
  start: 3412,
  length: 3,
  convRule: rule14
}, {
  start: 3415,
  length: 1,
  convRule: rule124
}, {
  start: 3416,
  length: 7,
  convRule: rule17
}, {
  start: 3423,
  length: 3,
  convRule: rule14
}, {
  start: 3426,
  length: 2,
  convRule: rule92
}, {
  start: 3430,
  length: 10,
  convRule: rule8
}, {
  start: 3440,
  length: 9,
  convRule: rule17
}, {
  start: 3449,
  length: 1,
  convRule: rule13
}, {
  start: 3450,
  length: 6,
  convRule: rule14
}, {
  start: 3457,
  length: 1,
  convRule: rule92
}, {
  start: 3458,
  length: 2,
  convRule: rule124
}, {
  start: 3461,
  length: 18,
  convRule: rule14
}, {
  start: 3482,
  length: 24,
  convRule: rule14
}, {
  start: 3507,
  length: 9,
  convRule: rule14
}, {
  start: 3517,
  length: 1,
  convRule: rule14
}, {
  start: 3520,
  length: 7,
  convRule: rule14
}, {
  start: 3530,
  length: 1,
  convRule: rule92
}, {
  start: 3535,
  length: 3,
  convRule: rule124
}, {
  start: 3538,
  length: 3,
  convRule: rule92
}, {
  start: 3542,
  length: 1,
  convRule: rule92
}, {
  start: 3544,
  length: 8,
  convRule: rule124
}, {
  start: 3558,
  length: 10,
  convRule: rule8
}, {
  start: 3570,
  length: 2,
  convRule: rule124
}, {
  start: 3572,
  length: 1,
  convRule: rule2
}, {
  start: 3585,
  length: 48,
  convRule: rule14
}, {
  start: 3633,
  length: 1,
  convRule: rule92
}, {
  start: 3634,
  length: 2,
  convRule: rule14
}, {
  start: 3636,
  length: 7,
  convRule: rule92
}, {
  start: 3647,
  length: 1,
  convRule: rule3
}, {
  start: 3648,
  length: 6,
  convRule: rule14
}, {
  start: 3654,
  length: 1,
  convRule: rule91
}, {
  start: 3655,
  length: 8,
  convRule: rule92
}, {
  start: 3663,
  length: 1,
  convRule: rule2
}, {
  start: 3664,
  length: 10,
  convRule: rule8
}, {
  start: 3674,
  length: 2,
  convRule: rule2
}, {
  start: 3713,
  length: 2,
  convRule: rule14
}, {
  start: 3716,
  length: 1,
  convRule: rule14
}, {
  start: 3718,
  length: 5,
  convRule: rule14
}, {
  start: 3724,
  length: 24,
  convRule: rule14
}, {
  start: 3749,
  length: 1,
  convRule: rule14
}, {
  start: 3751,
  length: 10,
  convRule: rule14
}, {
  start: 3761,
  length: 1,
  convRule: rule92
}, {
  start: 3762,
  length: 2,
  convRule: rule14
}, {
  start: 3764,
  length: 9,
  convRule: rule92
}, {
  start: 3773,
  length: 1,
  convRule: rule14
}, {
  start: 3776,
  length: 5,
  convRule: rule14
}, {
  start: 3782,
  length: 1,
  convRule: rule91
}, {
  start: 3784,
  length: 6,
  convRule: rule92
}, {
  start: 3792,
  length: 10,
  convRule: rule8
}, {
  start: 3804,
  length: 4,
  convRule: rule14
}, {
  start: 3840,
  length: 1,
  convRule: rule14
}, {
  start: 3841,
  length: 3,
  convRule: rule13
}, {
  start: 3844,
  length: 15,
  convRule: rule2
}, {
  start: 3859,
  length: 1,
  convRule: rule13
}, {
  start: 3860,
  length: 1,
  convRule: rule2
}, {
  start: 3861,
  length: 3,
  convRule: rule13
}, {
  start: 3864,
  length: 2,
  convRule: rule92
}, {
  start: 3866,
  length: 6,
  convRule: rule13
}, {
  start: 3872,
  length: 10,
  convRule: rule8
}, {
  start: 3882,
  length: 10,
  convRule: rule17
}, {
  start: 3892,
  length: 1,
  convRule: rule13
}, {
  start: 3893,
  length: 1,
  convRule: rule92
}, {
  start: 3894,
  length: 1,
  convRule: rule13
}, {
  start: 3895,
  length: 1,
  convRule: rule92
}, {
  start: 3896,
  length: 1,
  convRule: rule13
}, {
  start: 3897,
  length: 1,
  convRule: rule92
}, {
  start: 3898,
  length: 1,
  convRule: rule4
}, {
  start: 3899,
  length: 1,
  convRule: rule5
}, {
  start: 3900,
  length: 1,
  convRule: rule4
}, {
  start: 3901,
  length: 1,
  convRule: rule5
}, {
  start: 3902,
  length: 2,
  convRule: rule124
}, {
  start: 3904,
  length: 8,
  convRule: rule14
}, {
  start: 3913,
  length: 36,
  convRule: rule14
}, {
  start: 3953,
  length: 14,
  convRule: rule92
}, {
  start: 3967,
  length: 1,
  convRule: rule124
}, {
  start: 3968,
  length: 5,
  convRule: rule92
}, {
  start: 3973,
  length: 1,
  convRule: rule2
}, {
  start: 3974,
  length: 2,
  convRule: rule92
}, {
  start: 3976,
  length: 5,
  convRule: rule14
}, {
  start: 3981,
  length: 11,
  convRule: rule92
}, {
  start: 3993,
  length: 36,
  convRule: rule92
}, {
  start: 4030,
  length: 8,
  convRule: rule13
}, {
  start: 4038,
  length: 1,
  convRule: rule92
}, {
  start: 4039,
  length: 6,
  convRule: rule13
}, {
  start: 4046,
  length: 2,
  convRule: rule13
}, {
  start: 4048,
  length: 5,
  convRule: rule2
}, {
  start: 4053,
  length: 4,
  convRule: rule13
}, {
  start: 4057,
  length: 2,
  convRule: rule2
}, {
  start: 4096,
  length: 43,
  convRule: rule14
}, {
  start: 4139,
  length: 2,
  convRule: rule124
}, {
  start: 4141,
  length: 4,
  convRule: rule92
}, {
  start: 4145,
  length: 1,
  convRule: rule124
}, {
  start: 4146,
  length: 6,
  convRule: rule92
}, {
  start: 4152,
  length: 1,
  convRule: rule124
}, {
  start: 4153,
  length: 2,
  convRule: rule92
}, {
  start: 4155,
  length: 2,
  convRule: rule124
}, {
  start: 4157,
  length: 2,
  convRule: rule92
}, {
  start: 4159,
  length: 1,
  convRule: rule14
}, {
  start: 4160,
  length: 10,
  convRule: rule8
}, {
  start: 4170,
  length: 6,
  convRule: rule2
}, {
  start: 4176,
  length: 6,
  convRule: rule14
}, {
  start: 4182,
  length: 2,
  convRule: rule124
}, {
  start: 4184,
  length: 2,
  convRule: rule92
}, {
  start: 4186,
  length: 4,
  convRule: rule14
}, {
  start: 4190,
  length: 3,
  convRule: rule92
}, {
  start: 4193,
  length: 1,
  convRule: rule14
}, {
  start: 4194,
  length: 3,
  convRule: rule124
}, {
  start: 4197,
  length: 2,
  convRule: rule14
}, {
  start: 4199,
  length: 7,
  convRule: rule124
}, {
  start: 4206,
  length: 3,
  convRule: rule14
}, {
  start: 4209,
  length: 4,
  convRule: rule92
}, {
  start: 4213,
  length: 13,
  convRule: rule14
}, {
  start: 4226,
  length: 1,
  convRule: rule92
}, {
  start: 4227,
  length: 2,
  convRule: rule124
}, {
  start: 4229,
  length: 2,
  convRule: rule92
}, {
  start: 4231,
  length: 6,
  convRule: rule124
}, {
  start: 4237,
  length: 1,
  convRule: rule92
}, {
  start: 4238,
  length: 1,
  convRule: rule14
}, {
  start: 4239,
  length: 1,
  convRule: rule124
}, {
  start: 4240,
  length: 10,
  convRule: rule8
}, {
  start: 4250,
  length: 3,
  convRule: rule124
}, {
  start: 4253,
  length: 1,
  convRule: rule92
}, {
  start: 4254,
  length: 2,
  convRule: rule13
}, {
  start: 4256,
  length: 38,
  convRule: rule125
}, {
  start: 4295,
  length: 1,
  convRule: rule125
}, {
  start: 4301,
  length: 1,
  convRule: rule125
}, {
  start: 4304,
  length: 43,
  convRule: rule126
}, {
  start: 4347,
  length: 1,
  convRule: rule2
}, {
  start: 4348,
  length: 1,
  convRule: rule91
}, {
  start: 4349,
  length: 3,
  convRule: rule126
}, {
  start: 4352,
  length: 329,
  convRule: rule14
}, {
  start: 4682,
  length: 4,
  convRule: rule14
}, {
  start: 4688,
  length: 7,
  convRule: rule14
}, {
  start: 4696,
  length: 1,
  convRule: rule14
}, {
  start: 4698,
  length: 4,
  convRule: rule14
}, {
  start: 4704,
  length: 41,
  convRule: rule14
}, {
  start: 4746,
  length: 4,
  convRule: rule14
}, {
  start: 4752,
  length: 33,
  convRule: rule14
}, {
  start: 4786,
  length: 4,
  convRule: rule14
}, {
  start: 4792,
  length: 7,
  convRule: rule14
}, {
  start: 4800,
  length: 1,
  convRule: rule14
}, {
  start: 4802,
  length: 4,
  convRule: rule14
}, {
  start: 4808,
  length: 15,
  convRule: rule14
}, {
  start: 4824,
  length: 57,
  convRule: rule14
}, {
  start: 4882,
  length: 4,
  convRule: rule14
}, {
  start: 4888,
  length: 67,
  convRule: rule14
}, {
  start: 4957,
  length: 3,
  convRule: rule92
}, {
  start: 4960,
  length: 9,
  convRule: rule2
}, {
  start: 4969,
  length: 20,
  convRule: rule17
}, {
  start: 4992,
  length: 16,
  convRule: rule14
}, {
  start: 5008,
  length: 10,
  convRule: rule13
}, {
  start: 5024,
  length: 80,
  convRule: rule127
}, {
  start: 5104,
  length: 6,
  convRule: rule104
}, {
  start: 5112,
  length: 6,
  convRule: rule110
}, {
  start: 5120,
  length: 1,
  convRule: rule7
}, {
  start: 5121,
  length: 620,
  convRule: rule14
}, {
  start: 5741,
  length: 1,
  convRule: rule13
}, {
  start: 5742,
  length: 1,
  convRule: rule2
}, {
  start: 5743,
  length: 17,
  convRule: rule14
}, {
  start: 5760,
  length: 1,
  convRule: rule1
}, {
  start: 5761,
  length: 26,
  convRule: rule14
}, {
  start: 5787,
  length: 1,
  convRule: rule4
}, {
  start: 5788,
  length: 1,
  convRule: rule5
}, {
  start: 5792,
  length: 75,
  convRule: rule14
}, {
  start: 5867,
  length: 3,
  convRule: rule2
}, {
  start: 5870,
  length: 3,
  convRule: rule128
}, {
  start: 5873,
  length: 8,
  convRule: rule14
}, {
  start: 5888,
  length: 13,
  convRule: rule14
}, {
  start: 5902,
  length: 4,
  convRule: rule14
}, {
  start: 5906,
  length: 3,
  convRule: rule92
}, {
  start: 5920,
  length: 18,
  convRule: rule14
}, {
  start: 5938,
  length: 3,
  convRule: rule92
}, {
  start: 5941,
  length: 2,
  convRule: rule2
}, {
  start: 5952,
  length: 18,
  convRule: rule14
}, {
  start: 5970,
  length: 2,
  convRule: rule92
}, {
  start: 5984,
  length: 13,
  convRule: rule14
}, {
  start: 5998,
  length: 3,
  convRule: rule14
}, {
  start: 6002,
  length: 2,
  convRule: rule92
}, {
  start: 6016,
  length: 52,
  convRule: rule14
}, {
  start: 6068,
  length: 2,
  convRule: rule92
}, {
  start: 6070,
  length: 1,
  convRule: rule124
}, {
  start: 6071,
  length: 7,
  convRule: rule92
}, {
  start: 6078,
  length: 8,
  convRule: rule124
}, {
  start: 6086,
  length: 1,
  convRule: rule92
}, {
  start: 6087,
  length: 2,
  convRule: rule124
}, {
  start: 6089,
  length: 11,
  convRule: rule92
}, {
  start: 6100,
  length: 3,
  convRule: rule2
}, {
  start: 6103,
  length: 1,
  convRule: rule91
}, {
  start: 6104,
  length: 3,
  convRule: rule2
}, {
  start: 6107,
  length: 1,
  convRule: rule3
}, {
  start: 6108,
  length: 1,
  convRule: rule14
}, {
  start: 6109,
  length: 1,
  convRule: rule92
}, {
  start: 6112,
  length: 10,
  convRule: rule8
}, {
  start: 6128,
  length: 10,
  convRule: rule17
}, {
  start: 6144,
  length: 6,
  convRule: rule2
}, {
  start: 6150,
  length: 1,
  convRule: rule7
}, {
  start: 6151,
  length: 4,
  convRule: rule2
}, {
  start: 6155,
  length: 3,
  convRule: rule92
}, {
  start: 6158,
  length: 1,
  convRule: rule16
}, {
  start: 6160,
  length: 10,
  convRule: rule8
}, {
  start: 6176,
  length: 35,
  convRule: rule14
}, {
  start: 6211,
  length: 1,
  convRule: rule91
}, {
  start: 6212,
  length: 53,
  convRule: rule14
}, {
  start: 6272,
  length: 5,
  convRule: rule14
}, {
  start: 6277,
  length: 2,
  convRule: rule92
}, {
  start: 6279,
  length: 34,
  convRule: rule14
}, {
  start: 6313,
  length: 1,
  convRule: rule92
}, {
  start: 6314,
  length: 1,
  convRule: rule14
}, {
  start: 6320,
  length: 70,
  convRule: rule14
}, {
  start: 6400,
  length: 31,
  convRule: rule14
}, {
  start: 6432,
  length: 3,
  convRule: rule92
}, {
  start: 6435,
  length: 4,
  convRule: rule124
}, {
  start: 6439,
  length: 2,
  convRule: rule92
}, {
  start: 6441,
  length: 3,
  convRule: rule124
}, {
  start: 6448,
  length: 2,
  convRule: rule124
}, {
  start: 6450,
  length: 1,
  convRule: rule92
}, {
  start: 6451,
  length: 6,
  convRule: rule124
}, {
  start: 6457,
  length: 3,
  convRule: rule92
}, {
  start: 6464,
  length: 1,
  convRule: rule13
}, {
  start: 6468,
  length: 2,
  convRule: rule2
}, {
  start: 6470,
  length: 10,
  convRule: rule8
}, {
  start: 6480,
  length: 30,
  convRule: rule14
}, {
  start: 6512,
  length: 5,
  convRule: rule14
}, {
  start: 6528,
  length: 44,
  convRule: rule14
}, {
  start: 6576,
  length: 26,
  convRule: rule14
}, {
  start: 6608,
  length: 10,
  convRule: rule8
}, {
  start: 6618,
  length: 1,
  convRule: rule17
}, {
  start: 6622,
  length: 34,
  convRule: rule13
}, {
  start: 6656,
  length: 23,
  convRule: rule14
}, {
  start: 6679,
  length: 2,
  convRule: rule92
}, {
  start: 6681,
  length: 2,
  convRule: rule124
}, {
  start: 6683,
  length: 1,
  convRule: rule92
}, {
  start: 6686,
  length: 2,
  convRule: rule2
}, {
  start: 6688,
  length: 53,
  convRule: rule14
}, {
  start: 6741,
  length: 1,
  convRule: rule124
}, {
  start: 6742,
  length: 1,
  convRule: rule92
}, {
  start: 6743,
  length: 1,
  convRule: rule124
}, {
  start: 6744,
  length: 7,
  convRule: rule92
}, {
  start: 6752,
  length: 1,
  convRule: rule92
}, {
  start: 6753,
  length: 1,
  convRule: rule124
}, {
  start: 6754,
  length: 1,
  convRule: rule92
}, {
  start: 6755,
  length: 2,
  convRule: rule124
}, {
  start: 6757,
  length: 8,
  convRule: rule92
}, {
  start: 6765,
  length: 6,
  convRule: rule124
}, {
  start: 6771,
  length: 10,
  convRule: rule92
}, {
  start: 6783,
  length: 1,
  convRule: rule92
}, {
  start: 6784,
  length: 10,
  convRule: rule8
}, {
  start: 6800,
  length: 10,
  convRule: rule8
}, {
  start: 6816,
  length: 7,
  convRule: rule2
}, {
  start: 6823,
  length: 1,
  convRule: rule91
}, {
  start: 6824,
  length: 6,
  convRule: rule2
}, {
  start: 6832,
  length: 14,
  convRule: rule92
}, {
  start: 6846,
  length: 1,
  convRule: rule119
}, {
  start: 6847,
  length: 2,
  convRule: rule92
}, {
  start: 6912,
  length: 4,
  convRule: rule92
}, {
  start: 6916,
  length: 1,
  convRule: rule124
}, {
  start: 6917,
  length: 47,
  convRule: rule14
}, {
  start: 6964,
  length: 1,
  convRule: rule92
}, {
  start: 6965,
  length: 1,
  convRule: rule124
}, {
  start: 6966,
  length: 5,
  convRule: rule92
}, {
  start: 6971,
  length: 1,
  convRule: rule124
}, {
  start: 6972,
  length: 1,
  convRule: rule92
}, {
  start: 6973,
  length: 5,
  convRule: rule124
}, {
  start: 6978,
  length: 1,
  convRule: rule92
}, {
  start: 6979,
  length: 2,
  convRule: rule124
}, {
  start: 6981,
  length: 7,
  convRule: rule14
}, {
  start: 6992,
  length: 10,
  convRule: rule8
}, {
  start: 7002,
  length: 7,
  convRule: rule2
}, {
  start: 7009,
  length: 10,
  convRule: rule13
}, {
  start: 7019,
  length: 9,
  convRule: rule92
}, {
  start: 7028,
  length: 9,
  convRule: rule13
}, {
  start: 7040,
  length: 2,
  convRule: rule92
}, {
  start: 7042,
  length: 1,
  convRule: rule124
}, {
  start: 7043,
  length: 30,
  convRule: rule14
}, {
  start: 7073,
  length: 1,
  convRule: rule124
}, {
  start: 7074,
  length: 4,
  convRule: rule92
}, {
  start: 7078,
  length: 2,
  convRule: rule124
}, {
  start: 7080,
  length: 2,
  convRule: rule92
}, {
  start: 7082,
  length: 1,
  convRule: rule124
}, {
  start: 7083,
  length: 3,
  convRule: rule92
}, {
  start: 7086,
  length: 2,
  convRule: rule14
}, {
  start: 7088,
  length: 10,
  convRule: rule8
}, {
  start: 7098,
  length: 44,
  convRule: rule14
}, {
  start: 7142,
  length: 1,
  convRule: rule92
}, {
  start: 7143,
  length: 1,
  convRule: rule124
}, {
  start: 7144,
  length: 2,
  convRule: rule92
}, {
  start: 7146,
  length: 3,
  convRule: rule124
}, {
  start: 7149,
  length: 1,
  convRule: rule92
}, {
  start: 7150,
  length: 1,
  convRule: rule124
}, {
  start: 7151,
  length: 3,
  convRule: rule92
}, {
  start: 7154,
  length: 2,
  convRule: rule124
}, {
  start: 7164,
  length: 4,
  convRule: rule2
}, {
  start: 7168,
  length: 36,
  convRule: rule14
}, {
  start: 7204,
  length: 8,
  convRule: rule124
}, {
  start: 7212,
  length: 8,
  convRule: rule92
}, {
  start: 7220,
  length: 2,
  convRule: rule124
}, {
  start: 7222,
  length: 2,
  convRule: rule92
}, {
  start: 7227,
  length: 5,
  convRule: rule2
}, {
  start: 7232,
  length: 10,
  convRule: rule8
}, {
  start: 7245,
  length: 3,
  convRule: rule14
}, {
  start: 7248,
  length: 10,
  convRule: rule8
}, {
  start: 7258,
  length: 30,
  convRule: rule14
}, {
  start: 7288,
  length: 6,
  convRule: rule91
}, {
  start: 7294,
  length: 2,
  convRule: rule2
}, {
  start: 7296,
  length: 1,
  convRule: rule129
}, {
  start: 7297,
  length: 1,
  convRule: rule130
}, {
  start: 7298,
  length: 1,
  convRule: rule131
}, {
  start: 7299,
  length: 2,
  convRule: rule132
}, {
  start: 7301,
  length: 1,
  convRule: rule133
}, {
  start: 7302,
  length: 1,
  convRule: rule134
}, {
  start: 7303,
  length: 1,
  convRule: rule135
}, {
  start: 7304,
  length: 1,
  convRule: rule136
}, {
  start: 7312,
  length: 43,
  convRule: rule137
}, {
  start: 7357,
  length: 3,
  convRule: rule137
}, {
  start: 7360,
  length: 8,
  convRule: rule2
}, {
  start: 7376,
  length: 3,
  convRule: rule92
}, {
  start: 7379,
  length: 1,
  convRule: rule2
}, {
  start: 7380,
  length: 13,
  convRule: rule92
}, {
  start: 7393,
  length: 1,
  convRule: rule124
}, {
  start: 7394,
  length: 7,
  convRule: rule92
}, {
  start: 7401,
  length: 4,
  convRule: rule14
}, {
  start: 7405,
  length: 1,
  convRule: rule92
}, {
  start: 7406,
  length: 6,
  convRule: rule14
}, {
  start: 7412,
  length: 1,
  convRule: rule92
}, {
  start: 7413,
  length: 2,
  convRule: rule14
}, {
  start: 7415,
  length: 1,
  convRule: rule124
}, {
  start: 7416,
  length: 2,
  convRule: rule92
}, {
  start: 7418,
  length: 1,
  convRule: rule14
}, {
  start: 7424,
  length: 44,
  convRule: rule20
}, {
  start: 7468,
  length: 63,
  convRule: rule91
}, {
  start: 7531,
  length: 13,
  convRule: rule20
}, {
  start: 7544,
  length: 1,
  convRule: rule91
}, {
  start: 7545,
  length: 1,
  convRule: rule138
}, {
  start: 7546,
  length: 3,
  convRule: rule20
}, {
  start: 7549,
  length: 1,
  convRule: rule139
}, {
  start: 7550,
  length: 16,
  convRule: rule20
}, {
  start: 7566,
  length: 1,
  convRule: rule140
}, {
  start: 7567,
  length: 12,
  convRule: rule20
}, {
  start: 7579,
  length: 37,
  convRule: rule91
}, {
  start: 7616,
  length: 58,
  convRule: rule92
}, {
  start: 7675,
  length: 5,
  convRule: rule92
}, {
  start: 7680,
  length: 1,
  convRule: rule22
}, {
  start: 7681,
  length: 1,
  convRule: rule23
}, {
  start: 7682,
  length: 1,
  convRule: rule22
}, {
  start: 7683,
  length: 1,
  convRule: rule23
}, {
  start: 7684,
  length: 1,
  convRule: rule22
}, {
  start: 7685,
  length: 1,
  convRule: rule23
}, {
  start: 7686,
  length: 1,
  convRule: rule22
}, {
  start: 7687,
  length: 1,
  convRule: rule23
}, {
  start: 7688,
  length: 1,
  convRule: rule22
}, {
  start: 7689,
  length: 1,
  convRule: rule23
}, {
  start: 7690,
  length: 1,
  convRule: rule22
}, {
  start: 7691,
  length: 1,
  convRule: rule23
}, {
  start: 7692,
  length: 1,
  convRule: rule22
}, {
  start: 7693,
  length: 1,
  convRule: rule23
}, {
  start: 7694,
  length: 1,
  convRule: rule22
}, {
  start: 7695,
  length: 1,
  convRule: rule23
}, {
  start: 7696,
  length: 1,
  convRule: rule22
}, {
  start: 7697,
  length: 1,
  convRule: rule23
}, {
  start: 7698,
  length: 1,
  convRule: rule22
}, {
  start: 7699,
  length: 1,
  convRule: rule23
}, {
  start: 7700,
  length: 1,
  convRule: rule22
}, {
  start: 7701,
  length: 1,
  convRule: rule23
}, {
  start: 7702,
  length: 1,
  convRule: rule22
}, {
  start: 7703,
  length: 1,
  convRule: rule23
}, {
  start: 7704,
  length: 1,
  convRule: rule22
}, {
  start: 7705,
  length: 1,
  convRule: rule23
}, {
  start: 7706,
  length: 1,
  convRule: rule22
}, {
  start: 7707,
  length: 1,
  convRule: rule23
}, {
  start: 7708,
  length: 1,
  convRule: rule22
}, {
  start: 7709,
  length: 1,
  convRule: rule23
}, {
  start: 7710,
  length: 1,
  convRule: rule22
}, {
  start: 7711,
  length: 1,
  convRule: rule23
}, {
  start: 7712,
  length: 1,
  convRule: rule22
}, {
  start: 7713,
  length: 1,
  convRule: rule23
}, {
  start: 7714,
  length: 1,
  convRule: rule22
}, {
  start: 7715,
  length: 1,
  convRule: rule23
}, {
  start: 7716,
  length: 1,
  convRule: rule22
}, {
  start: 7717,
  length: 1,
  convRule: rule23
}, {
  start: 7718,
  length: 1,
  convRule: rule22
}, {
  start: 7719,
  length: 1,
  convRule: rule23
}, {
  start: 7720,
  length: 1,
  convRule: rule22
}, {
  start: 7721,
  length: 1,
  convRule: rule23
}, {
  start: 7722,
  length: 1,
  convRule: rule22
}, {
  start: 7723,
  length: 1,
  convRule: rule23
}, {
  start: 7724,
  length: 1,
  convRule: rule22
}, {
  start: 7725,
  length: 1,
  convRule: rule23
}, {
  start: 7726,
  length: 1,
  convRule: rule22
}, {
  start: 7727,
  length: 1,
  convRule: rule23
}, {
  start: 7728,
  length: 1,
  convRule: rule22
}, {
  start: 7729,
  length: 1,
  convRule: rule23
}, {
  start: 7730,
  length: 1,
  convRule: rule22
}, {
  start: 7731,
  length: 1,
  convRule: rule23
}, {
  start: 7732,
  length: 1,
  convRule: rule22
}, {
  start: 7733,
  length: 1,
  convRule: rule23
}, {
  start: 7734,
  length: 1,
  convRule: rule22
}, {
  start: 7735,
  length: 1,
  convRule: rule23
}, {
  start: 7736,
  length: 1,
  convRule: rule22
}, {
  start: 7737,
  length: 1,
  convRule: rule23
}, {
  start: 7738,
  length: 1,
  convRule: rule22
}, {
  start: 7739,
  length: 1,
  convRule: rule23
}, {
  start: 7740,
  length: 1,
  convRule: rule22
}, {
  start: 7741,
  length: 1,
  convRule: rule23
}, {
  start: 7742,
  length: 1,
  convRule: rule22
}, {
  start: 7743,
  length: 1,
  convRule: rule23
}, {
  start: 7744,
  length: 1,
  convRule: rule22
}, {
  start: 7745,
  length: 1,
  convRule: rule23
}, {
  start: 7746,
  length: 1,
  convRule: rule22
}, {
  start: 7747,
  length: 1,
  convRule: rule23
}, {
  start: 7748,
  length: 1,
  convRule: rule22
}, {
  start: 7749,
  length: 1,
  convRule: rule23
}, {
  start: 7750,
  length: 1,
  convRule: rule22
}, {
  start: 7751,
  length: 1,
  convRule: rule23
}, {
  start: 7752,
  length: 1,
  convRule: rule22
}, {
  start: 7753,
  length: 1,
  convRule: rule23
}, {
  start: 7754,
  length: 1,
  convRule: rule22
}, {
  start: 7755,
  length: 1,
  convRule: rule23
}, {
  start: 7756,
  length: 1,
  convRule: rule22
}, {
  start: 7757,
  length: 1,
  convRule: rule23
}, {
  start: 7758,
  length: 1,
  convRule: rule22
}, {
  start: 7759,
  length: 1,
  convRule: rule23
}, {
  start: 7760,
  length: 1,
  convRule: rule22
}, {
  start: 7761,
  length: 1,
  convRule: rule23
}, {
  start: 7762,
  length: 1,
  convRule: rule22
}, {
  start: 7763,
  length: 1,
  convRule: rule23
}, {
  start: 7764,
  length: 1,
  convRule: rule22
}, {
  start: 7765,
  length: 1,
  convRule: rule23
}, {
  start: 7766,
  length: 1,
  convRule: rule22
}, {
  start: 7767,
  length: 1,
  convRule: rule23
}, {
  start: 7768,
  length: 1,
  convRule: rule22
}, {
  start: 7769,
  length: 1,
  convRule: rule23
}, {
  start: 7770,
  length: 1,
  convRule: rule22
}, {
  start: 7771,
  length: 1,
  convRule: rule23
}, {
  start: 7772,
  length: 1,
  convRule: rule22
}, {
  start: 7773,
  length: 1,
  convRule: rule23
}, {
  start: 7774,
  length: 1,
  convRule: rule22
}, {
  start: 7775,
  length: 1,
  convRule: rule23
}, {
  start: 7776,
  length: 1,
  convRule: rule22
}, {
  start: 7777,
  length: 1,
  convRule: rule23
}, {
  start: 7778,
  length: 1,
  convRule: rule22
}, {
  start: 7779,
  length: 1,
  convRule: rule23
}, {
  start: 7780,
  length: 1,
  convRule: rule22
}, {
  start: 7781,
  length: 1,
  convRule: rule23
}, {
  start: 7782,
  length: 1,
  convRule: rule22
}, {
  start: 7783,
  length: 1,
  convRule: rule23
}, {
  start: 7784,
  length: 1,
  convRule: rule22
}, {
  start: 7785,
  length: 1,
  convRule: rule23
}, {
  start: 7786,
  length: 1,
  convRule: rule22
}, {
  start: 7787,
  length: 1,
  convRule: rule23
}, {
  start: 7788,
  length: 1,
  convRule: rule22
}, {
  start: 7789,
  length: 1,
  convRule: rule23
}, {
  start: 7790,
  length: 1,
  convRule: rule22
}, {
  start: 7791,
  length: 1,
  convRule: rule23
}, {
  start: 7792,
  length: 1,
  convRule: rule22
}, {
  start: 7793,
  length: 1,
  convRule: rule23
}, {
  start: 7794,
  length: 1,
  convRule: rule22
}, {
  start: 7795,
  length: 1,
  convRule: rule23
}, {
  start: 7796,
  length: 1,
  convRule: rule22
}, {
  start: 7797,
  length: 1,
  convRule: rule23
}, {
  start: 7798,
  length: 1,
  convRule: rule22
}, {
  start: 7799,
  length: 1,
  convRule: rule23
}, {
  start: 7800,
  length: 1,
  convRule: rule22
}, {
  start: 7801,
  length: 1,
  convRule: rule23
}, {
  start: 7802,
  length: 1,
  convRule: rule22
}, {
  start: 7803,
  length: 1,
  convRule: rule23
}, {
  start: 7804,
  length: 1,
  convRule: rule22
}, {
  start: 7805,
  length: 1,
  convRule: rule23
}, {
  start: 7806,
  length: 1,
  convRule: rule22
}, {
  start: 7807,
  length: 1,
  convRule: rule23
}, {
  start: 7808,
  length: 1,
  convRule: rule22
}, {
  start: 7809,
  length: 1,
  convRule: rule23
}, {
  start: 7810,
  length: 1,
  convRule: rule22
}, {
  start: 7811,
  length: 1,
  convRule: rule23
}, {
  start: 7812,
  length: 1,
  convRule: rule22
}, {
  start: 7813,
  length: 1,
  convRule: rule23
}, {
  start: 7814,
  length: 1,
  convRule: rule22
}, {
  start: 7815,
  length: 1,
  convRule: rule23
}, {
  start: 7816,
  length: 1,
  convRule: rule22
}, {
  start: 7817,
  length: 1,
  convRule: rule23
}, {
  start: 7818,
  length: 1,
  convRule: rule22
}, {
  start: 7819,
  length: 1,
  convRule: rule23
}, {
  start: 7820,
  length: 1,
  convRule: rule22
}, {
  start: 7821,
  length: 1,
  convRule: rule23
}, {
  start: 7822,
  length: 1,
  convRule: rule22
}, {
  start: 7823,
  length: 1,
  convRule: rule23
}, {
  start: 7824,
  length: 1,
  convRule: rule22
}, {
  start: 7825,
  length: 1,
  convRule: rule23
}, {
  start: 7826,
  length: 1,
  convRule: rule22
}, {
  start: 7827,
  length: 1,
  convRule: rule23
}, {
  start: 7828,
  length: 1,
  convRule: rule22
}, {
  start: 7829,
  length: 1,
  convRule: rule23
}, {
  start: 7830,
  length: 5,
  convRule: rule20
}, {
  start: 7835,
  length: 1,
  convRule: rule141
}, {
  start: 7836,
  length: 2,
  convRule: rule20
}, {
  start: 7838,
  length: 1,
  convRule: rule142
}, {
  start: 7839,
  length: 1,
  convRule: rule20
}, {
  start: 7840,
  length: 1,
  convRule: rule22
}, {
  start: 7841,
  length: 1,
  convRule: rule23
}, {
  start: 7842,
  length: 1,
  convRule: rule22
}, {
  start: 7843,
  length: 1,
  convRule: rule23
}, {
  start: 7844,
  length: 1,
  convRule: rule22
}, {
  start: 7845,
  length: 1,
  convRule: rule23
}, {
  start: 7846,
  length: 1,
  convRule: rule22
}, {
  start: 7847,
  length: 1,
  convRule: rule23
}, {
  start: 7848,
  length: 1,
  convRule: rule22
}, {
  start: 7849,
  length: 1,
  convRule: rule23
}, {
  start: 7850,
  length: 1,
  convRule: rule22
}, {
  start: 7851,
  length: 1,
  convRule: rule23
}, {
  start: 7852,
  length: 1,
  convRule: rule22
}, {
  start: 7853,
  length: 1,
  convRule: rule23
}, {
  start: 7854,
  length: 1,
  convRule: rule22
}, {
  start: 7855,
  length: 1,
  convRule: rule23
}, {
  start: 7856,
  length: 1,
  convRule: rule22
}, {
  start: 7857,
  length: 1,
  convRule: rule23
}, {
  start: 7858,
  length: 1,
  convRule: rule22
}, {
  start: 7859,
  length: 1,
  convRule: rule23
}, {
  start: 7860,
  length: 1,
  convRule: rule22
}, {
  start: 7861,
  length: 1,
  convRule: rule23
}, {
  start: 7862,
  length: 1,
  convRule: rule22
}, {
  start: 7863,
  length: 1,
  convRule: rule23
}, {
  start: 7864,
  length: 1,
  convRule: rule22
}, {
  start: 7865,
  length: 1,
  convRule: rule23
}, {
  start: 7866,
  length: 1,
  convRule: rule22
}, {
  start: 7867,
  length: 1,
  convRule: rule23
}, {
  start: 7868,
  length: 1,
  convRule: rule22
}, {
  start: 7869,
  length: 1,
  convRule: rule23
}, {
  start: 7870,
  length: 1,
  convRule: rule22
}, {
  start: 7871,
  length: 1,
  convRule: rule23
}, {
  start: 7872,
  length: 1,
  convRule: rule22
}, {
  start: 7873,
  length: 1,
  convRule: rule23
}, {
  start: 7874,
  length: 1,
  convRule: rule22
}, {
  start: 7875,
  length: 1,
  convRule: rule23
}, {
  start: 7876,
  length: 1,
  convRule: rule22
}, {
  start: 7877,
  length: 1,
  convRule: rule23
}, {
  start: 7878,
  length: 1,
  convRule: rule22
}, {
  start: 7879,
  length: 1,
  convRule: rule23
}, {
  start: 7880,
  length: 1,
  convRule: rule22
}, {
  start: 7881,
  length: 1,
  convRule: rule23
}, {
  start: 7882,
  length: 1,
  convRule: rule22
}, {
  start: 7883,
  length: 1,
  convRule: rule23
}, {
  start: 7884,
  length: 1,
  convRule: rule22
}, {
  start: 7885,
  length: 1,
  convRule: rule23
}, {
  start: 7886,
  length: 1,
  convRule: rule22
}, {
  start: 7887,
  length: 1,
  convRule: rule23
}, {
  start: 7888,
  length: 1,
  convRule: rule22
}, {
  start: 7889,
  length: 1,
  convRule: rule23
}, {
  start: 7890,
  length: 1,
  convRule: rule22
}, {
  start: 7891,
  length: 1,
  convRule: rule23
}, {
  start: 7892,
  length: 1,
  convRule: rule22
}, {
  start: 7893,
  length: 1,
  convRule: rule23
}, {
  start: 7894,
  length: 1,
  convRule: rule22
}, {
  start: 7895,
  length: 1,
  convRule: rule23
}, {
  start: 7896,
  length: 1,
  convRule: rule22
}, {
  start: 7897,
  length: 1,
  convRule: rule23
}, {
  start: 7898,
  length: 1,
  convRule: rule22
}, {
  start: 7899,
  length: 1,
  convRule: rule23
}, {
  start: 7900,
  length: 1,
  convRule: rule22
}, {
  start: 7901,
  length: 1,
  convRule: rule23
}, {
  start: 7902,
  length: 1,
  convRule: rule22
}, {
  start: 7903,
  length: 1,
  convRule: rule23
}, {
  start: 7904,
  length: 1,
  convRule: rule22
}, {
  start: 7905,
  length: 1,
  convRule: rule23
}, {
  start: 7906,
  length: 1,
  convRule: rule22
}, {
  start: 7907,
  length: 1,
  convRule: rule23
}, {
  start: 7908,
  length: 1,
  convRule: rule22
}, {
  start: 7909,
  length: 1,
  convRule: rule23
}, {
  start: 7910,
  length: 1,
  convRule: rule22
}, {
  start: 7911,
  length: 1,
  convRule: rule23
}, {
  start: 7912,
  length: 1,
  convRule: rule22
}, {
  start: 7913,
  length: 1,
  convRule: rule23
}, {
  start: 7914,
  length: 1,
  convRule: rule22
}, {
  start: 7915,
  length: 1,
  convRule: rule23
}, {
  start: 7916,
  length: 1,
  convRule: rule22
}, {
  start: 7917,
  length: 1,
  convRule: rule23
}, {
  start: 7918,
  length: 1,
  convRule: rule22
}, {
  start: 7919,
  length: 1,
  convRule: rule23
}, {
  start: 7920,
  length: 1,
  convRule: rule22
}, {
  start: 7921,
  length: 1,
  convRule: rule23
}, {
  start: 7922,
  length: 1,
  convRule: rule22
}, {
  start: 7923,
  length: 1,
  convRule: rule23
}, {
  start: 7924,
  length: 1,
  convRule: rule22
}, {
  start: 7925,
  length: 1,
  convRule: rule23
}, {
  start: 7926,
  length: 1,
  convRule: rule22
}, {
  start: 7927,
  length: 1,
  convRule: rule23
}, {
  start: 7928,
  length: 1,
  convRule: rule22
}, {
  start: 7929,
  length: 1,
  convRule: rule23
}, {
  start: 7930,
  length: 1,
  convRule: rule22
}, {
  start: 7931,
  length: 1,
  convRule: rule23
}, {
  start: 7932,
  length: 1,
  convRule: rule22
}, {
  start: 7933,
  length: 1,
  convRule: rule23
}, {
  start: 7934,
  length: 1,
  convRule: rule22
}, {
  start: 7935,
  length: 1,
  convRule: rule23
}, {
  start: 7936,
  length: 8,
  convRule: rule143
}, {
  start: 7944,
  length: 8,
  convRule: rule144
}, {
  start: 7952,
  length: 6,
  convRule: rule143
}, {
  start: 7960,
  length: 6,
  convRule: rule144
}, {
  start: 7968,
  length: 8,
  convRule: rule143
}, {
  start: 7976,
  length: 8,
  convRule: rule144
}, {
  start: 7984,
  length: 8,
  convRule: rule143
}, {
  start: 7992,
  length: 8,
  convRule: rule144
}, {
  start: 8e3,
  length: 6,
  convRule: rule143
}, {
  start: 8008,
  length: 6,
  convRule: rule144
}, {
  start: 8016,
  length: 1,
  convRule: rule20
}, {
  start: 8017,
  length: 1,
  convRule: rule143
}, {
  start: 8018,
  length: 1,
  convRule: rule20
}, {
  start: 8019,
  length: 1,
  convRule: rule143
}, {
  start: 8020,
  length: 1,
  convRule: rule20
}, {
  start: 8021,
  length: 1,
  convRule: rule143
}, {
  start: 8022,
  length: 1,
  convRule: rule20
}, {
  start: 8023,
  length: 1,
  convRule: rule143
}, {
  start: 8025,
  length: 1,
  convRule: rule144
}, {
  start: 8027,
  length: 1,
  convRule: rule144
}, {
  start: 8029,
  length: 1,
  convRule: rule144
}, {
  start: 8031,
  length: 1,
  convRule: rule144
}, {
  start: 8032,
  length: 8,
  convRule: rule143
}, {
  start: 8040,
  length: 8,
  convRule: rule144
}, {
  start: 8048,
  length: 2,
  convRule: rule145
}, {
  start: 8050,
  length: 4,
  convRule: rule146
}, {
  start: 8054,
  length: 2,
  convRule: rule147
}, {
  start: 8056,
  length: 2,
  convRule: rule148
}, {
  start: 8058,
  length: 2,
  convRule: rule149
}, {
  start: 8060,
  length: 2,
  convRule: rule150
}, {
  start: 8064,
  length: 8,
  convRule: rule143
}, {
  start: 8072,
  length: 8,
  convRule: rule151
}, {
  start: 8080,
  length: 8,
  convRule: rule143
}, {
  start: 8088,
  length: 8,
  convRule: rule151
}, {
  start: 8096,
  length: 8,
  convRule: rule143
}, {
  start: 8104,
  length: 8,
  convRule: rule151
}, {
  start: 8112,
  length: 2,
  convRule: rule143
}, {
  start: 8114,
  length: 1,
  convRule: rule20
}, {
  start: 8115,
  length: 1,
  convRule: rule152
}, {
  start: 8116,
  length: 1,
  convRule: rule20
}, {
  start: 8118,
  length: 2,
  convRule: rule20
}, {
  start: 8120,
  length: 2,
  convRule: rule144
}, {
  start: 8122,
  length: 2,
  convRule: rule153
}, {
  start: 8124,
  length: 1,
  convRule: rule154
}, {
  start: 8125,
  length: 1,
  convRule: rule10
}, {
  start: 8126,
  length: 1,
  convRule: rule155
}, {
  start: 8127,
  length: 3,
  convRule: rule10
}, {
  start: 8130,
  length: 1,
  convRule: rule20
}, {
  start: 8131,
  length: 1,
  convRule: rule152
}, {
  start: 8132,
  length: 1,
  convRule: rule20
}, {
  start: 8134,
  length: 2,
  convRule: rule20
}, {
  start: 8136,
  length: 4,
  convRule: rule156
}, {
  start: 8140,
  length: 1,
  convRule: rule154
}, {
  start: 8141,
  length: 3,
  convRule: rule10
}, {
  start: 8144,
  length: 2,
  convRule: rule143
}, {
  start: 8146,
  length: 2,
  convRule: rule20
}, {
  start: 8150,
  length: 2,
  convRule: rule20
}, {
  start: 8152,
  length: 2,
  convRule: rule144
}, {
  start: 8154,
  length: 2,
  convRule: rule157
}, {
  start: 8157,
  length: 3,
  convRule: rule10
}, {
  start: 8160,
  length: 2,
  convRule: rule143
}, {
  start: 8162,
  length: 3,
  convRule: rule20
}, {
  start: 8165,
  length: 1,
  convRule: rule113
}, {
  start: 8166,
  length: 2,
  convRule: rule20
}, {
  start: 8168,
  length: 2,
  convRule: rule144
}, {
  start: 8170,
  length: 2,
  convRule: rule158
}, {
  start: 8172,
  length: 1,
  convRule: rule117
}, {
  start: 8173,
  length: 3,
  convRule: rule10
}, {
  start: 8178,
  length: 1,
  convRule: rule20
}, {
  start: 8179,
  length: 1,
  convRule: rule152
}, {
  start: 8180,
  length: 1,
  convRule: rule20
}, {
  start: 8182,
  length: 2,
  convRule: rule20
}, {
  start: 8184,
  length: 2,
  convRule: rule159
}, {
  start: 8186,
  length: 2,
  convRule: rule160
}, {
  start: 8188,
  length: 1,
  convRule: rule154
}, {
  start: 8189,
  length: 2,
  convRule: rule10
}, {
  start: 8192,
  length: 11,
  convRule: rule1
}, {
  start: 8203,
  length: 5,
  convRule: rule16
}, {
  start: 8208,
  length: 6,
  convRule: rule7
}, {
  start: 8214,
  length: 2,
  convRule: rule2
}, {
  start: 8216,
  length: 1,
  convRule: rule15
}, {
  start: 8217,
  length: 1,
  convRule: rule19
}, {
  start: 8218,
  length: 1,
  convRule: rule4
}, {
  start: 8219,
  length: 2,
  convRule: rule15
}, {
  start: 8221,
  length: 1,
  convRule: rule19
}, {
  start: 8222,
  length: 1,
  convRule: rule4
}, {
  start: 8223,
  length: 1,
  convRule: rule15
}, {
  start: 8224,
  length: 8,
  convRule: rule2
}, {
  start: 8232,
  length: 1,
  convRule: rule161
}, {
  start: 8233,
  length: 1,
  convRule: rule162
}, {
  start: 8234,
  length: 5,
  convRule: rule16
}, {
  start: 8239,
  length: 1,
  convRule: rule1
}, {
  start: 8240,
  length: 9,
  convRule: rule2
}, {
  start: 8249,
  length: 1,
  convRule: rule15
}, {
  start: 8250,
  length: 1,
  convRule: rule19
}, {
  start: 8251,
  length: 4,
  convRule: rule2
}, {
  start: 8255,
  length: 2,
  convRule: rule11
}, {
  start: 8257,
  length: 3,
  convRule: rule2
}, {
  start: 8260,
  length: 1,
  convRule: rule6
}, {
  start: 8261,
  length: 1,
  convRule: rule4
}, {
  start: 8262,
  length: 1,
  convRule: rule5
}, {
  start: 8263,
  length: 11,
  convRule: rule2
}, {
  start: 8274,
  length: 1,
  convRule: rule6
}, {
  start: 8275,
  length: 1,
  convRule: rule2
}, {
  start: 8276,
  length: 1,
  convRule: rule11
}, {
  start: 8277,
  length: 10,
  convRule: rule2
}, {
  start: 8287,
  length: 1,
  convRule: rule1
}, {
  start: 8288,
  length: 5,
  convRule: rule16
}, {
  start: 8294,
  length: 10,
  convRule: rule16
}, {
  start: 8304,
  length: 1,
  convRule: rule17
}, {
  start: 8305,
  length: 1,
  convRule: rule91
}, {
  start: 8308,
  length: 6,
  convRule: rule17
}, {
  start: 8314,
  length: 3,
  convRule: rule6
}, {
  start: 8317,
  length: 1,
  convRule: rule4
}, {
  start: 8318,
  length: 1,
  convRule: rule5
}, {
  start: 8319,
  length: 1,
  convRule: rule91
}, {
  start: 8320,
  length: 10,
  convRule: rule17
}, {
  start: 8330,
  length: 3,
  convRule: rule6
}, {
  start: 8333,
  length: 1,
  convRule: rule4
}, {
  start: 8334,
  length: 1,
  convRule: rule5
}, {
  start: 8336,
  length: 13,
  convRule: rule91
}, {
  start: 8352,
  length: 32,
  convRule: rule3
}, {
  start: 8400,
  length: 13,
  convRule: rule92
}, {
  start: 8413,
  length: 4,
  convRule: rule119
}, {
  start: 8417,
  length: 1,
  convRule: rule92
}, {
  start: 8418,
  length: 3,
  convRule: rule119
}, {
  start: 8421,
  length: 12,
  convRule: rule92
}, {
  start: 8448,
  length: 2,
  convRule: rule13
}, {
  start: 8450,
  length: 1,
  convRule: rule107
}, {
  start: 8451,
  length: 4,
  convRule: rule13
}, {
  start: 8455,
  length: 1,
  convRule: rule107
}, {
  start: 8456,
  length: 2,
  convRule: rule13
}, {
  start: 8458,
  length: 1,
  convRule: rule20
}, {
  start: 8459,
  length: 3,
  convRule: rule107
}, {
  start: 8462,
  length: 2,
  convRule: rule20
}, {
  start: 8464,
  length: 3,
  convRule: rule107
}, {
  start: 8467,
  length: 1,
  convRule: rule20
}, {
  start: 8468,
  length: 1,
  convRule: rule13
}, {
  start: 8469,
  length: 1,
  convRule: rule107
}, {
  start: 8470,
  length: 2,
  convRule: rule13
}, {
  start: 8472,
  length: 1,
  convRule: rule6
}, {
  start: 8473,
  length: 5,
  convRule: rule107
}, {
  start: 8478,
  length: 6,
  convRule: rule13
}, {
  start: 8484,
  length: 1,
  convRule: rule107
}, {
  start: 8485,
  length: 1,
  convRule: rule13
}, {
  start: 8486,
  length: 1,
  convRule: rule163
}, {
  start: 8487,
  length: 1,
  convRule: rule13
}, {
  start: 8488,
  length: 1,
  convRule: rule107
}, {
  start: 8489,
  length: 1,
  convRule: rule13
}, {
  start: 8490,
  length: 1,
  convRule: rule164
}, {
  start: 8491,
  length: 1,
  convRule: rule165
}, {
  start: 8492,
  length: 2,
  convRule: rule107
}, {
  start: 8494,
  length: 1,
  convRule: rule13
}, {
  start: 8495,
  length: 1,
  convRule: rule20
}, {
  start: 8496,
  length: 2,
  convRule: rule107
}, {
  start: 8498,
  length: 1,
  convRule: rule166
}, {
  start: 8499,
  length: 1,
  convRule: rule107
}, {
  start: 8500,
  length: 1,
  convRule: rule20
}, {
  start: 8501,
  length: 4,
  convRule: rule14
}, {
  start: 8505,
  length: 1,
  convRule: rule20
}, {
  start: 8506,
  length: 2,
  convRule: rule13
}, {
  start: 8508,
  length: 2,
  convRule: rule20
}, {
  start: 8510,
  length: 2,
  convRule: rule107
}, {
  start: 8512,
  length: 5,
  convRule: rule6
}, {
  start: 8517,
  length: 1,
  convRule: rule107
}, {
  start: 8518,
  length: 4,
  convRule: rule20
}, {
  start: 8522,
  length: 1,
  convRule: rule13
}, {
  start: 8523,
  length: 1,
  convRule: rule6
}, {
  start: 8524,
  length: 2,
  convRule: rule13
}, {
  start: 8526,
  length: 1,
  convRule: rule167
}, {
  start: 8527,
  length: 1,
  convRule: rule13
}, {
  start: 8528,
  length: 16,
  convRule: rule17
}, {
  start: 8544,
  length: 16,
  convRule: rule168
}, {
  start: 8560,
  length: 16,
  convRule: rule169
}, {
  start: 8576,
  length: 3,
  convRule: rule128
}, {
  start: 8579,
  length: 1,
  convRule: rule22
}, {
  start: 8580,
  length: 1,
  convRule: rule23
}, {
  start: 8581,
  length: 4,
  convRule: rule128
}, {
  start: 8585,
  length: 1,
  convRule: rule17
}, {
  start: 8586,
  length: 2,
  convRule: rule13
}, {
  start: 8592,
  length: 5,
  convRule: rule6
}, {
  start: 8597,
  length: 5,
  convRule: rule13
}, {
  start: 8602,
  length: 2,
  convRule: rule6
}, {
  start: 8604,
  length: 4,
  convRule: rule13
}, {
  start: 8608,
  length: 1,
  convRule: rule6
}, {
  start: 8609,
  length: 2,
  convRule: rule13
}, {
  start: 8611,
  length: 1,
  convRule: rule6
}, {
  start: 8612,
  length: 2,
  convRule: rule13
}, {
  start: 8614,
  length: 1,
  convRule: rule6
}, {
  start: 8615,
  length: 7,
  convRule: rule13
}, {
  start: 8622,
  length: 1,
  convRule: rule6
}, {
  start: 8623,
  length: 31,
  convRule: rule13
}, {
  start: 8654,
  length: 2,
  convRule: rule6
}, {
  start: 8656,
  length: 2,
  convRule: rule13
}, {
  start: 8658,
  length: 1,
  convRule: rule6
}, {
  start: 8659,
  length: 1,
  convRule: rule13
}, {
  start: 8660,
  length: 1,
  convRule: rule6
}, {
  start: 8661,
  length: 31,
  convRule: rule13
}, {
  start: 8692,
  length: 268,
  convRule: rule6
}, {
  start: 8960,
  length: 8,
  convRule: rule13
}, {
  start: 8968,
  length: 1,
  convRule: rule4
}, {
  start: 8969,
  length: 1,
  convRule: rule5
}, {
  start: 8970,
  length: 1,
  convRule: rule4
}, {
  start: 8971,
  length: 1,
  convRule: rule5
}, {
  start: 8972,
  length: 20,
  convRule: rule13
}, {
  start: 8992,
  length: 2,
  convRule: rule6
}, {
  start: 8994,
  length: 7,
  convRule: rule13
}, {
  start: 9001,
  length: 1,
  convRule: rule4
}, {
  start: 9002,
  length: 1,
  convRule: rule5
}, {
  start: 9003,
  length: 81,
  convRule: rule13
}, {
  start: 9084,
  length: 1,
  convRule: rule6
}, {
  start: 9085,
  length: 30,
  convRule: rule13
}, {
  start: 9115,
  length: 25,
  convRule: rule6
}, {
  start: 9140,
  length: 40,
  convRule: rule13
}, {
  start: 9180,
  length: 6,
  convRule: rule6
}, {
  start: 9186,
  length: 69,
  convRule: rule13
}, {
  start: 9280,
  length: 11,
  convRule: rule13
}, {
  start: 9312,
  length: 60,
  convRule: rule17
}, {
  start: 9372,
  length: 26,
  convRule: rule13
}, {
  start: 9398,
  length: 26,
  convRule: rule170
}, {
  start: 9424,
  length: 26,
  convRule: rule171
}, {
  start: 9450,
  length: 22,
  convRule: rule17
}, {
  start: 9472,
  length: 183,
  convRule: rule13
}, {
  start: 9655,
  length: 1,
  convRule: rule6
}, {
  start: 9656,
  length: 9,
  convRule: rule13
}, {
  start: 9665,
  length: 1,
  convRule: rule6
}, {
  start: 9666,
  length: 54,
  convRule: rule13
}, {
  start: 9720,
  length: 8,
  convRule: rule6
}, {
  start: 9728,
  length: 111,
  convRule: rule13
}, {
  start: 9839,
  length: 1,
  convRule: rule6
}, {
  start: 9840,
  length: 248,
  convRule: rule13
}, {
  start: 10088,
  length: 1,
  convRule: rule4
}, {
  start: 10089,
  length: 1,
  convRule: rule5
}, {
  start: 10090,
  length: 1,
  convRule: rule4
}, {
  start: 10091,
  length: 1,
  convRule: rule5
}, {
  start: 10092,
  length: 1,
  convRule: rule4
}, {
  start: 10093,
  length: 1,
  convRule: rule5
}, {
  start: 10094,
  length: 1,
  convRule: rule4
}, {
  start: 10095,
  length: 1,
  convRule: rule5
}, {
  start: 10096,
  length: 1,
  convRule: rule4
}, {
  start: 10097,
  length: 1,
  convRule: rule5
}, {
  start: 10098,
  length: 1,
  convRule: rule4
}, {
  start: 10099,
  length: 1,
  convRule: rule5
}, {
  start: 10100,
  length: 1,
  convRule: rule4
}, {
  start: 10101,
  length: 1,
  convRule: rule5
}, {
  start: 10102,
  length: 30,
  convRule: rule17
}, {
  start: 10132,
  length: 44,
  convRule: rule13
}, {
  start: 10176,
  length: 5,
  convRule: rule6
}, {
  start: 10181,
  length: 1,
  convRule: rule4
}, {
  start: 10182,
  length: 1,
  convRule: rule5
}, {
  start: 10183,
  length: 31,
  convRule: rule6
}, {
  start: 10214,
  length: 1,
  convRule: rule4
}, {
  start: 10215,
  length: 1,
  convRule: rule5
}, {
  start: 10216,
  length: 1,
  convRule: rule4
}, {
  start: 10217,
  length: 1,
  convRule: rule5
}, {
  start: 10218,
  length: 1,
  convRule: rule4
}, {
  start: 10219,
  length: 1,
  convRule: rule5
}, {
  start: 10220,
  length: 1,
  convRule: rule4
}, {
  start: 10221,
  length: 1,
  convRule: rule5
}, {
  start: 10222,
  length: 1,
  convRule: rule4
}, {
  start: 10223,
  length: 1,
  convRule: rule5
}, {
  start: 10224,
  length: 16,
  convRule: rule6
}, {
  start: 10240,
  length: 256,
  convRule: rule13
}, {
  start: 10496,
  length: 131,
  convRule: rule6
}, {
  start: 10627,
  length: 1,
  convRule: rule4
}, {
  start: 10628,
  length: 1,
  convRule: rule5
}, {
  start: 10629,
  length: 1,
  convRule: rule4
}, {
  start: 10630,
  length: 1,
  convRule: rule5
}, {
  start: 10631,
  length: 1,
  convRule: rule4
}, {
  start: 10632,
  length: 1,
  convRule: rule5
}, {
  start: 10633,
  length: 1,
  convRule: rule4
}, {
  start: 10634,
  length: 1,
  convRule: rule5
}, {
  start: 10635,
  length: 1,
  convRule: rule4
}, {
  start: 10636,
  length: 1,
  convRule: rule5
}, {
  start: 10637,
  length: 1,
  convRule: rule4
}, {
  start: 10638,
  length: 1,
  convRule: rule5
}, {
  start: 10639,
  length: 1,
  convRule: rule4
}, {
  start: 10640,
  length: 1,
  convRule: rule5
}, {
  start: 10641,
  length: 1,
  convRule: rule4
}, {
  start: 10642,
  length: 1,
  convRule: rule5
}, {
  start: 10643,
  length: 1,
  convRule: rule4
}, {
  start: 10644,
  length: 1,
  convRule: rule5
}, {
  start: 10645,
  length: 1,
  convRule: rule4
}, {
  start: 10646,
  length: 1,
  convRule: rule5
}, {
  start: 10647,
  length: 1,
  convRule: rule4
}, {
  start: 10648,
  length: 1,
  convRule: rule5
}, {
  start: 10649,
  length: 63,
  convRule: rule6
}, {
  start: 10712,
  length: 1,
  convRule: rule4
}, {
  start: 10713,
  length: 1,
  convRule: rule5
}, {
  start: 10714,
  length: 1,
  convRule: rule4
}, {
  start: 10715,
  length: 1,
  convRule: rule5
}, {
  start: 10716,
  length: 32,
  convRule: rule6
}, {
  start: 10748,
  length: 1,
  convRule: rule4
}, {
  start: 10749,
  length: 1,
  convRule: rule5
}, {
  start: 10750,
  length: 258,
  convRule: rule6
}, {
  start: 11008,
  length: 48,
  convRule: rule13
}, {
  start: 11056,
  length: 21,
  convRule: rule6
}, {
  start: 11077,
  length: 2,
  convRule: rule13
}, {
  start: 11079,
  length: 6,
  convRule: rule6
}, {
  start: 11085,
  length: 39,
  convRule: rule13
}, {
  start: 11126,
  length: 32,
  convRule: rule13
}, {
  start: 11159,
  length: 105,
  convRule: rule13
}, {
  start: 11264,
  length: 47,
  convRule: rule122
}, {
  start: 11312,
  length: 47,
  convRule: rule123
}, {
  start: 11360,
  length: 1,
  convRule: rule22
}, {
  start: 11361,
  length: 1,
  convRule: rule23
}, {
  start: 11362,
  length: 1,
  convRule: rule172
}, {
  start: 11363,
  length: 1,
  convRule: rule173
}, {
  start: 11364,
  length: 1,
  convRule: rule174
}, {
  start: 11365,
  length: 1,
  convRule: rule175
}, {
  start: 11366,
  length: 1,
  convRule: rule176
}, {
  start: 11367,
  length: 1,
  convRule: rule22
}, {
  start: 11368,
  length: 1,
  convRule: rule23
}, {
  start: 11369,
  length: 1,
  convRule: rule22
}, {
  start: 11370,
  length: 1,
  convRule: rule23
}, {
  start: 11371,
  length: 1,
  convRule: rule22
}, {
  start: 11372,
  length: 1,
  convRule: rule23
}, {
  start: 11373,
  length: 1,
  convRule: rule177
}, {
  start: 11374,
  length: 1,
  convRule: rule178
}, {
  start: 11375,
  length: 1,
  convRule: rule179
}, {
  start: 11376,
  length: 1,
  convRule: rule180
}, {
  start: 11377,
  length: 1,
  convRule: rule20
}, {
  start: 11378,
  length: 1,
  convRule: rule22
}, {
  start: 11379,
  length: 1,
  convRule: rule23
}, {
  start: 11380,
  length: 1,
  convRule: rule20
}, {
  start: 11381,
  length: 1,
  convRule: rule22
}, {
  start: 11382,
  length: 1,
  convRule: rule23
}, {
  start: 11383,
  length: 5,
  convRule: rule20
}, {
  start: 11388,
  length: 2,
  convRule: rule91
}, {
  start: 11390,
  length: 2,
  convRule: rule181
}, {
  start: 11392,
  length: 1,
  convRule: rule22
}, {
  start: 11393,
  length: 1,
  convRule: rule23
}, {
  start: 11394,
  length: 1,
  convRule: rule22
}, {
  start: 11395,
  length: 1,
  convRule: rule23
}, {
  start: 11396,
  length: 1,
  convRule: rule22
}, {
  start: 11397,
  length: 1,
  convRule: rule23
}, {
  start: 11398,
  length: 1,
  convRule: rule22
}, {
  start: 11399,
  length: 1,
  convRule: rule23
}, {
  start: 11400,
  length: 1,
  convRule: rule22
}, {
  start: 11401,
  length: 1,
  convRule: rule23
}, {
  start: 11402,
  length: 1,
  convRule: rule22
}, {
  start: 11403,
  length: 1,
  convRule: rule23
}, {
  start: 11404,
  length: 1,
  convRule: rule22
}, {
  start: 11405,
  length: 1,
  convRule: rule23
}, {
  start: 11406,
  length: 1,
  convRule: rule22
}, {
  start: 11407,
  length: 1,
  convRule: rule23
}, {
  start: 11408,
  length: 1,
  convRule: rule22
}, {
  start: 11409,
  length: 1,
  convRule: rule23
}, {
  start: 11410,
  length: 1,
  convRule: rule22
}, {
  start: 11411,
  length: 1,
  convRule: rule23
}, {
  start: 11412,
  length: 1,
  convRule: rule22
}, {
  start: 11413,
  length: 1,
  convRule: rule23
}, {
  start: 11414,
  length: 1,
  convRule: rule22
}, {
  start: 11415,
  length: 1,
  convRule: rule23
}, {
  start: 11416,
  length: 1,
  convRule: rule22
}, {
  start: 11417,
  length: 1,
  convRule: rule23
}, {
  start: 11418,
  length: 1,
  convRule: rule22
}, {
  start: 11419,
  length: 1,
  convRule: rule23
}, {
  start: 11420,
  length: 1,
  convRule: rule22
}, {
  start: 11421,
  length: 1,
  convRule: rule23
}, {
  start: 11422,
  length: 1,
  convRule: rule22
}, {
  start: 11423,
  length: 1,
  convRule: rule23
}, {
  start: 11424,
  length: 1,
  convRule: rule22
}, {
  start: 11425,
  length: 1,
  convRule: rule23
}, {
  start: 11426,
  length: 1,
  convRule: rule22
}, {
  start: 11427,
  length: 1,
  convRule: rule23
}, {
  start: 11428,
  length: 1,
  convRule: rule22
}, {
  start: 11429,
  length: 1,
  convRule: rule23
}, {
  start: 11430,
  length: 1,
  convRule: rule22
}, {
  start: 11431,
  length: 1,
  convRule: rule23
}, {
  start: 11432,
  length: 1,
  convRule: rule22
}, {
  start: 11433,
  length: 1,
  convRule: rule23
}, {
  start: 11434,
  length: 1,
  convRule: rule22
}, {
  start: 11435,
  length: 1,
  convRule: rule23
}, {
  start: 11436,
  length: 1,
  convRule: rule22
}, {
  start: 11437,
  length: 1,
  convRule: rule23
}, {
  start: 11438,
  length: 1,
  convRule: rule22
}, {
  start: 11439,
  length: 1,
  convRule: rule23
}, {
  start: 11440,
  length: 1,
  convRule: rule22
}, {
  start: 11441,
  length: 1,
  convRule: rule23
}, {
  start: 11442,
  length: 1,
  convRule: rule22
}, {
  start: 11443,
  length: 1,
  convRule: rule23
}, {
  start: 11444,
  length: 1,
  convRule: rule22
}, {
  start: 11445,
  length: 1,
  convRule: rule23
}, {
  start: 11446,
  length: 1,
  convRule: rule22
}, {
  start: 11447,
  length: 1,
  convRule: rule23
}, {
  start: 11448,
  length: 1,
  convRule: rule22
}, {
  start: 11449,
  length: 1,
  convRule: rule23
}, {
  start: 11450,
  length: 1,
  convRule: rule22
}, {
  start: 11451,
  length: 1,
  convRule: rule23
}, {
  start: 11452,
  length: 1,
  convRule: rule22
}, {
  start: 11453,
  length: 1,
  convRule: rule23
}, {
  start: 11454,
  length: 1,
  convRule: rule22
}, {
  start: 11455,
  length: 1,
  convRule: rule23
}, {
  start: 11456,
  length: 1,
  convRule: rule22
}, {
  start: 11457,
  length: 1,
  convRule: rule23
}, {
  start: 11458,
  length: 1,
  convRule: rule22
}, {
  start: 11459,
  length: 1,
  convRule: rule23
}, {
  start: 11460,
  length: 1,
  convRule: rule22
}, {
  start: 11461,
  length: 1,
  convRule: rule23
}, {
  start: 11462,
  length: 1,
  convRule: rule22
}, {
  start: 11463,
  length: 1,
  convRule: rule23
}, {
  start: 11464,
  length: 1,
  convRule: rule22
}, {
  start: 11465,
  length: 1,
  convRule: rule23
}, {
  start: 11466,
  length: 1,
  convRule: rule22
}, {
  start: 11467,
  length: 1,
  convRule: rule23
}, {
  start: 11468,
  length: 1,
  convRule: rule22
}, {
  start: 11469,
  length: 1,
  convRule: rule23
}, {
  start: 11470,
  length: 1,
  convRule: rule22
}, {
  start: 11471,
  length: 1,
  convRule: rule23
}, {
  start: 11472,
  length: 1,
  convRule: rule22
}, {
  start: 11473,
  length: 1,
  convRule: rule23
}, {
  start: 11474,
  length: 1,
  convRule: rule22
}, {
  start: 11475,
  length: 1,
  convRule: rule23
}, {
  start: 11476,
  length: 1,
  convRule: rule22
}, {
  start: 11477,
  length: 1,
  convRule: rule23
}, {
  start: 11478,
  length: 1,
  convRule: rule22
}, {
  start: 11479,
  length: 1,
  convRule: rule23
}, {
  start: 11480,
  length: 1,
  convRule: rule22
}, {
  start: 11481,
  length: 1,
  convRule: rule23
}, {
  start: 11482,
  length: 1,
  convRule: rule22
}, {
  start: 11483,
  length: 1,
  convRule: rule23
}, {
  start: 11484,
  length: 1,
  convRule: rule22
}, {
  start: 11485,
  length: 1,
  convRule: rule23
}, {
  start: 11486,
  length: 1,
  convRule: rule22
}, {
  start: 11487,
  length: 1,
  convRule: rule23
}, {
  start: 11488,
  length: 1,
  convRule: rule22
}, {
  start: 11489,
  length: 1,
  convRule: rule23
}, {
  start: 11490,
  length: 1,
  convRule: rule22
}, {
  start: 11491,
  length: 1,
  convRule: rule23
}, {
  start: 11492,
  length: 1,
  convRule: rule20
}, {
  start: 11493,
  length: 6,
  convRule: rule13
}, {
  start: 11499,
  length: 1,
  convRule: rule22
}, {
  start: 11500,
  length: 1,
  convRule: rule23
}, {
  start: 11501,
  length: 1,
  convRule: rule22
}, {
  start: 11502,
  length: 1,
  convRule: rule23
}, {
  start: 11503,
  length: 3,
  convRule: rule92
}, {
  start: 11506,
  length: 1,
  convRule: rule22
}, {
  start: 11507,
  length: 1,
  convRule: rule23
}, {
  start: 11513,
  length: 4,
  convRule: rule2
}, {
  start: 11517,
  length: 1,
  convRule: rule17
}, {
  start: 11518,
  length: 2,
  convRule: rule2
}, {
  start: 11520,
  length: 38,
  convRule: rule182
}, {
  start: 11559,
  length: 1,
  convRule: rule182
}, {
  start: 11565,
  length: 1,
  convRule: rule182
}, {
  start: 11568,
  length: 56,
  convRule: rule14
}, {
  start: 11631,
  length: 1,
  convRule: rule91
}, {
  start: 11632,
  length: 1,
  convRule: rule2
}, {
  start: 11647,
  length: 1,
  convRule: rule92
}, {
  start: 11648,
  length: 23,
  convRule: rule14
}, {
  start: 11680,
  length: 7,
  convRule: rule14
}, {
  start: 11688,
  length: 7,
  convRule: rule14
}, {
  start: 11696,
  length: 7,
  convRule: rule14
}, {
  start: 11704,
  length: 7,
  convRule: rule14
}, {
  start: 11712,
  length: 7,
  convRule: rule14
}, {
  start: 11720,
  length: 7,
  convRule: rule14
}, {
  start: 11728,
  length: 7,
  convRule: rule14
}, {
  start: 11736,
  length: 7,
  convRule: rule14
}, {
  start: 11744,
  length: 32,
  convRule: rule92
}, {
  start: 11776,
  length: 2,
  convRule: rule2
}, {
  start: 11778,
  length: 1,
  convRule: rule15
}, {
  start: 11779,
  length: 1,
  convRule: rule19
}, {
  start: 11780,
  length: 1,
  convRule: rule15
}, {
  start: 11781,
  length: 1,
  convRule: rule19
}, {
  start: 11782,
  length: 3,
  convRule: rule2
}, {
  start: 11785,
  length: 1,
  convRule: rule15
}, {
  start: 11786,
  length: 1,
  convRule: rule19
}, {
  start: 11787,
  length: 1,
  convRule: rule2
}, {
  start: 11788,
  length: 1,
  convRule: rule15
}, {
  start: 11789,
  length: 1,
  convRule: rule19
}, {
  start: 11790,
  length: 9,
  convRule: rule2
}, {
  start: 11799,
  length: 1,
  convRule: rule7
}, {
  start: 11800,
  length: 2,
  convRule: rule2
}, {
  start: 11802,
  length: 1,
  convRule: rule7
}, {
  start: 11803,
  length: 1,
  convRule: rule2
}, {
  start: 11804,
  length: 1,
  convRule: rule15
}, {
  start: 11805,
  length: 1,
  convRule: rule19
}, {
  start: 11806,
  length: 2,
  convRule: rule2
}, {
  start: 11808,
  length: 1,
  convRule: rule15
}, {
  start: 11809,
  length: 1,
  convRule: rule19
}, {
  start: 11810,
  length: 1,
  convRule: rule4
}, {
  start: 11811,
  length: 1,
  convRule: rule5
}, {
  start: 11812,
  length: 1,
  convRule: rule4
}, {
  start: 11813,
  length: 1,
  convRule: rule5
}, {
  start: 11814,
  length: 1,
  convRule: rule4
}, {
  start: 11815,
  length: 1,
  convRule: rule5
}, {
  start: 11816,
  length: 1,
  convRule: rule4
}, {
  start: 11817,
  length: 1,
  convRule: rule5
}, {
  start: 11818,
  length: 5,
  convRule: rule2
}, {
  start: 11823,
  length: 1,
  convRule: rule91
}, {
  start: 11824,
  length: 10,
  convRule: rule2
}, {
  start: 11834,
  length: 2,
  convRule: rule7
}, {
  start: 11836,
  length: 4,
  convRule: rule2
}, {
  start: 11840,
  length: 1,
  convRule: rule7
}, {
  start: 11841,
  length: 1,
  convRule: rule2
}, {
  start: 11842,
  length: 1,
  convRule: rule4
}, {
  start: 11843,
  length: 13,
  convRule: rule2
}, {
  start: 11856,
  length: 2,
  convRule: rule13
}, {
  start: 11858,
  length: 1,
  convRule: rule2
}, {
  start: 11904,
  length: 26,
  convRule: rule13
}, {
  start: 11931,
  length: 89,
  convRule: rule13
}, {
  start: 12032,
  length: 214,
  convRule: rule13
}, {
  start: 12272,
  length: 12,
  convRule: rule13
}, {
  start: 12288,
  length: 1,
  convRule: rule1
}, {
  start: 12289,
  length: 3,
  convRule: rule2
}, {
  start: 12292,
  length: 1,
  convRule: rule13
}, {
  start: 12293,
  length: 1,
  convRule: rule91
}, {
  start: 12294,
  length: 1,
  convRule: rule14
}, {
  start: 12295,
  length: 1,
  convRule: rule128
}, {
  start: 12296,
  length: 1,
  convRule: rule4
}, {
  start: 12297,
  length: 1,
  convRule: rule5
}, {
  start: 12298,
  length: 1,
  convRule: rule4
}, {
  start: 12299,
  length: 1,
  convRule: rule5
}, {
  start: 12300,
  length: 1,
  convRule: rule4
}, {
  start: 12301,
  length: 1,
  convRule: rule5
}, {
  start: 12302,
  length: 1,
  convRule: rule4
}, {
  start: 12303,
  length: 1,
  convRule: rule5
}, {
  start: 12304,
  length: 1,
  convRule: rule4
}, {
  start: 12305,
  length: 1,
  convRule: rule5
}, {
  start: 12306,
  length: 2,
  convRule: rule13
}, {
  start: 12308,
  length: 1,
  convRule: rule4
}, {
  start: 12309,
  length: 1,
  convRule: rule5
}, {
  start: 12310,
  length: 1,
  convRule: rule4
}, {
  start: 12311,
  length: 1,
  convRule: rule5
}, {
  start: 12312,
  length: 1,
  convRule: rule4
}, {
  start: 12313,
  length: 1,
  convRule: rule5
}, {
  start: 12314,
  length: 1,
  convRule: rule4
}, {
  start: 12315,
  length: 1,
  convRule: rule5
}, {
  start: 12316,
  length: 1,
  convRule: rule7
}, {
  start: 12317,
  length: 1,
  convRule: rule4
}, {
  start: 12318,
  length: 2,
  convRule: rule5
}, {
  start: 12320,
  length: 1,
  convRule: rule13
}, {
  start: 12321,
  length: 9,
  convRule: rule128
}, {
  start: 12330,
  length: 4,
  convRule: rule92
}, {
  start: 12334,
  length: 2,
  convRule: rule124
}, {
  start: 12336,
  length: 1,
  convRule: rule7
}, {
  start: 12337,
  length: 5,
  convRule: rule91
}, {
  start: 12342,
  length: 2,
  convRule: rule13
}, {
  start: 12344,
  length: 3,
  convRule: rule128
}, {
  start: 12347,
  length: 1,
  convRule: rule91
}, {
  start: 12348,
  length: 1,
  convRule: rule14
}, {
  start: 12349,
  length: 1,
  convRule: rule2
}, {
  start: 12350,
  length: 2,
  convRule: rule13
}, {
  start: 12353,
  length: 86,
  convRule: rule14
}, {
  start: 12441,
  length: 2,
  convRule: rule92
}, {
  start: 12443,
  length: 2,
  convRule: rule10
}, {
  start: 12445,
  length: 2,
  convRule: rule91
}, {
  start: 12447,
  length: 1,
  convRule: rule14
}, {
  start: 12448,
  length: 1,
  convRule: rule7
}, {
  start: 12449,
  length: 90,
  convRule: rule14
}, {
  start: 12539,
  length: 1,
  convRule: rule2
}, {
  start: 12540,
  length: 3,
  convRule: rule91
}, {
  start: 12543,
  length: 1,
  convRule: rule14
}, {
  start: 12549,
  length: 43,
  convRule: rule14
}, {
  start: 12593,
  length: 94,
  convRule: rule14
}, {
  start: 12688,
  length: 2,
  convRule: rule13
}, {
  start: 12690,
  length: 4,
  convRule: rule17
}, {
  start: 12694,
  length: 10,
  convRule: rule13
}, {
  start: 12704,
  length: 32,
  convRule: rule14
}, {
  start: 12736,
  length: 36,
  convRule: rule13
}, {
  start: 12784,
  length: 16,
  convRule: rule14
}, {
  start: 12800,
  length: 31,
  convRule: rule13
}, {
  start: 12832,
  length: 10,
  convRule: rule17
}, {
  start: 12842,
  length: 30,
  convRule: rule13
}, {
  start: 12872,
  length: 8,
  convRule: rule17
}, {
  start: 12880,
  length: 1,
  convRule: rule13
}, {
  start: 12881,
  length: 15,
  convRule: rule17
}, {
  start: 12896,
  length: 32,
  convRule: rule13
}, {
  start: 12928,
  length: 10,
  convRule: rule17
}, {
  start: 12938,
  length: 39,
  convRule: rule13
}, {
  start: 12977,
  length: 15,
  convRule: rule17
}, {
  start: 12992,
  length: 320,
  convRule: rule13
}, {
  start: 13312,
  length: 6592,
  convRule: rule14
}, {
  start: 19904,
  length: 64,
  convRule: rule13
}, {
  start: 19968,
  length: 20989,
  convRule: rule14
}, {
  start: 40960,
  length: 21,
  convRule: rule14
}, {
  start: 40981,
  length: 1,
  convRule: rule91
}, {
  start: 40982,
  length: 1143,
  convRule: rule14
}, {
  start: 42128,
  length: 55,
  convRule: rule13
}, {
  start: 42192,
  length: 40,
  convRule: rule14
}, {
  start: 42232,
  length: 6,
  convRule: rule91
}, {
  start: 42238,
  length: 2,
  convRule: rule2
}, {
  start: 42240,
  length: 268,
  convRule: rule14
}, {
  start: 42508,
  length: 1,
  convRule: rule91
}, {
  start: 42509,
  length: 3,
  convRule: rule2
}, {
  start: 42512,
  length: 16,
  convRule: rule14
}, {
  start: 42528,
  length: 10,
  convRule: rule8
}, {
  start: 42538,
  length: 2,
  convRule: rule14
}, {
  start: 42560,
  length: 1,
  convRule: rule22
}, {
  start: 42561,
  length: 1,
  convRule: rule23
}, {
  start: 42562,
  length: 1,
  convRule: rule22
}, {
  start: 42563,
  length: 1,
  convRule: rule23
}, {
  start: 42564,
  length: 1,
  convRule: rule22
}, {
  start: 42565,
  length: 1,
  convRule: rule23
}, {
  start: 42566,
  length: 1,
  convRule: rule22
}, {
  start: 42567,
  length: 1,
  convRule: rule23
}, {
  start: 42568,
  length: 1,
  convRule: rule22
}, {
  start: 42569,
  length: 1,
  convRule: rule23
}, {
  start: 42570,
  length: 1,
  convRule: rule22
}, {
  start: 42571,
  length: 1,
  convRule: rule23
}, {
  start: 42572,
  length: 1,
  convRule: rule22
}, {
  start: 42573,
  length: 1,
  convRule: rule23
}, {
  start: 42574,
  length: 1,
  convRule: rule22
}, {
  start: 42575,
  length: 1,
  convRule: rule23
}, {
  start: 42576,
  length: 1,
  convRule: rule22
}, {
  start: 42577,
  length: 1,
  convRule: rule23
}, {
  start: 42578,
  length: 1,
  convRule: rule22
}, {
  start: 42579,
  length: 1,
  convRule: rule23
}, {
  start: 42580,
  length: 1,
  convRule: rule22
}, {
  start: 42581,
  length: 1,
  convRule: rule23
}, {
  start: 42582,
  length: 1,
  convRule: rule22
}, {
  start: 42583,
  length: 1,
  convRule: rule23
}, {
  start: 42584,
  length: 1,
  convRule: rule22
}, {
  start: 42585,
  length: 1,
  convRule: rule23
}, {
  start: 42586,
  length: 1,
  convRule: rule22
}, {
  start: 42587,
  length: 1,
  convRule: rule23
}, {
  start: 42588,
  length: 1,
  convRule: rule22
}, {
  start: 42589,
  length: 1,
  convRule: rule23
}, {
  start: 42590,
  length: 1,
  convRule: rule22
}, {
  start: 42591,
  length: 1,
  convRule: rule23
}, {
  start: 42592,
  length: 1,
  convRule: rule22
}, {
  start: 42593,
  length: 1,
  convRule: rule23
}, {
  start: 42594,
  length: 1,
  convRule: rule22
}, {
  start: 42595,
  length: 1,
  convRule: rule23
}, {
  start: 42596,
  length: 1,
  convRule: rule22
}, {
  start: 42597,
  length: 1,
  convRule: rule23
}, {
  start: 42598,
  length: 1,
  convRule: rule22
}, {
  start: 42599,
  length: 1,
  convRule: rule23
}, {
  start: 42600,
  length: 1,
  convRule: rule22
}, {
  start: 42601,
  length: 1,
  convRule: rule23
}, {
  start: 42602,
  length: 1,
  convRule: rule22
}, {
  start: 42603,
  length: 1,
  convRule: rule23
}, {
  start: 42604,
  length: 1,
  convRule: rule22
}, {
  start: 42605,
  length: 1,
  convRule: rule23
}, {
  start: 42606,
  length: 1,
  convRule: rule14
}, {
  start: 42607,
  length: 1,
  convRule: rule92
}, {
  start: 42608,
  length: 3,
  convRule: rule119
}, {
  start: 42611,
  length: 1,
  convRule: rule2
}, {
  start: 42612,
  length: 10,
  convRule: rule92
}, {
  start: 42622,
  length: 1,
  convRule: rule2
}, {
  start: 42623,
  length: 1,
  convRule: rule91
}, {
  start: 42624,
  length: 1,
  convRule: rule22
}, {
  start: 42625,
  length: 1,
  convRule: rule23
}, {
  start: 42626,
  length: 1,
  convRule: rule22
}, {
  start: 42627,
  length: 1,
  convRule: rule23
}, {
  start: 42628,
  length: 1,
  convRule: rule22
}, {
  start: 42629,
  length: 1,
  convRule: rule23
}, {
  start: 42630,
  length: 1,
  convRule: rule22
}, {
  start: 42631,
  length: 1,
  convRule: rule23
}, {
  start: 42632,
  length: 1,
  convRule: rule22
}, {
  start: 42633,
  length: 1,
  convRule: rule23
}, {
  start: 42634,
  length: 1,
  convRule: rule22
}, {
  start: 42635,
  length: 1,
  convRule: rule23
}, {
  start: 42636,
  length: 1,
  convRule: rule22
}, {
  start: 42637,
  length: 1,
  convRule: rule23
}, {
  start: 42638,
  length: 1,
  convRule: rule22
}, {
  start: 42639,
  length: 1,
  convRule: rule23
}, {
  start: 42640,
  length: 1,
  convRule: rule22
}, {
  start: 42641,
  length: 1,
  convRule: rule23
}, {
  start: 42642,
  length: 1,
  convRule: rule22
}, {
  start: 42643,
  length: 1,
  convRule: rule23
}, {
  start: 42644,
  length: 1,
  convRule: rule22
}, {
  start: 42645,
  length: 1,
  convRule: rule23
}, {
  start: 42646,
  length: 1,
  convRule: rule22
}, {
  start: 42647,
  length: 1,
  convRule: rule23
}, {
  start: 42648,
  length: 1,
  convRule: rule22
}, {
  start: 42649,
  length: 1,
  convRule: rule23
}, {
  start: 42650,
  length: 1,
  convRule: rule22
}, {
  start: 42651,
  length: 1,
  convRule: rule23
}, {
  start: 42652,
  length: 2,
  convRule: rule91
}, {
  start: 42654,
  length: 2,
  convRule: rule92
}, {
  start: 42656,
  length: 70,
  convRule: rule14
}, {
  start: 42726,
  length: 10,
  convRule: rule128
}, {
  start: 42736,
  length: 2,
  convRule: rule92
}, {
  start: 42738,
  length: 6,
  convRule: rule2
}, {
  start: 42752,
  length: 23,
  convRule: rule10
}, {
  start: 42775,
  length: 9,
  convRule: rule91
}, {
  start: 42784,
  length: 2,
  convRule: rule10
}, {
  start: 42786,
  length: 1,
  convRule: rule22
}, {
  start: 42787,
  length: 1,
  convRule: rule23
}, {
  start: 42788,
  length: 1,
  convRule: rule22
}, {
  start: 42789,
  length: 1,
  convRule: rule23
}, {
  start: 42790,
  length: 1,
  convRule: rule22
}, {
  start: 42791,
  length: 1,
  convRule: rule23
}, {
  start: 42792,
  length: 1,
  convRule: rule22
}, {
  start: 42793,
  length: 1,
  convRule: rule23
}, {
  start: 42794,
  length: 1,
  convRule: rule22
}, {
  start: 42795,
  length: 1,
  convRule: rule23
}, {
  start: 42796,
  length: 1,
  convRule: rule22
}, {
  start: 42797,
  length: 1,
  convRule: rule23
}, {
  start: 42798,
  length: 1,
  convRule: rule22
}, {
  start: 42799,
  length: 1,
  convRule: rule23
}, {
  start: 42800,
  length: 2,
  convRule: rule20
}, {
  start: 42802,
  length: 1,
  convRule: rule22
}, {
  start: 42803,
  length: 1,
  convRule: rule23
}, {
  start: 42804,
  length: 1,
  convRule: rule22
}, {
  start: 42805,
  length: 1,
  convRule: rule23
}, {
  start: 42806,
  length: 1,
  convRule: rule22
}, {
  start: 42807,
  length: 1,
  convRule: rule23
}, {
  start: 42808,
  length: 1,
  convRule: rule22
}, {
  start: 42809,
  length: 1,
  convRule: rule23
}, {
  start: 42810,
  length: 1,
  convRule: rule22
}, {
  start: 42811,
  length: 1,
  convRule: rule23
}, {
  start: 42812,
  length: 1,
  convRule: rule22
}, {
  start: 42813,
  length: 1,
  convRule: rule23
}, {
  start: 42814,
  length: 1,
  convRule: rule22
}, {
  start: 42815,
  length: 1,
  convRule: rule23
}, {
  start: 42816,
  length: 1,
  convRule: rule22
}, {
  start: 42817,
  length: 1,
  convRule: rule23
}, {
  start: 42818,
  length: 1,
  convRule: rule22
}, {
  start: 42819,
  length: 1,
  convRule: rule23
}, {
  start: 42820,
  length: 1,
  convRule: rule22
}, {
  start: 42821,
  length: 1,
  convRule: rule23
}, {
  start: 42822,
  length: 1,
  convRule: rule22
}, {
  start: 42823,
  length: 1,
  convRule: rule23
}, {
  start: 42824,
  length: 1,
  convRule: rule22
}, {
  start: 42825,
  length: 1,
  convRule: rule23
}, {
  start: 42826,
  length: 1,
  convRule: rule22
}, {
  start: 42827,
  length: 1,
  convRule: rule23
}, {
  start: 42828,
  length: 1,
  convRule: rule22
}, {
  start: 42829,
  length: 1,
  convRule: rule23
}, {
  start: 42830,
  length: 1,
  convRule: rule22
}, {
  start: 42831,
  length: 1,
  convRule: rule23
}, {
  start: 42832,
  length: 1,
  convRule: rule22
}, {
  start: 42833,
  length: 1,
  convRule: rule23
}, {
  start: 42834,
  length: 1,
  convRule: rule22
}, {
  start: 42835,
  length: 1,
  convRule: rule23
}, {
  start: 42836,
  length: 1,
  convRule: rule22
}, {
  start: 42837,
  length: 1,
  convRule: rule23
}, {
  start: 42838,
  length: 1,
  convRule: rule22
}, {
  start: 42839,
  length: 1,
  convRule: rule23
}, {
  start: 42840,
  length: 1,
  convRule: rule22
}, {
  start: 42841,
  length: 1,
  convRule: rule23
}, {
  start: 42842,
  length: 1,
  convRule: rule22
}, {
  start: 42843,
  length: 1,
  convRule: rule23
}, {
  start: 42844,
  length: 1,
  convRule: rule22
}, {
  start: 42845,
  length: 1,
  convRule: rule23
}, {
  start: 42846,
  length: 1,
  convRule: rule22
}, {
  start: 42847,
  length: 1,
  convRule: rule23
}, {
  start: 42848,
  length: 1,
  convRule: rule22
}, {
  start: 42849,
  length: 1,
  convRule: rule23
}, {
  start: 42850,
  length: 1,
  convRule: rule22
}, {
  start: 42851,
  length: 1,
  convRule: rule23
}, {
  start: 42852,
  length: 1,
  convRule: rule22
}, {
  start: 42853,
  length: 1,
  convRule: rule23
}, {
  start: 42854,
  length: 1,
  convRule: rule22
}, {
  start: 42855,
  length: 1,
  convRule: rule23
}, {
  start: 42856,
  length: 1,
  convRule: rule22
}, {
  start: 42857,
  length: 1,
  convRule: rule23
}, {
  start: 42858,
  length: 1,
  convRule: rule22
}, {
  start: 42859,
  length: 1,
  convRule: rule23
}, {
  start: 42860,
  length: 1,
  convRule: rule22
}, {
  start: 42861,
  length: 1,
  convRule: rule23
}, {
  start: 42862,
  length: 1,
  convRule: rule22
}, {
  start: 42863,
  length: 1,
  convRule: rule23
}, {
  start: 42864,
  length: 1,
  convRule: rule91
}, {
  start: 42865,
  length: 8,
  convRule: rule20
}, {
  start: 42873,
  length: 1,
  convRule: rule22
}, {
  start: 42874,
  length: 1,
  convRule: rule23
}, {
  start: 42875,
  length: 1,
  convRule: rule22
}, {
  start: 42876,
  length: 1,
  convRule: rule23
}, {
  start: 42877,
  length: 1,
  convRule: rule183
}, {
  start: 42878,
  length: 1,
  convRule: rule22
}, {
  start: 42879,
  length: 1,
  convRule: rule23
}, {
  start: 42880,
  length: 1,
  convRule: rule22
}, {
  start: 42881,
  length: 1,
  convRule: rule23
}, {
  start: 42882,
  length: 1,
  convRule: rule22
}, {
  start: 42883,
  length: 1,
  convRule: rule23
}, {
  start: 42884,
  length: 1,
  convRule: rule22
}, {
  start: 42885,
  length: 1,
  convRule: rule23
}, {
  start: 42886,
  length: 1,
  convRule: rule22
}, {
  start: 42887,
  length: 1,
  convRule: rule23
}, {
  start: 42888,
  length: 1,
  convRule: rule91
}, {
  start: 42889,
  length: 2,
  convRule: rule10
}, {
  start: 42891,
  length: 1,
  convRule: rule22
}, {
  start: 42892,
  length: 1,
  convRule: rule23
}, {
  start: 42893,
  length: 1,
  convRule: rule184
}, {
  start: 42894,
  length: 1,
  convRule: rule20
}, {
  start: 42895,
  length: 1,
  convRule: rule14
}, {
  start: 42896,
  length: 1,
  convRule: rule22
}, {
  start: 42897,
  length: 1,
  convRule: rule23
}, {
  start: 42898,
  length: 1,
  convRule: rule22
}, {
  start: 42899,
  length: 1,
  convRule: rule23
}, {
  start: 42900,
  length: 1,
  convRule: rule185
}, {
  start: 42901,
  length: 1,
  convRule: rule20
}, {
  start: 42902,
  length: 1,
  convRule: rule22
}, {
  start: 42903,
  length: 1,
  convRule: rule23
}, {
  start: 42904,
  length: 1,
  convRule: rule22
}, {
  start: 42905,
  length: 1,
  convRule: rule23
}, {
  start: 42906,
  length: 1,
  convRule: rule22
}, {
  start: 42907,
  length: 1,
  convRule: rule23
}, {
  start: 42908,
  length: 1,
  convRule: rule22
}, {
  start: 42909,
  length: 1,
  convRule: rule23
}, {
  start: 42910,
  length: 1,
  convRule: rule22
}, {
  start: 42911,
  length: 1,
  convRule: rule23
}, {
  start: 42912,
  length: 1,
  convRule: rule22
}, {
  start: 42913,
  length: 1,
  convRule: rule23
}, {
  start: 42914,
  length: 1,
  convRule: rule22
}, {
  start: 42915,
  length: 1,
  convRule: rule23
}, {
  start: 42916,
  length: 1,
  convRule: rule22
}, {
  start: 42917,
  length: 1,
  convRule: rule23
}, {
  start: 42918,
  length: 1,
  convRule: rule22
}, {
  start: 42919,
  length: 1,
  convRule: rule23
}, {
  start: 42920,
  length: 1,
  convRule: rule22
}, {
  start: 42921,
  length: 1,
  convRule: rule23
}, {
  start: 42922,
  length: 1,
  convRule: rule186
}, {
  start: 42923,
  length: 1,
  convRule: rule187
}, {
  start: 42924,
  length: 1,
  convRule: rule188
}, {
  start: 42925,
  length: 1,
  convRule: rule189
}, {
  start: 42926,
  length: 1,
  convRule: rule186
}, {
  start: 42927,
  length: 1,
  convRule: rule20
}, {
  start: 42928,
  length: 1,
  convRule: rule190
}, {
  start: 42929,
  length: 1,
  convRule: rule191
}, {
  start: 42930,
  length: 1,
  convRule: rule192
}, {
  start: 42931,
  length: 1,
  convRule: rule193
}, {
  start: 42932,
  length: 1,
  convRule: rule22
}, {
  start: 42933,
  length: 1,
  convRule: rule23
}, {
  start: 42934,
  length: 1,
  convRule: rule22
}, {
  start: 42935,
  length: 1,
  convRule: rule23
}, {
  start: 42936,
  length: 1,
  convRule: rule22
}, {
  start: 42937,
  length: 1,
  convRule: rule23
}, {
  start: 42938,
  length: 1,
  convRule: rule22
}, {
  start: 42939,
  length: 1,
  convRule: rule23
}, {
  start: 42940,
  length: 1,
  convRule: rule22
}, {
  start: 42941,
  length: 1,
  convRule: rule23
}, {
  start: 42942,
  length: 1,
  convRule: rule22
}, {
  start: 42943,
  length: 1,
  convRule: rule23
}, {
  start: 42946,
  length: 1,
  convRule: rule22
}, {
  start: 42947,
  length: 1,
  convRule: rule23
}, {
  start: 42948,
  length: 1,
  convRule: rule194
}, {
  start: 42949,
  length: 1,
  convRule: rule195
}, {
  start: 42950,
  length: 1,
  convRule: rule196
}, {
  start: 42951,
  length: 1,
  convRule: rule22
}, {
  start: 42952,
  length: 1,
  convRule: rule23
}, {
  start: 42953,
  length: 1,
  convRule: rule22
}, {
  start: 42954,
  length: 1,
  convRule: rule23
}, {
  start: 42997,
  length: 1,
  convRule: rule22
}, {
  start: 42998,
  length: 1,
  convRule: rule23
}, {
  start: 42999,
  length: 1,
  convRule: rule14
}, {
  start: 43e3,
  length: 2,
  convRule: rule91
}, {
  start: 43002,
  length: 1,
  convRule: rule20
}, {
  start: 43003,
  length: 7,
  convRule: rule14
}, {
  start: 43010,
  length: 1,
  convRule: rule92
}, {
  start: 43011,
  length: 3,
  convRule: rule14
}, {
  start: 43014,
  length: 1,
  convRule: rule92
}, {
  start: 43015,
  length: 4,
  convRule: rule14
}, {
  start: 43019,
  length: 1,
  convRule: rule92
}, {
  start: 43020,
  length: 23,
  convRule: rule14
}, {
  start: 43043,
  length: 2,
  convRule: rule124
}, {
  start: 43045,
  length: 2,
  convRule: rule92
}, {
  start: 43047,
  length: 1,
  convRule: rule124
}, {
  start: 43048,
  length: 4,
  convRule: rule13
}, {
  start: 43052,
  length: 1,
  convRule: rule92
}, {
  start: 43056,
  length: 6,
  convRule: rule17
}, {
  start: 43062,
  length: 2,
  convRule: rule13
}, {
  start: 43064,
  length: 1,
  convRule: rule3
}, {
  start: 43065,
  length: 1,
  convRule: rule13
}, {
  start: 43072,
  length: 52,
  convRule: rule14
}, {
  start: 43124,
  length: 4,
  convRule: rule2
}, {
  start: 43136,
  length: 2,
  convRule: rule124
}, {
  start: 43138,
  length: 50,
  convRule: rule14
}, {
  start: 43188,
  length: 16,
  convRule: rule124
}, {
  start: 43204,
  length: 2,
  convRule: rule92
}, {
  start: 43214,
  length: 2,
  convRule: rule2
}, {
  start: 43216,
  length: 10,
  convRule: rule8
}, {
  start: 43232,
  length: 18,
  convRule: rule92
}, {
  start: 43250,
  length: 6,
  convRule: rule14
}, {
  start: 43256,
  length: 3,
  convRule: rule2
}, {
  start: 43259,
  length: 1,
  convRule: rule14
}, {
  start: 43260,
  length: 1,
  convRule: rule2
}, {
  start: 43261,
  length: 2,
  convRule: rule14
}, {
  start: 43263,
  length: 1,
  convRule: rule92
}, {
  start: 43264,
  length: 10,
  convRule: rule8
}, {
  start: 43274,
  length: 28,
  convRule: rule14
}, {
  start: 43302,
  length: 8,
  convRule: rule92
}, {
  start: 43310,
  length: 2,
  convRule: rule2
}, {
  start: 43312,
  length: 23,
  convRule: rule14
}, {
  start: 43335,
  length: 11,
  convRule: rule92
}, {
  start: 43346,
  length: 2,
  convRule: rule124
}, {
  start: 43359,
  length: 1,
  convRule: rule2
}, {
  start: 43360,
  length: 29,
  convRule: rule14
}, {
  start: 43392,
  length: 3,
  convRule: rule92
}, {
  start: 43395,
  length: 1,
  convRule: rule124
}, {
  start: 43396,
  length: 47,
  convRule: rule14
}, {
  start: 43443,
  length: 1,
  convRule: rule92
}, {
  start: 43444,
  length: 2,
  convRule: rule124
}, {
  start: 43446,
  length: 4,
  convRule: rule92
}, {
  start: 43450,
  length: 2,
  convRule: rule124
}, {
  start: 43452,
  length: 2,
  convRule: rule92
}, {
  start: 43454,
  length: 3,
  convRule: rule124
}, {
  start: 43457,
  length: 13,
  convRule: rule2
}, {
  start: 43471,
  length: 1,
  convRule: rule91
}, {
  start: 43472,
  length: 10,
  convRule: rule8
}, {
  start: 43486,
  length: 2,
  convRule: rule2
}, {
  start: 43488,
  length: 5,
  convRule: rule14
}, {
  start: 43493,
  length: 1,
  convRule: rule92
}, {
  start: 43494,
  length: 1,
  convRule: rule91
}, {
  start: 43495,
  length: 9,
  convRule: rule14
}, {
  start: 43504,
  length: 10,
  convRule: rule8
}, {
  start: 43514,
  length: 5,
  convRule: rule14
}, {
  start: 43520,
  length: 41,
  convRule: rule14
}, {
  start: 43561,
  length: 6,
  convRule: rule92
}, {
  start: 43567,
  length: 2,
  convRule: rule124
}, {
  start: 43569,
  length: 2,
  convRule: rule92
}, {
  start: 43571,
  length: 2,
  convRule: rule124
}, {
  start: 43573,
  length: 2,
  convRule: rule92
}, {
  start: 43584,
  length: 3,
  convRule: rule14
}, {
  start: 43587,
  length: 1,
  convRule: rule92
}, {
  start: 43588,
  length: 8,
  convRule: rule14
}, {
  start: 43596,
  length: 1,
  convRule: rule92
}, {
  start: 43597,
  length: 1,
  convRule: rule124
}, {
  start: 43600,
  length: 10,
  convRule: rule8
}, {
  start: 43612,
  length: 4,
  convRule: rule2
}, {
  start: 43616,
  length: 16,
  convRule: rule14
}, {
  start: 43632,
  length: 1,
  convRule: rule91
}, {
  start: 43633,
  length: 6,
  convRule: rule14
}, {
  start: 43639,
  length: 3,
  convRule: rule13
}, {
  start: 43642,
  length: 1,
  convRule: rule14
}, {
  start: 43643,
  length: 1,
  convRule: rule124
}, {
  start: 43644,
  length: 1,
  convRule: rule92
}, {
  start: 43645,
  length: 1,
  convRule: rule124
}, {
  start: 43646,
  length: 50,
  convRule: rule14
}, {
  start: 43696,
  length: 1,
  convRule: rule92
}, {
  start: 43697,
  length: 1,
  convRule: rule14
}, {
  start: 43698,
  length: 3,
  convRule: rule92
}, {
  start: 43701,
  length: 2,
  convRule: rule14
}, {
  start: 43703,
  length: 2,
  convRule: rule92
}, {
  start: 43705,
  length: 5,
  convRule: rule14
}, {
  start: 43710,
  length: 2,
  convRule: rule92
}, {
  start: 43712,
  length: 1,
  convRule: rule14
}, {
  start: 43713,
  length: 1,
  convRule: rule92
}, {
  start: 43714,
  length: 1,
  convRule: rule14
}, {
  start: 43739,
  length: 2,
  convRule: rule14
}, {
  start: 43741,
  length: 1,
  convRule: rule91
}, {
  start: 43742,
  length: 2,
  convRule: rule2
}, {
  start: 43744,
  length: 11,
  convRule: rule14
}, {
  start: 43755,
  length: 1,
  convRule: rule124
}, {
  start: 43756,
  length: 2,
  convRule: rule92
}, {
  start: 43758,
  length: 2,
  convRule: rule124
}, {
  start: 43760,
  length: 2,
  convRule: rule2
}, {
  start: 43762,
  length: 1,
  convRule: rule14
}, {
  start: 43763,
  length: 2,
  convRule: rule91
}, {
  start: 43765,
  length: 1,
  convRule: rule124
}, {
  start: 43766,
  length: 1,
  convRule: rule92
}, {
  start: 43777,
  length: 6,
  convRule: rule14
}, {
  start: 43785,
  length: 6,
  convRule: rule14
}, {
  start: 43793,
  length: 6,
  convRule: rule14
}, {
  start: 43808,
  length: 7,
  convRule: rule14
}, {
  start: 43816,
  length: 7,
  convRule: rule14
}, {
  start: 43824,
  length: 35,
  convRule: rule20
}, {
  start: 43859,
  length: 1,
  convRule: rule197
}, {
  start: 43860,
  length: 7,
  convRule: rule20
}, {
  start: 43867,
  length: 1,
  convRule: rule10
}, {
  start: 43868,
  length: 4,
  convRule: rule91
}, {
  start: 43872,
  length: 9,
  convRule: rule20
}, {
  start: 43881,
  length: 1,
  convRule: rule91
}, {
  start: 43882,
  length: 2,
  convRule: rule10
}, {
  start: 43888,
  length: 80,
  convRule: rule198
}, {
  start: 43968,
  length: 35,
  convRule: rule14
}, {
  start: 44003,
  length: 2,
  convRule: rule124
}, {
  start: 44005,
  length: 1,
  convRule: rule92
}, {
  start: 44006,
  length: 2,
  convRule: rule124
}, {
  start: 44008,
  length: 1,
  convRule: rule92
}, {
  start: 44009,
  length: 2,
  convRule: rule124
}, {
  start: 44011,
  length: 1,
  convRule: rule2
}, {
  start: 44012,
  length: 1,
  convRule: rule124
}, {
  start: 44013,
  length: 1,
  convRule: rule92
}, {
  start: 44016,
  length: 10,
  convRule: rule8
}, {
  start: 44032,
  length: 11172,
  convRule: rule14
}, {
  start: 55216,
  length: 23,
  convRule: rule14
}, {
  start: 55243,
  length: 49,
  convRule: rule14
}, {
  start: 55296,
  length: 896,
  convRule: rule199
}, {
  start: 56192,
  length: 128,
  convRule: rule199
}, {
  start: 56320,
  length: 1024,
  convRule: rule199
}, {
  start: 57344,
  length: 6400,
  convRule: rule200
}, {
  start: 63744,
  length: 366,
  convRule: rule14
}, {
  start: 64112,
  length: 106,
  convRule: rule14
}, {
  start: 64256,
  length: 7,
  convRule: rule20
}, {
  start: 64275,
  length: 5,
  convRule: rule20
}, {
  start: 64285,
  length: 1,
  convRule: rule14
}, {
  start: 64286,
  length: 1,
  convRule: rule92
}, {
  start: 64287,
  length: 10,
  convRule: rule14
}, {
  start: 64297,
  length: 1,
  convRule: rule6
}, {
  start: 64298,
  length: 13,
  convRule: rule14
}, {
  start: 64312,
  length: 5,
  convRule: rule14
}, {
  start: 64318,
  length: 1,
  convRule: rule14
}, {
  start: 64320,
  length: 2,
  convRule: rule14
}, {
  start: 64323,
  length: 2,
  convRule: rule14
}, {
  start: 64326,
  length: 108,
  convRule: rule14
}, {
  start: 64434,
  length: 16,
  convRule: rule10
}, {
  start: 64467,
  length: 363,
  convRule: rule14
}, {
  start: 64830,
  length: 1,
  convRule: rule5
}, {
  start: 64831,
  length: 1,
  convRule: rule4
}, {
  start: 64848,
  length: 64,
  convRule: rule14
}, {
  start: 64914,
  length: 54,
  convRule: rule14
}, {
  start: 65008,
  length: 12,
  convRule: rule14
}, {
  start: 65020,
  length: 1,
  convRule: rule3
}, {
  start: 65021,
  length: 1,
  convRule: rule13
}, {
  start: 65024,
  length: 16,
  convRule: rule92
}, {
  start: 65040,
  length: 7,
  convRule: rule2
}, {
  start: 65047,
  length: 1,
  convRule: rule4
}, {
  start: 65048,
  length: 1,
  convRule: rule5
}, {
  start: 65049,
  length: 1,
  convRule: rule2
}, {
  start: 65056,
  length: 16,
  convRule: rule92
}, {
  start: 65072,
  length: 1,
  convRule: rule2
}, {
  start: 65073,
  length: 2,
  convRule: rule7
}, {
  start: 65075,
  length: 2,
  convRule: rule11
}, {
  start: 65077,
  length: 1,
  convRule: rule4
}, {
  start: 65078,
  length: 1,
  convRule: rule5
}, {
  start: 65079,
  length: 1,
  convRule: rule4
}, {
  start: 65080,
  length: 1,
  convRule: rule5
}, {
  start: 65081,
  length: 1,
  convRule: rule4
}, {
  start: 65082,
  length: 1,
  convRule: rule5
}, {
  start: 65083,
  length: 1,
  convRule: rule4
}, {
  start: 65084,
  length: 1,
  convRule: rule5
}, {
  start: 65085,
  length: 1,
  convRule: rule4
}, {
  start: 65086,
  length: 1,
  convRule: rule5
}, {
  start: 65087,
  length: 1,
  convRule: rule4
}, {
  start: 65088,
  length: 1,
  convRule: rule5
}, {
  start: 65089,
  length: 1,
  convRule: rule4
}, {
  start: 65090,
  length: 1,
  convRule: rule5
}, {
  start: 65091,
  length: 1,
  convRule: rule4
}, {
  start: 65092,
  length: 1,
  convRule: rule5
}, {
  start: 65093,
  length: 2,
  convRule: rule2
}, {
  start: 65095,
  length: 1,
  convRule: rule4
}, {
  start: 65096,
  length: 1,
  convRule: rule5
}, {
  start: 65097,
  length: 4,
  convRule: rule2
}, {
  start: 65101,
  length: 3,
  convRule: rule11
}, {
  start: 65104,
  length: 3,
  convRule: rule2
}, {
  start: 65108,
  length: 4,
  convRule: rule2
}, {
  start: 65112,
  length: 1,
  convRule: rule7
}, {
  start: 65113,
  length: 1,
  convRule: rule4
}, {
  start: 65114,
  length: 1,
  convRule: rule5
}, {
  start: 65115,
  length: 1,
  convRule: rule4
}, {
  start: 65116,
  length: 1,
  convRule: rule5
}, {
  start: 65117,
  length: 1,
  convRule: rule4
}, {
  start: 65118,
  length: 1,
  convRule: rule5
}, {
  start: 65119,
  length: 3,
  convRule: rule2
}, {
  start: 65122,
  length: 1,
  convRule: rule6
}, {
  start: 65123,
  length: 1,
  convRule: rule7
}, {
  start: 65124,
  length: 3,
  convRule: rule6
}, {
  start: 65128,
  length: 1,
  convRule: rule2
}, {
  start: 65129,
  length: 1,
  convRule: rule3
}, {
  start: 65130,
  length: 2,
  convRule: rule2
}, {
  start: 65136,
  length: 5,
  convRule: rule14
}, {
  start: 65142,
  length: 135,
  convRule: rule14
}, {
  start: 65279,
  length: 1,
  convRule: rule16
}, {
  start: 65281,
  length: 3,
  convRule: rule2
}, {
  start: 65284,
  length: 1,
  convRule: rule3
}, {
  start: 65285,
  length: 3,
  convRule: rule2
}, {
  start: 65288,
  length: 1,
  convRule: rule4
}, {
  start: 65289,
  length: 1,
  convRule: rule5
}, {
  start: 65290,
  length: 1,
  convRule: rule2
}, {
  start: 65291,
  length: 1,
  convRule: rule6
}, {
  start: 65292,
  length: 1,
  convRule: rule2
}, {
  start: 65293,
  length: 1,
  convRule: rule7
}, {
  start: 65294,
  length: 2,
  convRule: rule2
}, {
  start: 65296,
  length: 10,
  convRule: rule8
}, {
  start: 65306,
  length: 2,
  convRule: rule2
}, {
  start: 65308,
  length: 3,
  convRule: rule6
}, {
  start: 65311,
  length: 2,
  convRule: rule2
}, {
  start: 65313,
  length: 26,
  convRule: rule9
}, {
  start: 65339,
  length: 1,
  convRule: rule4
}, {
  start: 65340,
  length: 1,
  convRule: rule2
}, {
  start: 65341,
  length: 1,
  convRule: rule5
}, {
  start: 65342,
  length: 1,
  convRule: rule10
}, {
  start: 65343,
  length: 1,
  convRule: rule11
}, {
  start: 65344,
  length: 1,
  convRule: rule10
}, {
  start: 65345,
  length: 26,
  convRule: rule12
}, {
  start: 65371,
  length: 1,
  convRule: rule4
}, {
  start: 65372,
  length: 1,
  convRule: rule6
}, {
  start: 65373,
  length: 1,
  convRule: rule5
}, {
  start: 65374,
  length: 1,
  convRule: rule6
}, {
  start: 65375,
  length: 1,
  convRule: rule4
}, {
  start: 65376,
  length: 1,
  convRule: rule5
}, {
  start: 65377,
  length: 1,
  convRule: rule2
}, {
  start: 65378,
  length: 1,
  convRule: rule4
}, {
  start: 65379,
  length: 1,
  convRule: rule5
}, {
  start: 65380,
  length: 2,
  convRule: rule2
}, {
  start: 65382,
  length: 10,
  convRule: rule14
}, {
  start: 65392,
  length: 1,
  convRule: rule91
}, {
  start: 65393,
  length: 45,
  convRule: rule14
}, {
  start: 65438,
  length: 2,
  convRule: rule91
}, {
  start: 65440,
  length: 31,
  convRule: rule14
}, {
  start: 65474,
  length: 6,
  convRule: rule14
}, {
  start: 65482,
  length: 6,
  convRule: rule14
}, {
  start: 65490,
  length: 6,
  convRule: rule14
}, {
  start: 65498,
  length: 3,
  convRule: rule14
}, {
  start: 65504,
  length: 2,
  convRule: rule3
}, {
  start: 65506,
  length: 1,
  convRule: rule6
}, {
  start: 65507,
  length: 1,
  convRule: rule10
}, {
  start: 65508,
  length: 1,
  convRule: rule13
}, {
  start: 65509,
  length: 2,
  convRule: rule3
}, {
  start: 65512,
  length: 1,
  convRule: rule13
}, {
  start: 65513,
  length: 4,
  convRule: rule6
}, {
  start: 65517,
  length: 2,
  convRule: rule13
}, {
  start: 65529,
  length: 3,
  convRule: rule16
}, {
  start: 65532,
  length: 2,
  convRule: rule13
}, {
  start: 65536,
  length: 12,
  convRule: rule14
}, {
  start: 65549,
  length: 26,
  convRule: rule14
}, {
  start: 65576,
  length: 19,
  convRule: rule14
}, {
  start: 65596,
  length: 2,
  convRule: rule14
}, {
  start: 65599,
  length: 15,
  convRule: rule14
}, {
  start: 65616,
  length: 14,
  convRule: rule14
}, {
  start: 65664,
  length: 123,
  convRule: rule14
}, {
  start: 65792,
  length: 3,
  convRule: rule2
}, {
  start: 65799,
  length: 45,
  convRule: rule17
}, {
  start: 65847,
  length: 9,
  convRule: rule13
}, {
  start: 65856,
  length: 53,
  convRule: rule128
}, {
  start: 65909,
  length: 4,
  convRule: rule17
}, {
  start: 65913,
  length: 17,
  convRule: rule13
}, {
  start: 65930,
  length: 2,
  convRule: rule17
}, {
  start: 65932,
  length: 3,
  convRule: rule13
}, {
  start: 65936,
  length: 13,
  convRule: rule13
}, {
  start: 65952,
  length: 1,
  convRule: rule13
}, {
  start: 66e3,
  length: 45,
  convRule: rule13
}, {
  start: 66045,
  length: 1,
  convRule: rule92
}, {
  start: 66176,
  length: 29,
  convRule: rule14
}, {
  start: 66208,
  length: 49,
  convRule: rule14
}, {
  start: 66272,
  length: 1,
  convRule: rule92
}, {
  start: 66273,
  length: 27,
  convRule: rule17
}, {
  start: 66304,
  length: 32,
  convRule: rule14
}, {
  start: 66336,
  length: 4,
  convRule: rule17
}, {
  start: 66349,
  length: 20,
  convRule: rule14
}, {
  start: 66369,
  length: 1,
  convRule: rule128
}, {
  start: 66370,
  length: 8,
  convRule: rule14
}, {
  start: 66378,
  length: 1,
  convRule: rule128
}, {
  start: 66384,
  length: 38,
  convRule: rule14
}, {
  start: 66422,
  length: 5,
  convRule: rule92
}, {
  start: 66432,
  length: 30,
  convRule: rule14
}, {
  start: 66463,
  length: 1,
  convRule: rule2
}, {
  start: 66464,
  length: 36,
  convRule: rule14
}, {
  start: 66504,
  length: 8,
  convRule: rule14
}, {
  start: 66512,
  length: 1,
  convRule: rule2
}, {
  start: 66513,
  length: 5,
  convRule: rule128
}, {
  start: 66560,
  length: 40,
  convRule: rule201
}, {
  start: 66600,
  length: 40,
  convRule: rule202
}, {
  start: 66640,
  length: 78,
  convRule: rule14
}, {
  start: 66720,
  length: 10,
  convRule: rule8
}, {
  start: 66736,
  length: 36,
  convRule: rule201
}, {
  start: 66776,
  length: 36,
  convRule: rule202
}, {
  start: 66816,
  length: 40,
  convRule: rule14
}, {
  start: 66864,
  length: 52,
  convRule: rule14
}, {
  start: 66927,
  length: 1,
  convRule: rule2
}, {
  start: 67072,
  length: 311,
  convRule: rule14
}, {
  start: 67392,
  length: 22,
  convRule: rule14
}, {
  start: 67424,
  length: 8,
  convRule: rule14
}, {
  start: 67584,
  length: 6,
  convRule: rule14
}, {
  start: 67592,
  length: 1,
  convRule: rule14
}, {
  start: 67594,
  length: 44,
  convRule: rule14
}, {
  start: 67639,
  length: 2,
  convRule: rule14
}, {
  start: 67644,
  length: 1,
  convRule: rule14
}, {
  start: 67647,
  length: 23,
  convRule: rule14
}, {
  start: 67671,
  length: 1,
  convRule: rule2
}, {
  start: 67672,
  length: 8,
  convRule: rule17
}, {
  start: 67680,
  length: 23,
  convRule: rule14
}, {
  start: 67703,
  length: 2,
  convRule: rule13
}, {
  start: 67705,
  length: 7,
  convRule: rule17
}, {
  start: 67712,
  length: 31,
  convRule: rule14
}, {
  start: 67751,
  length: 9,
  convRule: rule17
}, {
  start: 67808,
  length: 19,
  convRule: rule14
}, {
  start: 67828,
  length: 2,
  convRule: rule14
}, {
  start: 67835,
  length: 5,
  convRule: rule17
}, {
  start: 67840,
  length: 22,
  convRule: rule14
}, {
  start: 67862,
  length: 6,
  convRule: rule17
}, {
  start: 67871,
  length: 1,
  convRule: rule2
}, {
  start: 67872,
  length: 26,
  convRule: rule14
}, {
  start: 67903,
  length: 1,
  convRule: rule2
}, {
  start: 67968,
  length: 56,
  convRule: rule14
}, {
  start: 68028,
  length: 2,
  convRule: rule17
}, {
  start: 68030,
  length: 2,
  convRule: rule14
}, {
  start: 68032,
  length: 16,
  convRule: rule17
}, {
  start: 68050,
  length: 46,
  convRule: rule17
}, {
  start: 68096,
  length: 1,
  convRule: rule14
}, {
  start: 68097,
  length: 3,
  convRule: rule92
}, {
  start: 68101,
  length: 2,
  convRule: rule92
}, {
  start: 68108,
  length: 4,
  convRule: rule92
}, {
  start: 68112,
  length: 4,
  convRule: rule14
}, {
  start: 68117,
  length: 3,
  convRule: rule14
}, {
  start: 68121,
  length: 29,
  convRule: rule14
}, {
  start: 68152,
  length: 3,
  convRule: rule92
}, {
  start: 68159,
  length: 1,
  convRule: rule92
}, {
  start: 68160,
  length: 9,
  convRule: rule17
}, {
  start: 68176,
  length: 9,
  convRule: rule2
}, {
  start: 68192,
  length: 29,
  convRule: rule14
}, {
  start: 68221,
  length: 2,
  convRule: rule17
}, {
  start: 68223,
  length: 1,
  convRule: rule2
}, {
  start: 68224,
  length: 29,
  convRule: rule14
}, {
  start: 68253,
  length: 3,
  convRule: rule17
}, {
  start: 68288,
  length: 8,
  convRule: rule14
}, {
  start: 68296,
  length: 1,
  convRule: rule13
}, {
  start: 68297,
  length: 28,
  convRule: rule14
}, {
  start: 68325,
  length: 2,
  convRule: rule92
}, {
  start: 68331,
  length: 5,
  convRule: rule17
}, {
  start: 68336,
  length: 7,
  convRule: rule2
}, {
  start: 68352,
  length: 54,
  convRule: rule14
}, {
  start: 68409,
  length: 7,
  convRule: rule2
}, {
  start: 68416,
  length: 22,
  convRule: rule14
}, {
  start: 68440,
  length: 8,
  convRule: rule17
}, {
  start: 68448,
  length: 19,
  convRule: rule14
}, {
  start: 68472,
  length: 8,
  convRule: rule17
}, {
  start: 68480,
  length: 18,
  convRule: rule14
}, {
  start: 68505,
  length: 4,
  convRule: rule2
}, {
  start: 68521,
  length: 7,
  convRule: rule17
}, {
  start: 68608,
  length: 73,
  convRule: rule14
}, {
  start: 68736,
  length: 51,
  convRule: rule97
}, {
  start: 68800,
  length: 51,
  convRule: rule102
}, {
  start: 68858,
  length: 6,
  convRule: rule17
}, {
  start: 68864,
  length: 36,
  convRule: rule14
}, {
  start: 68900,
  length: 4,
  convRule: rule92
}, {
  start: 68912,
  length: 10,
  convRule: rule8
}, {
  start: 69216,
  length: 31,
  convRule: rule17
}, {
  start: 69248,
  length: 42,
  convRule: rule14
}, {
  start: 69291,
  length: 2,
  convRule: rule92
}, {
  start: 69293,
  length: 1,
  convRule: rule7
}, {
  start: 69296,
  length: 2,
  convRule: rule14
}, {
  start: 69376,
  length: 29,
  convRule: rule14
}, {
  start: 69405,
  length: 10,
  convRule: rule17
}, {
  start: 69415,
  length: 1,
  convRule: rule14
}, {
  start: 69424,
  length: 22,
  convRule: rule14
}, {
  start: 69446,
  length: 11,
  convRule: rule92
}, {
  start: 69457,
  length: 4,
  convRule: rule17
}, {
  start: 69461,
  length: 5,
  convRule: rule2
}, {
  start: 69552,
  length: 21,
  convRule: rule14
}, {
  start: 69573,
  length: 7,
  convRule: rule17
}, {
  start: 69600,
  length: 23,
  convRule: rule14
}, {
  start: 69632,
  length: 1,
  convRule: rule124
}, {
  start: 69633,
  length: 1,
  convRule: rule92
}, {
  start: 69634,
  length: 1,
  convRule: rule124
}, {
  start: 69635,
  length: 53,
  convRule: rule14
}, {
  start: 69688,
  length: 15,
  convRule: rule92
}, {
  start: 69703,
  length: 7,
  convRule: rule2
}, {
  start: 69714,
  length: 20,
  convRule: rule17
}, {
  start: 69734,
  length: 10,
  convRule: rule8
}, {
  start: 69759,
  length: 3,
  convRule: rule92
}, {
  start: 69762,
  length: 1,
  convRule: rule124
}, {
  start: 69763,
  length: 45,
  convRule: rule14
}, {
  start: 69808,
  length: 3,
  convRule: rule124
}, {
  start: 69811,
  length: 4,
  convRule: rule92
}, {
  start: 69815,
  length: 2,
  convRule: rule124
}, {
  start: 69817,
  length: 2,
  convRule: rule92
}, {
  start: 69819,
  length: 2,
  convRule: rule2
}, {
  start: 69821,
  length: 1,
  convRule: rule16
}, {
  start: 69822,
  length: 4,
  convRule: rule2
}, {
  start: 69837,
  length: 1,
  convRule: rule16
}, {
  start: 69840,
  length: 25,
  convRule: rule14
}, {
  start: 69872,
  length: 10,
  convRule: rule8
}, {
  start: 69888,
  length: 3,
  convRule: rule92
}, {
  start: 69891,
  length: 36,
  convRule: rule14
}, {
  start: 69927,
  length: 5,
  convRule: rule92
}, {
  start: 69932,
  length: 1,
  convRule: rule124
}, {
  start: 69933,
  length: 8,
  convRule: rule92
}, {
  start: 69942,
  length: 10,
  convRule: rule8
}, {
  start: 69952,
  length: 4,
  convRule: rule2
}, {
  start: 69956,
  length: 1,
  convRule: rule14
}, {
  start: 69957,
  length: 2,
  convRule: rule124
}, {
  start: 69959,
  length: 1,
  convRule: rule14
}, {
  start: 69968,
  length: 35,
  convRule: rule14
}, {
  start: 70003,
  length: 1,
  convRule: rule92
}, {
  start: 70004,
  length: 2,
  convRule: rule2
}, {
  start: 70006,
  length: 1,
  convRule: rule14
}, {
  start: 70016,
  length: 2,
  convRule: rule92
}, {
  start: 70018,
  length: 1,
  convRule: rule124
}, {
  start: 70019,
  length: 48,
  convRule: rule14
}, {
  start: 70067,
  length: 3,
  convRule: rule124
}, {
  start: 70070,
  length: 9,
  convRule: rule92
}, {
  start: 70079,
  length: 2,
  convRule: rule124
}, {
  start: 70081,
  length: 4,
  convRule: rule14
}, {
  start: 70085,
  length: 4,
  convRule: rule2
}, {
  start: 70089,
  length: 4,
  convRule: rule92
}, {
  start: 70093,
  length: 1,
  convRule: rule2
}, {
  start: 70094,
  length: 1,
  convRule: rule124
}, {
  start: 70095,
  length: 1,
  convRule: rule92
}, {
  start: 70096,
  length: 10,
  convRule: rule8
}, {
  start: 70106,
  length: 1,
  convRule: rule14
}, {
  start: 70107,
  length: 1,
  convRule: rule2
}, {
  start: 70108,
  length: 1,
  convRule: rule14
}, {
  start: 70109,
  length: 3,
  convRule: rule2
}, {
  start: 70113,
  length: 20,
  convRule: rule17
}, {
  start: 70144,
  length: 18,
  convRule: rule14
}, {
  start: 70163,
  length: 25,
  convRule: rule14
}, {
  start: 70188,
  length: 3,
  convRule: rule124
}, {
  start: 70191,
  length: 3,
  convRule: rule92
}, {
  start: 70194,
  length: 2,
  convRule: rule124
}, {
  start: 70196,
  length: 1,
  convRule: rule92
}, {
  start: 70197,
  length: 1,
  convRule: rule124
}, {
  start: 70198,
  length: 2,
  convRule: rule92
}, {
  start: 70200,
  length: 6,
  convRule: rule2
}, {
  start: 70206,
  length: 1,
  convRule: rule92
}, {
  start: 70272,
  length: 7,
  convRule: rule14
}, {
  start: 70280,
  length: 1,
  convRule: rule14
}, {
  start: 70282,
  length: 4,
  convRule: rule14
}, {
  start: 70287,
  length: 15,
  convRule: rule14
}, {
  start: 70303,
  length: 10,
  convRule: rule14
}, {
  start: 70313,
  length: 1,
  convRule: rule2
}, {
  start: 70320,
  length: 47,
  convRule: rule14
}, {
  start: 70367,
  length: 1,
  convRule: rule92
}, {
  start: 70368,
  length: 3,
  convRule: rule124
}, {
  start: 70371,
  length: 8,
  convRule: rule92
}, {
  start: 70384,
  length: 10,
  convRule: rule8
}, {
  start: 70400,
  length: 2,
  convRule: rule92
}, {
  start: 70402,
  length: 2,
  convRule: rule124
}, {
  start: 70405,
  length: 8,
  convRule: rule14
}, {
  start: 70415,
  length: 2,
  convRule: rule14
}, {
  start: 70419,
  length: 22,
  convRule: rule14
}, {
  start: 70442,
  length: 7,
  convRule: rule14
}, {
  start: 70450,
  length: 2,
  convRule: rule14
}, {
  start: 70453,
  length: 5,
  convRule: rule14
}, {
  start: 70459,
  length: 2,
  convRule: rule92
}, {
  start: 70461,
  length: 1,
  convRule: rule14
}, {
  start: 70462,
  length: 2,
  convRule: rule124
}, {
  start: 70464,
  length: 1,
  convRule: rule92
}, {
  start: 70465,
  length: 4,
  convRule: rule124
}, {
  start: 70471,
  length: 2,
  convRule: rule124
}, {
  start: 70475,
  length: 3,
  convRule: rule124
}, {
  start: 70480,
  length: 1,
  convRule: rule14
}, {
  start: 70487,
  length: 1,
  convRule: rule124
}, {
  start: 70493,
  length: 5,
  convRule: rule14
}, {
  start: 70498,
  length: 2,
  convRule: rule124
}, {
  start: 70502,
  length: 7,
  convRule: rule92
}, {
  start: 70512,
  length: 5,
  convRule: rule92
}, {
  start: 70656,
  length: 53,
  convRule: rule14
}, {
  start: 70709,
  length: 3,
  convRule: rule124
}, {
  start: 70712,
  length: 8,
  convRule: rule92
}, {
  start: 70720,
  length: 2,
  convRule: rule124
}, {
  start: 70722,
  length: 3,
  convRule: rule92
}, {
  start: 70725,
  length: 1,
  convRule: rule124
}, {
  start: 70726,
  length: 1,
  convRule: rule92
}, {
  start: 70727,
  length: 4,
  convRule: rule14
}, {
  start: 70731,
  length: 5,
  convRule: rule2
}, {
  start: 70736,
  length: 10,
  convRule: rule8
}, {
  start: 70746,
  length: 2,
  convRule: rule2
}, {
  start: 70749,
  length: 1,
  convRule: rule2
}, {
  start: 70750,
  length: 1,
  convRule: rule92
}, {
  start: 70751,
  length: 3,
  convRule: rule14
}, {
  start: 70784,
  length: 48,
  convRule: rule14
}, {
  start: 70832,
  length: 3,
  convRule: rule124
}, {
  start: 70835,
  length: 6,
  convRule: rule92
}, {
  start: 70841,
  length: 1,
  convRule: rule124
}, {
  start: 70842,
  length: 1,
  convRule: rule92
}, {
  start: 70843,
  length: 4,
  convRule: rule124
}, {
  start: 70847,
  length: 2,
  convRule: rule92
}, {
  start: 70849,
  length: 1,
  convRule: rule124
}, {
  start: 70850,
  length: 2,
  convRule: rule92
}, {
  start: 70852,
  length: 2,
  convRule: rule14
}, {
  start: 70854,
  length: 1,
  convRule: rule2
}, {
  start: 70855,
  length: 1,
  convRule: rule14
}, {
  start: 70864,
  length: 10,
  convRule: rule8
}, {
  start: 71040,
  length: 47,
  convRule: rule14
}, {
  start: 71087,
  length: 3,
  convRule: rule124
}, {
  start: 71090,
  length: 4,
  convRule: rule92
}, {
  start: 71096,
  length: 4,
  convRule: rule124
}, {
  start: 71100,
  length: 2,
  convRule: rule92
}, {
  start: 71102,
  length: 1,
  convRule: rule124
}, {
  start: 71103,
  length: 2,
  convRule: rule92
}, {
  start: 71105,
  length: 23,
  convRule: rule2
}, {
  start: 71128,
  length: 4,
  convRule: rule14
}, {
  start: 71132,
  length: 2,
  convRule: rule92
}, {
  start: 71168,
  length: 48,
  convRule: rule14
}, {
  start: 71216,
  length: 3,
  convRule: rule124
}, {
  start: 71219,
  length: 8,
  convRule: rule92
}, {
  start: 71227,
  length: 2,
  convRule: rule124
}, {
  start: 71229,
  length: 1,
  convRule: rule92
}, {
  start: 71230,
  length: 1,
  convRule: rule124
}, {
  start: 71231,
  length: 2,
  convRule: rule92
}, {
  start: 71233,
  length: 3,
  convRule: rule2
}, {
  start: 71236,
  length: 1,
  convRule: rule14
}, {
  start: 71248,
  length: 10,
  convRule: rule8
}, {
  start: 71264,
  length: 13,
  convRule: rule2
}, {
  start: 71296,
  length: 43,
  convRule: rule14
}, {
  start: 71339,
  length: 1,
  convRule: rule92
}, {
  start: 71340,
  length: 1,
  convRule: rule124
}, {
  start: 71341,
  length: 1,
  convRule: rule92
}, {
  start: 71342,
  length: 2,
  convRule: rule124
}, {
  start: 71344,
  length: 6,
  convRule: rule92
}, {
  start: 71350,
  length: 1,
  convRule: rule124
}, {
  start: 71351,
  length: 1,
  convRule: rule92
}, {
  start: 71352,
  length: 1,
  convRule: rule14
}, {
  start: 71360,
  length: 10,
  convRule: rule8
}, {
  start: 71424,
  length: 27,
  convRule: rule14
}, {
  start: 71453,
  length: 3,
  convRule: rule92
}, {
  start: 71456,
  length: 2,
  convRule: rule124
}, {
  start: 71458,
  length: 4,
  convRule: rule92
}, {
  start: 71462,
  length: 1,
  convRule: rule124
}, {
  start: 71463,
  length: 5,
  convRule: rule92
}, {
  start: 71472,
  length: 10,
  convRule: rule8
}, {
  start: 71482,
  length: 2,
  convRule: rule17
}, {
  start: 71484,
  length: 3,
  convRule: rule2
}, {
  start: 71487,
  length: 1,
  convRule: rule13
}, {
  start: 71680,
  length: 44,
  convRule: rule14
}, {
  start: 71724,
  length: 3,
  convRule: rule124
}, {
  start: 71727,
  length: 9,
  convRule: rule92
}, {
  start: 71736,
  length: 1,
  convRule: rule124
}, {
  start: 71737,
  length: 2,
  convRule: rule92
}, {
  start: 71739,
  length: 1,
  convRule: rule2
}, {
  start: 71840,
  length: 32,
  convRule: rule9
}, {
  start: 71872,
  length: 32,
  convRule: rule12
}, {
  start: 71904,
  length: 10,
  convRule: rule8
}, {
  start: 71914,
  length: 9,
  convRule: rule17
}, {
  start: 71935,
  length: 8,
  convRule: rule14
}, {
  start: 71945,
  length: 1,
  convRule: rule14
}, {
  start: 71948,
  length: 8,
  convRule: rule14
}, {
  start: 71957,
  length: 2,
  convRule: rule14
}, {
  start: 71960,
  length: 24,
  convRule: rule14
}, {
  start: 71984,
  length: 6,
  convRule: rule124
}, {
  start: 71991,
  length: 2,
  convRule: rule124
}, {
  start: 71995,
  length: 2,
  convRule: rule92
}, {
  start: 71997,
  length: 1,
  convRule: rule124
}, {
  start: 71998,
  length: 1,
  convRule: rule92
}, {
  start: 71999,
  length: 1,
  convRule: rule14
}, {
  start: 72e3,
  length: 1,
  convRule: rule124
}, {
  start: 72001,
  length: 1,
  convRule: rule14
}, {
  start: 72002,
  length: 1,
  convRule: rule124
}, {
  start: 72003,
  length: 1,
  convRule: rule92
}, {
  start: 72004,
  length: 3,
  convRule: rule2
}, {
  start: 72016,
  length: 10,
  convRule: rule8
}, {
  start: 72096,
  length: 8,
  convRule: rule14
}, {
  start: 72106,
  length: 39,
  convRule: rule14
}, {
  start: 72145,
  length: 3,
  convRule: rule124
}, {
  start: 72148,
  length: 4,
  convRule: rule92
}, {
  start: 72154,
  length: 2,
  convRule: rule92
}, {
  start: 72156,
  length: 4,
  convRule: rule124
}, {
  start: 72160,
  length: 1,
  convRule: rule92
}, {
  start: 72161,
  length: 1,
  convRule: rule14
}, {
  start: 72162,
  length: 1,
  convRule: rule2
}, {
  start: 72163,
  length: 1,
  convRule: rule14
}, {
  start: 72164,
  length: 1,
  convRule: rule124
}, {
  start: 72192,
  length: 1,
  convRule: rule14
}, {
  start: 72193,
  length: 10,
  convRule: rule92
}, {
  start: 72203,
  length: 40,
  convRule: rule14
}, {
  start: 72243,
  length: 6,
  convRule: rule92
}, {
  start: 72249,
  length: 1,
  convRule: rule124
}, {
  start: 72250,
  length: 1,
  convRule: rule14
}, {
  start: 72251,
  length: 4,
  convRule: rule92
}, {
  start: 72255,
  length: 8,
  convRule: rule2
}, {
  start: 72263,
  length: 1,
  convRule: rule92
}, {
  start: 72272,
  length: 1,
  convRule: rule14
}, {
  start: 72273,
  length: 6,
  convRule: rule92
}, {
  start: 72279,
  length: 2,
  convRule: rule124
}, {
  start: 72281,
  length: 3,
  convRule: rule92
}, {
  start: 72284,
  length: 46,
  convRule: rule14
}, {
  start: 72330,
  length: 13,
  convRule: rule92
}, {
  start: 72343,
  length: 1,
  convRule: rule124
}, {
  start: 72344,
  length: 2,
  convRule: rule92
}, {
  start: 72346,
  length: 3,
  convRule: rule2
}, {
  start: 72349,
  length: 1,
  convRule: rule14
}, {
  start: 72350,
  length: 5,
  convRule: rule2
}, {
  start: 72384,
  length: 57,
  convRule: rule14
}, {
  start: 72704,
  length: 9,
  convRule: rule14
}, {
  start: 72714,
  length: 37,
  convRule: rule14
}, {
  start: 72751,
  length: 1,
  convRule: rule124
}, {
  start: 72752,
  length: 7,
  convRule: rule92
}, {
  start: 72760,
  length: 6,
  convRule: rule92
}, {
  start: 72766,
  length: 1,
  convRule: rule124
}, {
  start: 72767,
  length: 1,
  convRule: rule92
}, {
  start: 72768,
  length: 1,
  convRule: rule14
}, {
  start: 72769,
  length: 5,
  convRule: rule2
}, {
  start: 72784,
  length: 10,
  convRule: rule8
}, {
  start: 72794,
  length: 19,
  convRule: rule17
}, {
  start: 72816,
  length: 2,
  convRule: rule2
}, {
  start: 72818,
  length: 30,
  convRule: rule14
}, {
  start: 72850,
  length: 22,
  convRule: rule92
}, {
  start: 72873,
  length: 1,
  convRule: rule124
}, {
  start: 72874,
  length: 7,
  convRule: rule92
}, {
  start: 72881,
  length: 1,
  convRule: rule124
}, {
  start: 72882,
  length: 2,
  convRule: rule92
}, {
  start: 72884,
  length: 1,
  convRule: rule124
}, {
  start: 72885,
  length: 2,
  convRule: rule92
}, {
  start: 72960,
  length: 7,
  convRule: rule14
}, {
  start: 72968,
  length: 2,
  convRule: rule14
}, {
  start: 72971,
  length: 38,
  convRule: rule14
}, {
  start: 73009,
  length: 6,
  convRule: rule92
}, {
  start: 73018,
  length: 1,
  convRule: rule92
}, {
  start: 73020,
  length: 2,
  convRule: rule92
}, {
  start: 73023,
  length: 7,
  convRule: rule92
}, {
  start: 73030,
  length: 1,
  convRule: rule14
}, {
  start: 73031,
  length: 1,
  convRule: rule92
}, {
  start: 73040,
  length: 10,
  convRule: rule8
}, {
  start: 73056,
  length: 6,
  convRule: rule14
}, {
  start: 73063,
  length: 2,
  convRule: rule14
}, {
  start: 73066,
  length: 32,
  convRule: rule14
}, {
  start: 73098,
  length: 5,
  convRule: rule124
}, {
  start: 73104,
  length: 2,
  convRule: rule92
}, {
  start: 73107,
  length: 2,
  convRule: rule124
}, {
  start: 73109,
  length: 1,
  convRule: rule92
}, {
  start: 73110,
  length: 1,
  convRule: rule124
}, {
  start: 73111,
  length: 1,
  convRule: rule92
}, {
  start: 73112,
  length: 1,
  convRule: rule14
}, {
  start: 73120,
  length: 10,
  convRule: rule8
}, {
  start: 73440,
  length: 19,
  convRule: rule14
}, {
  start: 73459,
  length: 2,
  convRule: rule92
}, {
  start: 73461,
  length: 2,
  convRule: rule124
}, {
  start: 73463,
  length: 2,
  convRule: rule2
}, {
  start: 73648,
  length: 1,
  convRule: rule14
}, {
  start: 73664,
  length: 21,
  convRule: rule17
}, {
  start: 73685,
  length: 8,
  convRule: rule13
}, {
  start: 73693,
  length: 4,
  convRule: rule3
}, {
  start: 73697,
  length: 17,
  convRule: rule13
}, {
  start: 73727,
  length: 1,
  convRule: rule2
}, {
  start: 73728,
  length: 922,
  convRule: rule14
}, {
  start: 74752,
  length: 111,
  convRule: rule128
}, {
  start: 74864,
  length: 5,
  convRule: rule2
}, {
  start: 74880,
  length: 196,
  convRule: rule14
}, {
  start: 77824,
  length: 1071,
  convRule: rule14
}, {
  start: 78896,
  length: 9,
  convRule: rule16
}, {
  start: 82944,
  length: 583,
  convRule: rule14
}, {
  start: 92160,
  length: 569,
  convRule: rule14
}, {
  start: 92736,
  length: 31,
  convRule: rule14
}, {
  start: 92768,
  length: 10,
  convRule: rule8
}, {
  start: 92782,
  length: 2,
  convRule: rule2
}, {
  start: 92880,
  length: 30,
  convRule: rule14
}, {
  start: 92912,
  length: 5,
  convRule: rule92
}, {
  start: 92917,
  length: 1,
  convRule: rule2
}, {
  start: 92928,
  length: 48,
  convRule: rule14
}, {
  start: 92976,
  length: 7,
  convRule: rule92
}, {
  start: 92983,
  length: 5,
  convRule: rule2
}, {
  start: 92988,
  length: 4,
  convRule: rule13
}, {
  start: 92992,
  length: 4,
  convRule: rule91
}, {
  start: 92996,
  length: 1,
  convRule: rule2
}, {
  start: 92997,
  length: 1,
  convRule: rule13
}, {
  start: 93008,
  length: 10,
  convRule: rule8
}, {
  start: 93019,
  length: 7,
  convRule: rule17
}, {
  start: 93027,
  length: 21,
  convRule: rule14
}, {
  start: 93053,
  length: 19,
  convRule: rule14
}, {
  start: 93760,
  length: 32,
  convRule: rule9
}, {
  start: 93792,
  length: 32,
  convRule: rule12
}, {
  start: 93824,
  length: 23,
  convRule: rule17
}, {
  start: 93847,
  length: 4,
  convRule: rule2
}, {
  start: 93952,
  length: 75,
  convRule: rule14
}, {
  start: 94031,
  length: 1,
  convRule: rule92
}, {
  start: 94032,
  length: 1,
  convRule: rule14
}, {
  start: 94033,
  length: 55,
  convRule: rule124
}, {
  start: 94095,
  length: 4,
  convRule: rule92
}, {
  start: 94099,
  length: 13,
  convRule: rule91
}, {
  start: 94176,
  length: 2,
  convRule: rule91
}, {
  start: 94178,
  length: 1,
  convRule: rule2
}, {
  start: 94179,
  length: 1,
  convRule: rule91
}, {
  start: 94180,
  length: 1,
  convRule: rule92
}, {
  start: 94192,
  length: 2,
  convRule: rule124
}, {
  start: 94208,
  length: 6136,
  convRule: rule14
}, {
  start: 100352,
  length: 1238,
  convRule: rule14
}, {
  start: 101632,
  length: 9,
  convRule: rule14
}, {
  start: 110592,
  length: 287,
  convRule: rule14
}, {
  start: 110928,
  length: 3,
  convRule: rule14
}, {
  start: 110948,
  length: 4,
  convRule: rule14
}, {
  start: 110960,
  length: 396,
  convRule: rule14
}, {
  start: 113664,
  length: 107,
  convRule: rule14
}, {
  start: 113776,
  length: 13,
  convRule: rule14
}, {
  start: 113792,
  length: 9,
  convRule: rule14
}, {
  start: 113808,
  length: 10,
  convRule: rule14
}, {
  start: 113820,
  length: 1,
  convRule: rule13
}, {
  start: 113821,
  length: 2,
  convRule: rule92
}, {
  start: 113823,
  length: 1,
  convRule: rule2
}, {
  start: 113824,
  length: 4,
  convRule: rule16
}, {
  start: 118784,
  length: 246,
  convRule: rule13
}, {
  start: 119040,
  length: 39,
  convRule: rule13
}, {
  start: 119081,
  length: 60,
  convRule: rule13
}, {
  start: 119141,
  length: 2,
  convRule: rule124
}, {
  start: 119143,
  length: 3,
  convRule: rule92
}, {
  start: 119146,
  length: 3,
  convRule: rule13
}, {
  start: 119149,
  length: 6,
  convRule: rule124
}, {
  start: 119155,
  length: 8,
  convRule: rule16
}, {
  start: 119163,
  length: 8,
  convRule: rule92
}, {
  start: 119171,
  length: 2,
  convRule: rule13
}, {
  start: 119173,
  length: 7,
  convRule: rule92
}, {
  start: 119180,
  length: 30,
  convRule: rule13
}, {
  start: 119210,
  length: 4,
  convRule: rule92
}, {
  start: 119214,
  length: 59,
  convRule: rule13
}, {
  start: 119296,
  length: 66,
  convRule: rule13
}, {
  start: 119362,
  length: 3,
  convRule: rule92
}, {
  start: 119365,
  length: 1,
  convRule: rule13
}, {
  start: 119520,
  length: 20,
  convRule: rule17
}, {
  start: 119552,
  length: 87,
  convRule: rule13
}, {
  start: 119648,
  length: 25,
  convRule: rule17
}, {
  start: 119808,
  length: 26,
  convRule: rule107
}, {
  start: 119834,
  length: 26,
  convRule: rule20
}, {
  start: 119860,
  length: 26,
  convRule: rule107
}, {
  start: 119886,
  length: 7,
  convRule: rule20
}, {
  start: 119894,
  length: 18,
  convRule: rule20
}, {
  start: 119912,
  length: 26,
  convRule: rule107
}, {
  start: 119938,
  length: 26,
  convRule: rule20
}, {
  start: 119964,
  length: 1,
  convRule: rule107
}, {
  start: 119966,
  length: 2,
  convRule: rule107
}, {
  start: 119970,
  length: 1,
  convRule: rule107
}, {
  start: 119973,
  length: 2,
  convRule: rule107
}, {
  start: 119977,
  length: 4,
  convRule: rule107
}, {
  start: 119982,
  length: 8,
  convRule: rule107
}, {
  start: 119990,
  length: 4,
  convRule: rule20
}, {
  start: 119995,
  length: 1,
  convRule: rule20
}, {
  start: 119997,
  length: 7,
  convRule: rule20
}, {
  start: 120005,
  length: 11,
  convRule: rule20
}, {
  start: 120016,
  length: 26,
  convRule: rule107
}, {
  start: 120042,
  length: 26,
  convRule: rule20
}, {
  start: 120068,
  length: 2,
  convRule: rule107
}, {
  start: 120071,
  length: 4,
  convRule: rule107
}, {
  start: 120077,
  length: 8,
  convRule: rule107
}, {
  start: 120086,
  length: 7,
  convRule: rule107
}, {
  start: 120094,
  length: 26,
  convRule: rule20
}, {
  start: 120120,
  length: 2,
  convRule: rule107
}, {
  start: 120123,
  length: 4,
  convRule: rule107
}, {
  start: 120128,
  length: 5,
  convRule: rule107
}, {
  start: 120134,
  length: 1,
  convRule: rule107
}, {
  start: 120138,
  length: 7,
  convRule: rule107
}, {
  start: 120146,
  length: 26,
  convRule: rule20
}, {
  start: 120172,
  length: 26,
  convRule: rule107
}, {
  start: 120198,
  length: 26,
  convRule: rule20
}, {
  start: 120224,
  length: 26,
  convRule: rule107
}, {
  start: 120250,
  length: 26,
  convRule: rule20
}, {
  start: 120276,
  length: 26,
  convRule: rule107
}, {
  start: 120302,
  length: 26,
  convRule: rule20
}, {
  start: 120328,
  length: 26,
  convRule: rule107
}, {
  start: 120354,
  length: 26,
  convRule: rule20
}, {
  start: 120380,
  length: 26,
  convRule: rule107
}, {
  start: 120406,
  length: 26,
  convRule: rule20
}, {
  start: 120432,
  length: 26,
  convRule: rule107
}, {
  start: 120458,
  length: 28,
  convRule: rule20
}, {
  start: 120488,
  length: 25,
  convRule: rule107
}, {
  start: 120513,
  length: 1,
  convRule: rule6
}, {
  start: 120514,
  length: 25,
  convRule: rule20
}, {
  start: 120539,
  length: 1,
  convRule: rule6
}, {
  start: 120540,
  length: 6,
  convRule: rule20
}, {
  start: 120546,
  length: 25,
  convRule: rule107
}, {
  start: 120571,
  length: 1,
  convRule: rule6
}, {
  start: 120572,
  length: 25,
  convRule: rule20
}, {
  start: 120597,
  length: 1,
  convRule: rule6
}, {
  start: 120598,
  length: 6,
  convRule: rule20
}, {
  start: 120604,
  length: 25,
  convRule: rule107
}, {
  start: 120629,
  length: 1,
  convRule: rule6
}, {
  start: 120630,
  length: 25,
  convRule: rule20
}, {
  start: 120655,
  length: 1,
  convRule: rule6
}, {
  start: 120656,
  length: 6,
  convRule: rule20
}, {
  start: 120662,
  length: 25,
  convRule: rule107
}, {
  start: 120687,
  length: 1,
  convRule: rule6
}, {
  start: 120688,
  length: 25,
  convRule: rule20
}, {
  start: 120713,
  length: 1,
  convRule: rule6
}, {
  start: 120714,
  length: 6,
  convRule: rule20
}, {
  start: 120720,
  length: 25,
  convRule: rule107
}, {
  start: 120745,
  length: 1,
  convRule: rule6
}, {
  start: 120746,
  length: 25,
  convRule: rule20
}, {
  start: 120771,
  length: 1,
  convRule: rule6
}, {
  start: 120772,
  length: 6,
  convRule: rule20
}, {
  start: 120778,
  length: 1,
  convRule: rule107
}, {
  start: 120779,
  length: 1,
  convRule: rule20
}, {
  start: 120782,
  length: 50,
  convRule: rule8
}, {
  start: 120832,
  length: 512,
  convRule: rule13
}, {
  start: 121344,
  length: 55,
  convRule: rule92
}, {
  start: 121399,
  length: 4,
  convRule: rule13
}, {
  start: 121403,
  length: 50,
  convRule: rule92
}, {
  start: 121453,
  length: 8,
  convRule: rule13
}, {
  start: 121461,
  length: 1,
  convRule: rule92
}, {
  start: 121462,
  length: 14,
  convRule: rule13
}, {
  start: 121476,
  length: 1,
  convRule: rule92
}, {
  start: 121477,
  length: 2,
  convRule: rule13
}, {
  start: 121479,
  length: 5,
  convRule: rule2
}, {
  start: 121499,
  length: 5,
  convRule: rule92
}, {
  start: 121505,
  length: 15,
  convRule: rule92
}, {
  start: 122880,
  length: 7,
  convRule: rule92
}, {
  start: 122888,
  length: 17,
  convRule: rule92
}, {
  start: 122907,
  length: 7,
  convRule: rule92
}, {
  start: 122915,
  length: 2,
  convRule: rule92
}, {
  start: 122918,
  length: 5,
  convRule: rule92
}, {
  start: 123136,
  length: 45,
  convRule: rule14
}, {
  start: 123184,
  length: 7,
  convRule: rule92
}, {
  start: 123191,
  length: 7,
  convRule: rule91
}, {
  start: 123200,
  length: 10,
  convRule: rule8
}, {
  start: 123214,
  length: 1,
  convRule: rule14
}, {
  start: 123215,
  length: 1,
  convRule: rule13
}, {
  start: 123584,
  length: 44,
  convRule: rule14
}, {
  start: 123628,
  length: 4,
  convRule: rule92
}, {
  start: 123632,
  length: 10,
  convRule: rule8
}, {
  start: 123647,
  length: 1,
  convRule: rule3
}, {
  start: 124928,
  length: 197,
  convRule: rule14
}, {
  start: 125127,
  length: 9,
  convRule: rule17
}, {
  start: 125136,
  length: 7,
  convRule: rule92
}, {
  start: 125184,
  length: 34,
  convRule: rule203
}, {
  start: 125218,
  length: 34,
  convRule: rule204
}, {
  start: 125252,
  length: 7,
  convRule: rule92
}, {
  start: 125259,
  length: 1,
  convRule: rule91
}, {
  start: 125264,
  length: 10,
  convRule: rule8
}, {
  start: 125278,
  length: 2,
  convRule: rule2
}, {
  start: 126065,
  length: 59,
  convRule: rule17
}, {
  start: 126124,
  length: 1,
  convRule: rule13
}, {
  start: 126125,
  length: 3,
  convRule: rule17
}, {
  start: 126128,
  length: 1,
  convRule: rule3
}, {
  start: 126129,
  length: 4,
  convRule: rule17
}, {
  start: 126209,
  length: 45,
  convRule: rule17
}, {
  start: 126254,
  length: 1,
  convRule: rule13
}, {
  start: 126255,
  length: 15,
  convRule: rule17
}, {
  start: 126464,
  length: 4,
  convRule: rule14
}, {
  start: 126469,
  length: 27,
  convRule: rule14
}, {
  start: 126497,
  length: 2,
  convRule: rule14
}, {
  start: 126500,
  length: 1,
  convRule: rule14
}, {
  start: 126503,
  length: 1,
  convRule: rule14
}, {
  start: 126505,
  length: 10,
  convRule: rule14
}, {
  start: 126516,
  length: 4,
  convRule: rule14
}, {
  start: 126521,
  length: 1,
  convRule: rule14
}, {
  start: 126523,
  length: 1,
  convRule: rule14
}, {
  start: 126530,
  length: 1,
  convRule: rule14
}, {
  start: 126535,
  length: 1,
  convRule: rule14
}, {
  start: 126537,
  length: 1,
  convRule: rule14
}, {
  start: 126539,
  length: 1,
  convRule: rule14
}, {
  start: 126541,
  length: 3,
  convRule: rule14
}, {
  start: 126545,
  length: 2,
  convRule: rule14
}, {
  start: 126548,
  length: 1,
  convRule: rule14
}, {
  start: 126551,
  length: 1,
  convRule: rule14
}, {
  start: 126553,
  length: 1,
  convRule: rule14
}, {
  start: 126555,
  length: 1,
  convRule: rule14
}, {
  start: 126557,
  length: 1,
  convRule: rule14
}, {
  start: 126559,
  length: 1,
  convRule: rule14
}, {
  start: 126561,
  length: 2,
  convRule: rule14
}, {
  start: 126564,
  length: 1,
  convRule: rule14
}, {
  start: 126567,
  length: 4,
  convRule: rule14
}, {
  start: 126572,
  length: 7,
  convRule: rule14
}, {
  start: 126580,
  length: 4,
  convRule: rule14
}, {
  start: 126585,
  length: 4,
  convRule: rule14
}, {
  start: 126590,
  length: 1,
  convRule: rule14
}, {
  start: 126592,
  length: 10,
  convRule: rule14
}, {
  start: 126603,
  length: 17,
  convRule: rule14
}, {
  start: 126625,
  length: 3,
  convRule: rule14
}, {
  start: 126629,
  length: 5,
  convRule: rule14
}, {
  start: 126635,
  length: 17,
  convRule: rule14
}, {
  start: 126704,
  length: 2,
  convRule: rule6
}, {
  start: 126976,
  length: 44,
  convRule: rule13
}, {
  start: 127024,
  length: 100,
  convRule: rule13
}, {
  start: 127136,
  length: 15,
  convRule: rule13
}, {
  start: 127153,
  length: 15,
  convRule: rule13
}, {
  start: 127169,
  length: 15,
  convRule: rule13
}, {
  start: 127185,
  length: 37,
  convRule: rule13
}, {
  start: 127232,
  length: 13,
  convRule: rule17
}, {
  start: 127245,
  length: 161,
  convRule: rule13
}, {
  start: 127462,
  length: 29,
  convRule: rule13
}, {
  start: 127504,
  length: 44,
  convRule: rule13
}, {
  start: 127552,
  length: 9,
  convRule: rule13
}, {
  start: 127568,
  length: 2,
  convRule: rule13
}, {
  start: 127584,
  length: 6,
  convRule: rule13
}, {
  start: 127744,
  length: 251,
  convRule: rule13
}, {
  start: 127995,
  length: 5,
  convRule: rule10
}, {
  start: 128e3,
  length: 728,
  convRule: rule13
}, {
  start: 128736,
  length: 13,
  convRule: rule13
}, {
  start: 128752,
  length: 13,
  convRule: rule13
}, {
  start: 128768,
  length: 116,
  convRule: rule13
}, {
  start: 128896,
  length: 89,
  convRule: rule13
}, {
  start: 128992,
  length: 12,
  convRule: rule13
}, {
  start: 129024,
  length: 12,
  convRule: rule13
}, {
  start: 129040,
  length: 56,
  convRule: rule13
}, {
  start: 129104,
  length: 10,
  convRule: rule13
}, {
  start: 129120,
  length: 40,
  convRule: rule13
}, {
  start: 129168,
  length: 30,
  convRule: rule13
}, {
  start: 129200,
  length: 2,
  convRule: rule13
}, {
  start: 129280,
  length: 121,
  convRule: rule13
}, {
  start: 129402,
  length: 82,
  convRule: rule13
}, {
  start: 129485,
  length: 135,
  convRule: rule13
}, {
  start: 129632,
  length: 14,
  convRule: rule13
}, {
  start: 129648,
  length: 5,
  convRule: rule13
}, {
  start: 129656,
  length: 3,
  convRule: rule13
}, {
  start: 129664,
  length: 7,
  convRule: rule13
}, {
  start: 129680,
  length: 25,
  convRule: rule13
}, {
  start: 129712,
  length: 7,
  convRule: rule13
}, {
  start: 129728,
  length: 3,
  convRule: rule13
}, {
  start: 129744,
  length: 7,
  convRule: rule13
}, {
  start: 129792,
  length: 147,
  convRule: rule13
}, {
  start: 129940,
  length: 55,
  convRule: rule13
}, {
  start: 130032,
  length: 10,
  convRule: rule8
}, {
  start: 131072,
  length: 42718,
  convRule: rule14
}, {
  start: 173824,
  length: 4149,
  convRule: rule14
}, {
  start: 177984,
  length: 222,
  convRule: rule14
}, {
  start: 178208,
  length: 5762,
  convRule: rule14
}, {
  start: 183984,
  length: 7473,
  convRule: rule14
}, {
  start: 194560,
  length: 542,
  convRule: rule14
}, {
  start: 196608,
  length: 4939,
  convRule: rule14
}, {
  start: 917505,
  length: 1,
  convRule: rule16
}, {
  start: 917536,
  length: 96,
  convRule: rule16
}, {
  start: 917760,
  length: 240,
  convRule: rule92
}, {
  start: 983040,
  length: 65534,
  convRule: rule200
}, {
  start: 1048576,
  length: 65534,
  convRule: rule200
}];
var checkAttr = function(categories) {
  return function($$char2) {
    var numOfBlocks = function() {
      var $27 = $$char2 < 256;
      if ($27) {
        return numLat1Blocks;
      }
      ;
      return numBlocks;
    }();
    var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
    if (maybeConversionRule instanceof Nothing) {
      return false;
    }
    ;
    if (maybeConversionRule instanceof Just) {
      return isJust(elemIndex(eqInt)(maybeConversionRule.value0.category)(categories));
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
  };
};
var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);
var uIswalpha = /* @__PURE__ */ checkAttr([gencatLL, gencatLU, gencatLT, gencatLM, gencatLO]);
var uIswupper = /* @__PURE__ */ checkAttr([gencatLU, gencatLT]);

// output/Data.CodePoint.Unicode/index.js
var modify3 = unsafeCoerce2;
var toLowerSimple = /* @__PURE__ */ modify3(uTowlower);
var toUpperSimple = /* @__PURE__ */ modify3(uTowupper);
var isUpper = /* @__PURE__ */ function() {
  var $52 = fromEnum(boundedEnumCodePoint);
  return function($53) {
    return uIswupper($52($53));
  };
}();
var isSpace = function(c) {
  var uc = fromEnum(boundedEnumCodePoint)(c);
  var $14 = uc <= 823;
  if ($14) {
    return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
  }
  ;
  return uIswspace(uc);
};
var isOctDigit = function(c) {
  var diff = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("0") | 0;
  return diff <= 7 && diff >= 0;
};
var isDecDigit = function(c) {
  var diff = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("0") | 0;
  return diff <= 9 && diff >= 0;
};
var isHexDigit = function(c) {
  return isDecDigit(c) || (function() {
    var diff = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("A") | 0;
    return diff <= 5 && diff >= 0;
  }() || function() {
    var diff = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("a") | 0;
    return diff <= 5 && diff >= 0;
  }());
};
var isAlphaNum = /* @__PURE__ */ function() {
  var $60 = fromEnum(boundedEnumCodePoint);
  return function($61) {
    return uIswalnum($60($61));
  };
}();
var isAlpha = /* @__PURE__ */ function() {
  var $62 = fromEnum(boundedEnumCodePoint);
  return function($63) {
    return uIswalpha($62($63));
  };
}();
var hexDigitToInt = function(c) {
  var hexUpper = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("A") | 0;
  var hexLower = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("a") | 0;
  var dec = fromEnum(boundedEnumCodePoint)(c) - toCharCode2("0") | 0;
  var result = function() {
    if (dec <= 9 && dec >= 0) {
      return new Just(dec);
    }
    ;
    if (hexLower <= 5 && hexLower >= 0) {
      return new Just(hexLower + 10 | 0);
    }
    ;
    if (hexUpper <= 5 && hexUpper >= 0) {
      return new Just(hexUpper + 10 | 0);
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 591, column 3 - line 591, column 22): " + []);
  }();
  return result;
};

// output/Parsing.String.Basic/index.js
var satisfyCP = function(p) {
  return satisfy(function($10) {
    return p(codePointFromChar($10));
  });
};
var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
var upper2 = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isUpper))("uppercase letter");
var oneOf2 = function(ss) {
  return withLazyErrorMessage(satisfy(flip(elem2(eqChar))(ss)))(function(v) {
    return "one of " + show(showArray(showChar))(ss);
  });
};
var octDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isOctDigit))("oct digit");
var noneOf = function(ss) {
  return withLazyErrorMessage(satisfy(flip(notElem2(eqChar))(ss)))(function(v) {
    return "none of " + show(showArray(showChar))(ss);
  });
};
var letter = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlpha))("letter");
var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
var digit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isDecDigit))("digit");
var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

// output/Data.String.Unicode/index.js
var convert = function(f) {
  var $2 = map(functorArray)(f);
  return function($3) {
    return fromCodePointArray($2(toCodePointArray($3)));
  };
};
var toLowerSimple2 = /* @__PURE__ */ convert(toLowerSimple);
var toUpperSimple2 = /* @__PURE__ */ convert(toUpperSimple);

// output/Parsing.Token/index.js
var unGenLanguageDef = function(v) {
  return v;
};
var theReservedNames = function(v) {
  if (v.caseSensitive) {
    return sort(ordString)(v.reservedNames);
  }
  ;
  if (otherwise) {
    return sort(ordString)(map(functorArray)(toLower)(v.reservedNames));
  }
  ;
  throw new Error("Failed pattern match at Parsing.Token (line 825, column 1 - line 825, column 70): " + [v.constructor.name]);
};
var simpleSpace = /* @__PURE__ */ skipMany1(/* @__PURE__ */ satisfyCodePoint(isSpace));
var oneLineComment = function(v) {
  return applySecond(applyParserT)($$try(string(v.commentLine)))(skipMany(satisfy(function(v1) {
    return v1 !== "\n";
  })));
};
var isReserved = function($copy_names) {
  return function($copy_name) {
    var $tco_var_names = $copy_names;
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(names, name2) {
      var v = uncons2(names);
      if (v instanceof Nothing) {
        $tco_done = true;
        return false;
      }
      ;
      if (v instanceof Just) {
        var v1 = compare(ordString)(v.value0.head)(name2);
        if (v1 instanceof LT) {
          $tco_var_names = v.value0.tail;
          $copy_name = name2;
          return;
        }
        ;
        if (v1 instanceof EQ) {
          $tco_done = true;
          return true;
        }
        ;
        if (v1 instanceof GT) {
          $tco_done = true;
          return false;
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 820, column 35 - line 823, column 18): " + [v1.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 818, column 3 - line 823, column 18): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($tco_var_names, $copy_name);
    }
    ;
    return $tco_result;
  };
};
var isReservedName = function(v) {
  return function(name2) {
    var caseName = function() {
      if (v.caseSensitive) {
        return name2;
      }
      ;
      if (otherwise) {
        return toLower(name2);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 812, column 3 - line 814, column 31): " + []);
    }();
    return isReserved(theReservedNames(v))(caseName);
  };
};
var inCommentSingle = function(v) {
  var startEnd = append(semigroupArray)(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
  return fix(lazyParserT)(function(p) {
    return alt(altParserT)($$void(functorParserT)($$try(string(v.commentEnd))))(alt(altParserT)(applySecond(applyParserT)(skipMany1(noneOf(startEnd)))(p))(withErrorMessage(applySecond(applyParserT)(oneOf2(startEnd))(p))("end of comment")));
  });
};
var multiLineComment = function(v) {
  return applySecond(applyParserT)($$try(string(v.commentStart)))(inComment(v));
};
var inCommentMulti = function(v) {
  var startEnd = append(semigroupArray)(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
  return fix(lazyParserT)(function(p) {
    return alt(altParserT)($$void(functorParserT)($$try(string(v.commentEnd))))(alt(altParserT)(applySecond(applyParserT)(multiLineComment(v))(p))(alt(altParserT)(applySecond(applyParserT)(skipMany1(noneOf(startEnd)))(p))(withErrorMessage(applySecond(applyParserT)(oneOf2(startEnd))(p))("end of comment"))));
  });
};
var inComment = function(v) {
  if (v.nestedComments) {
    return inCommentMulti(v);
  }
  ;
  return inCommentSingle(v);
};
var whiteSpace$prime = function(v) {
  if ($$null(v.commentLine) && $$null(v.commentStart)) {
    return skipMany(withErrorMessage(simpleSpace)(""));
  }
  ;
  if ($$null(v.commentLine)) {
    return skipMany(alt(altParserT)(simpleSpace)(withErrorMessage(multiLineComment(v))("")));
  }
  ;
  if ($$null(v.commentStart)) {
    return skipMany(alt(altParserT)(simpleSpace)(withErrorMessage(oneLineComment(v))("")));
  }
  ;
  if (otherwise) {
    return skipMany(alt(altParserT)(simpleSpace)(alt(altParserT)(oneLineComment(v))(withErrorMessage(multiLineComment(v))(""))));
  }
  ;
  throw new Error("Failed pattern match at Parsing.Token (line 834, column 1 - line 834, column 74): " + [v.constructor.name]);
};
var makeTokenParser = function(v) {
  var stringLetter = satisfy(function(c) {
    return c !== '"' && (c !== "\\" && c > "");
  });
  var sign2 = function(dictRing) {
    return alt(altParserT)(voidLeft(functorParserT)($$char("-"))(negate(dictRing)))(alt(altParserT)(voidLeft(functorParserT)($$char("+"))(identity(categoryFn)))(pure(applicativeParserT)(identity(categoryFn))));
  };
  var oper = function() {
    var go = bind(bindParserT)(v.opStart)(function(c) {
      return bind(bindParserT)(many3(alternativeParserT)(lazyParserT)(v.opLetter))(function(cs) {
        return pure(applicativeParserT)(singleton6(c) + fromCharArray(cs));
      });
    });
    return withErrorMessage(go)("operator");
  }();
  var number2 = function(base) {
    return function(baseDigit) {
      var folder = function(v1) {
        return function(v2) {
          if (v1 instanceof Nothing) {
            return Nothing.value;
          }
          ;
          if (v1 instanceof Just) {
            return map(functorMaybe)(function(v3) {
              return (base * v1.value0 | 0) + v3 | 0;
            })(hexDigitToInt(codePointFromChar(v2)));
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 704, column 5 - line 704, column 45): " + [v1.constructor.name, v2.constructor.name]);
        };
      };
      return bind(bindParserT)(some2(alternativeParserT)(lazyParserT)(baseDigit))(function(digits) {
        return maybe(fail("not digits"))(pure(applicativeParserT))(foldl(foldableArray)(folder)(new Just(0))(digits));
      });
    };
  };
  var octal = applySecond(applyParserT)(oneOf2(["o", "O"]))(number2(8)(octDigit));
  var lexeme = function(p) {
    return applyFirst(applyParserT)(p)(whiteSpace$prime(v));
  };
  var reservedOp2 = function(name2) {
    var go = bind(bindParserT)(string(name2))(function() {
      return withErrorMessage(notFollowedBy(v.opLetter))("end of " + name2);
    });
    return lexeme($$try(go));
  };
  var symbol = function(name2) {
    return voidLeft(functorParserT)(lexeme(string(name2)))(name2);
  };
  var parens = function(p) {
    return between(symbol("("))(symbol(")"))(p);
  };
  var semi = symbol(";");
  var semiSep = function(p) {
    return sepBy(p)(semi);
  };
  var semiSep1 = function(p) {
    return sepBy1(p)(semi);
  };
  var isReservedOp = function(name2) {
    return isReserved(sort(ordString)(v.reservedOpNames))(name2);
  };
  var operator = function() {
    var go = bind(bindParserT)(oper)(function(name2) {
      var $65 = isReservedOp(name2);
      if ($65) {
        return fail("reserved operator " + name2);
      }
      ;
      return pure(applicativeParserT)(name2);
    });
    return lexeme($$try(go));
  }();
  var ident = function() {
    var go = bind(bindParserT)(v.identStart)(function(c) {
      return bind(bindParserT)(many3(alternativeParserT)(lazyParserT)(v.identLetter))(function(cs) {
        return pure(applicativeParserT)(singleton6(c) + fromCharArray(cs));
      });
    });
    return withErrorMessage(go)("identifier");
  }();
  var identifier2 = function() {
    var go = bind(bindParserT)(ident)(function(name2) {
      var $66 = isReservedName(v)(name2);
      if ($66) {
        return fail("reserved word " + show(showString)(name2));
      }
      ;
      return pure(applicativeParserT)(name2);
    });
    return lexeme($$try(go));
  }();
  var hexadecimal2 = applySecond(applyParserT)(oneOf2(["x", "X"]))(number2(16)(hexDigit));
  var fraction = function() {
    var op = function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (v2 instanceof Just) {
          return bind(bindMaybe)(hexDigitToInt(codePointFromChar(v1)))(function(int$prime) {
            return pure(applicativeMaybe)((v2.value0 + toNumber(int$prime)) / 10);
          });
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 651, column 5 - line 651, column 47): " + [v1.constructor.name, v2.constructor.name]);
      };
    };
    return asErrorMessage("fraction")(bind(bindParserT)($$char("."))(function() {
      return bind(bindParserT)(withErrorMessage(some2(alternativeParserT)(lazyParserT)(digit))("fraction"))(function(digits) {
        return maybe(fail("not digit"))(pure(applicativeParserT))(foldr(foldableArray)(op)(new Just(0))(digits));
      });
    }));
  }();
  var escapeGap = withErrorMessage(applySecond(applyParserT)(some2(alternativeParserT)(lazyParserT)(space))($$char("\\")))("end of string gap");
  var escapeEmpty = $$char("&");
  var escMap = zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"]);
  var dot = symbol(".");
  var decimal = number2(10)(digit);
  var exponent$prime = function() {
    var power = function(e) {
      if (e < 0) {
        return 1 / power(-e | 0);
      }
      ;
      if (otherwise) {
        return pow(10)(toNumber(e));
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 664, column 5 - line 664, column 27): " + [e.constructor.name]);
    };
    return asErrorMessage("exponent")(bind(bindParserT)(oneOf2(["e", "E"]))(function() {
      return bind(bindParserT)(sign2(ringInt))(function(f) {
        return bind(bindParserT)(withErrorMessage(decimal)("exponent"))(function(e) {
          return pure(applicativeParserT)(power(f(e)));
        });
      });
    }));
  }();
  var fractExponent = function(n) {
    var justExponent = bind(bindParserT)(exponent$prime)(function(expo) {
      return pure(applicativeParserT)(toNumber(n) * expo);
    });
    var fractExponent$prime = bind(bindParserT)(fraction)(function(fract) {
      return bind(bindParserT)(option(1)(exponent$prime))(function(expo) {
        return pure(applicativeParserT)((toNumber(n) + fract) * expo);
      });
    });
    return alt(altParserT)(fractExponent$prime)(justExponent);
  };
  var fractFloat = function(n) {
    return map(functorParserT)(Right.create)(fractExponent(n));
  };
  var decimalFloat = bind(bindParserT)(decimal)(function(n) {
    return option(new Left(n))(fractFloat(n));
  });
  var zeroNumFloat = alt(altParserT)(map(functorParserT)(Left.create)(alt(altParserT)(hexadecimal2)(octal)))(alt(altParserT)(decimalFloat)(alt(altParserT)(fractFloat(0))(pure(applicativeParserT)(new Left(0)))));
  var natFloat = alt(altParserT)(applySecond(applyParserT)($$char("0"))(zeroNumFloat))(decimalFloat);
  var naturalOrFloat = withErrorMessage(lexeme(natFloat))("number");
  var floating = bind(bindParserT)(decimal)(fractExponent);
  var $$float2 = withErrorMessage(lexeme(floating))("float");
  var zeroNumber = withErrorMessage(applySecond(applyParserT)($$char("0"))(alt(altParserT)(hexadecimal2)(alt(altParserT)(octal)(alt(altParserT)(decimal)(pure(applicativeParserT)(0))))))("");
  var nat = alt(altParserT)(zeroNumber)(decimal);
  var $$int = bind(bindParserT)(lexeme(sign2(ringInt)))(function(f) {
    return bind(bindParserT)(nat)(function(n) {
      return pure(applicativeParserT)(f(n));
    });
  });
  var integer2 = withErrorMessage(lexeme($$int))("integer");
  var natural = withErrorMessage(lexeme(nat))("natural");
  var comma = symbol(",");
  var commaSep = function(p) {
    return sepBy(p)(comma);
  };
  var commaSep1 = function(p) {
    return sepBy1(p)(comma);
  };
  var colon = symbol(":");
  var charNum = bind(bindParserT)(alt(altParserT)(decimal)(alt(altParserT)(applySecond(applyParserT)($$char("o"))(number2(8)(octDigit)))(applySecond(applyParserT)($$char("x"))(number2(16)(hexDigit)))))(function(code) {
    var $71 = code > 1114111;
    if ($71) {
      return fail("invalid escape sequence");
    }
    ;
    var v1 = fromCharCode3(code);
    if (v1 instanceof Just) {
      return pure(applicativeParserT)(v1.value0);
    }
    ;
    if (v1 instanceof Nothing) {
      return fail("invalid character code (should not happen)");
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 498, column 10 - line 500, column 67): " + [v1.constructor.name]);
  });
  var charLetter = satisfy(function(c) {
    return c !== "'" && (c !== "\\" && c > "");
  });
  var charEsc = function() {
    var parseEsc = function(v1) {
      return voidLeft(functorParserT)($$char(v1.value0))(v1.value1);
    };
    return choice(foldableArray)(map(functorArray)(parseEsc)(escMap));
  }();
  var charControl = bind(bindParserT)($$char("^"))(function() {
    return bind(bindParserT)(upper2)(function(code) {
      var v1 = fromCharCode3((toCharCode2(code) - toCharCode2("A") | 0) + 1 | 0);
      if (v1 instanceof Just) {
        return pure(applicativeParserT)(v1.value0);
      }
      ;
      if (v1 instanceof Nothing) {
        return fail("invalid character code (should not happen)");
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 488, column 5 - line 490, column 67): " + [v1.constructor.name]);
    });
  });
  var caseString = function(name2) {
    if (v.caseSensitive) {
      return voidLeft(functorParserT)(string(name2))(name2);
    }
    ;
    if (otherwise) {
      var msg = show(showString)(name2);
      var caseChar = function(c) {
        var v1 = function(v2) {
          if (otherwise) {
            return $$char(c);
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 355, column 1 - line 355, column 80): " + [c.constructor.name]);
        };
        var $82 = isAlpha(codePointFromChar(c));
        if ($82) {
          var $83 = toChar(toLowerSimple2(singleton6(c)));
          if ($83 instanceof Just) {
            var $84 = toChar(toUpperSimple2(singleton6(c)));
            if ($84 instanceof Just) {
              return alt(altParserT)($$char($83.value0))($$char($84.value0));
            }
            ;
            return v1(true);
          }
          ;
          return v1(true);
        }
        ;
        return v1(true);
      };
      var walk = function(name$prime) {
        var v1 = uncons3(name$prime);
        if (v1 instanceof Nothing) {
          return pure(applicativeParserT)(unit);
        }
        ;
        if (v1 instanceof Just) {
          return applySecond(applyParserT)(withErrorMessage(caseChar(v1.value0.head))(msg))(walk(v1.value0.tail));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 757, column 22 - line 759, column 72): " + [v1.constructor.name]);
      };
      return voidLeft(functorParserT)(walk(name2))(name2);
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 751, column 3 - line 751, column 50): " + [name2.constructor.name]);
  };
  var reserved2 = function(name2) {
    var go = applySecond(applyParserT)(caseString(name2))(withErrorMessage(notFollowedBy(v.identLetter))("end of " + name2));
    return lexeme($$try(go));
  };
  var brackets = function(p) {
    return between(symbol("["))(symbol("]"))(p);
  };
  var braces = function(p) {
    return between(symbol("{"))(symbol("}"))(p);
  };
  var ascii3codes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"];
  var ascii3 = ["\0", "", "", "", "", "", "", "\x07", "", "", "", "", "", "", "", "", "", "", "\x1B", "\x7F"];
  var ascii2codes = ["BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP"];
  var ascii2 = ["\b", "	", "\n", "\v", "\f", "\r", "", "", "", "", "", "", "", " "];
  var asciiMap = zip(append(semigroupArray)(ascii3codes)(ascii2codes))(append(semigroupArray)(ascii3)(ascii2));
  var charAscii = function() {
    var parseAscii = function(v1) {
      return $$try(voidLeft(functorParserT)(string(v1.value0))(v1.value1));
    };
    return choice(foldableArray)(map(functorArray)(parseAscii)(asciiMap));
  }();
  var escapeCode = alt(altParserT)(charEsc)(alt(altParserT)(charNum)(alt(altParserT)(charAscii)(withErrorMessage(charControl)("escape code"))));
  var charEscape = applySecond(applyParserT)($$char("\\"))(escapeCode);
  var characterChar = alt(altParserT)(charLetter)(withErrorMessage(charEscape)("literal character"));
  var charLiteral = function() {
    var go = between($$char("'"))(withErrorMessage($$char("'"))("end of character"))(characterChar);
    return withErrorMessage(lexeme(go))("character");
  }();
  var stringEscape = bind(bindParserT)($$char("\\"))(function() {
    return alt(altParserT)(voidLeft(functorParserT)(escapeGap)(Nothing.value))(alt(altParserT)(voidLeft(functorParserT)(escapeEmpty)(Nothing.value))(map(functorParserT)(Just.create)(escapeCode)));
  });
  var stringChar = alt(altParserT)(map(functorParserT)(Just.create)(stringLetter))(withErrorMessage(stringEscape)("string character"));
  var stringLiteral2 = function() {
    var folder = function(v1) {
      return function(chars) {
        if (v1 instanceof Nothing) {
          return chars;
        }
        ;
        if (v1 instanceof Just) {
          return new Cons(v1.value0, chars);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 455, column 5 - line 455, column 51): " + [v1.constructor.name, chars.constructor.name]);
      };
    };
    var go = bind(bindParserT)(between($$char('"'))(withErrorMessage($$char('"'))("end of string"))(many(alternativeParserT)(lazyParserT)(stringChar)))(function(maybeChars) {
      return pure(applicativeParserT)(fromCharArray(toUnfoldable(unfoldableArray)(foldr(foldableList)(folder)(Nil.value)(maybeChars))));
    });
    return lexeme(withErrorMessage(go)("literal string"));
  }();
  var angles = function(p) {
    return between(symbol("<"))(symbol(">"))(p);
  };
  return {
    identifier: identifier2,
    reserved: reserved2,
    operator,
    reservedOp: reservedOp2,
    charLiteral,
    stringLiteral: stringLiteral2,
    natural,
    integer: integer2,
    "float": $$float2,
    naturalOrFloat,
    decimal,
    hexadecimal: hexadecimal2,
    octal,
    symbol,
    lexeme,
    whiteSpace: whiteSpace$prime(v),
    parens,
    braces,
    angles,
    brackets,
    semi,
    comma,
    colon,
    dot,
    semiSep,
    semiSep1,
    commaSep,
    commaSep1
  };
};

// output/Parsing.Language/index.js
var emptyDef = /* @__PURE__ */ function() {
  var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
  return {
    commentStart: "",
    commentEnd: "",
    commentLine: "",
    nestedComments: true,
    identStart: alt(altParserT)(letter)($$char("_")),
    identLetter: alt(altParserT)(alphaNum)(oneOf2(["_", "'"])),
    opStart: op$prime,
    opLetter: op$prime,
    reservedOpNames: [],
    reservedNames: [],
    caseSensitive: true
  };
}();

// output/Parser/index.js
var tokenParser = /* @__PURE__ */ makeTokenParser(/* @__PURE__ */ function() {
  var v = unGenLanguageDef(emptyDef);
  return {
    commentStart: v.commentStart,
    commentEnd: v.commentEnd,
    commentLine: v.commentLine,
    nestedComments: v.nestedComments,
    identStart: v.identStart,
    identLetter: v.identLetter,
    opStart: v.opStart,
    opLetter: v.opLetter,
    reservedNames: [],
    reservedOpNames: ["=", '"', '"', "_", ";"],
    caseSensitive: v.caseSensitive
  };
}());
var whiteSpace = /* @__PURE__ */ function() {
  return tokenParser.whiteSpace;
}();
var switchFunction = function(n) {
  var s = toString(n);
  return pure(applicativeParserT)(Switch.create(s));
};
var stringLiteral = /* @__PURE__ */ function() {
  return tokenParser.stringLiteral;
}();
var showParseError = function(v) {
  return show(showInt)(v.value1.line) + (":" + (show(showInt)(v.value1.column) + (" " + v.value0)));
};
var reservedOp = /* @__PURE__ */ function() {
  return tokenParser.reservedOp;
}();
var reserved = /* @__PURE__ */ function() {
  return tokenParser.reserved;
}();
var onlySemiColon = /* @__PURE__ */ discard(discardUnit)(bindParserT)(/* @__PURE__ */ lookAhead(/* @__PURE__ */ reservedOp(";")))(function() {
  return pure(applicativeParserT)(EmptyStatement.value);
});
var onlyEOF = /* @__PURE__ */ discard(discardUnit)(bindParserT)(/* @__PURE__ */ lookAhead(eof))(function() {
  return pure(applicativeParserT)(EmptyStatement.value);
});
var noTranmission = /* @__PURE__ */ function() {
  return $$try(choice(foldableArray)([voidLeft(functorParserT)(reserved("turn of"))(EmptyStatement.value), voidLeft(functorParserT)(reserved("turn off"))(EmptyStatement.value), voidLeft(functorParserT)(reserved("turns off"))(EmptyStatement.value), voidLeft(functorParserT)(reserved("turnof"))(EmptyStatement.value), voidLeft(functorParserT)(reserved("apagar"))(EmptyStatement.value)]));
}();
var integer = /* @__PURE__ */ function() {
  return tokenParser.integer;
}();
var identifier = /* @__PURE__ */ function() {
  return tokenParser.identifier;
}();
var matchKeyword = function(options) {
  return bind(bindParserT)(identifier)(function(word) {
    var keyword = toLower(word);
    var $9 = elem(foldableList)(eqString)(keyword)(fromFoldable(foldableList)(options));
    if ($9) {
      return pure(applicativeParserT)(unit);
    }
    ;
    return empty(plusParserT);
  });
};
var onOrOff = /* @__PURE__ */ $$try(/* @__PURE__ */ choice(foldableArray)([/* @__PURE__ */ $$try(/* @__PURE__ */ voidLeft(functorParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["on", "onn", "onnn", "onnnn"])))(true)), /* @__PURE__ */ $$try(/* @__PURE__ */ voidLeft(functorParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["off", "of", "offf", "offff"])))(false))]));
var monitorFunction = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["mon", "monitor", "m\xF3nitor", "monnitor"])))(function() {
  return bind(bindParserT)(identifier)(function(s) {
    return pure(applicativeParserT)(Monitor.create("https://www.transmits.link/monitors/" + s));
  });
});
var functionWithStringKeyword = function(keywords) {
  return function(constructor) {
    return $$try(bind(bindParserT)(matchKeyword(keywords))(function() {
      return bind(bindParserT)(identifier)(function(s) {
        return pure(applicativeParserT)(constructor(s));
      });
    }));
  };
};
var $$float = /* @__PURE__ */ function() {
  return tokenParser["float"];
}();
var negativeNumber = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ pure(applicativeParserT)(unit))(function() {
  return discard(discardUnit)(bindParserT)(reservedOp("-"))(function() {
    return map(functorParserT)(mul(semiringNumber)(-1))($$float);
  });
});
var number = /* @__PURE__ */ choice(foldableArray)([/* @__PURE__ */ $$try(negativeNumber), /* @__PURE__ */ $$try($$float), /* @__PURE__ */ map(functorParserT)(toNumber)(integer)]);
var functionWithNumberKeyword = function(keywords) {
  return function(constructor) {
    return $$try(bind(bindParserT)(matchKeyword(keywords))(function() {
      return bind(bindParserT)(number)(function(n) {
        return pure(applicativeParserT)(constructor(n));
      });
    }));
  };
};
var scalarFunction = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["scalar", "escalar", "scale", "escale", "scail", "escail", "esqueil", "squeil", "biggealo", "bigealo"])))(function() {
  return bind(bindParserT)(number)(function(n) {
    return pure(applicativeParserT)(Scalar.create(n));
  });
});
var switchFunctionWrapper = /* @__PURE__ */ $$try(/* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["switch", "suitch", "suich", "suish", "swish", "c\xE1mbiale"])))(function() {
  return bind(bindParserT)(number)(function(n) {
    return switchFunction(n);
  });
}));
var vec2xy = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ pure(applicativeParserT)(unit))(function() {
  return bind(bindParserT)(number)(function(x) {
    return bind(bindParserT)(number)(function(y) {
      return pure(applicativeParserT)({
        x,
        y
      });
    });
  });
});
var functionWithV2Keyword = function(keywords) {
  return function(constructor) {
    return $$try(bind(bindParserT)(matchKeyword(keywords))(function() {
      return bind(bindParserT)(vec2xy)(function(v2) {
        return pure(applicativeParserT)(constructor(v2));
      });
    }));
  };
};
var vec3xyz = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ pure(applicativeParserT)(unit))(function() {
  return bind(bindParserT)(number)(function(x) {
    return bind(bindParserT)(number)(function(y) {
      return bind(bindParserT)(number)(function(z) {
        return pure(applicativeParserT)({
          x,
          y,
          z
        });
      });
    });
  });
});
var functionWithV3Keyword = function(keywords) {
  return function(constructor) {
    return $$try(bind(bindParserT)(matchKeyword(keywords))(function() {
      return bind(bindParserT)(vec3xyz)(function(v3) {
        return pure(applicativeParserT)(constructor(v3));
      });
    }));
  };
};
var dynNumberRight = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ pure(applicativeParserT)(unit))(function() {
  return bind(bindParserT)(number)(function(v) {
    return pure(applicativeParserT)(new Right(v));
  });
});
var dynNumberLeft = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["a", "auto", "automatic", "atomatic", "automatico", "autom\xE1tico"])))(function() {
  return bind(bindParserT)(number)(function(v) {
    return pure(applicativeParserT)(new Left(v));
  });
});
var dynNumber = /* @__PURE__ */ choice(foldableArray)([/* @__PURE__ */ $$try(dynNumberLeft), /* @__PURE__ */ $$try(dynNumberRight)]);
var dynVec3xyz = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ pure(applicativeParserT)(unit))(function() {
  return bind(bindParserT)(dynNumber)(function(x) {
    return bind(bindParserT)(dynNumber)(function(y) {
      return bind(bindParserT)(dynNumber)(function(z) {
        return pure(applicativeParserT)({
          x,
          y,
          z
        });
      });
    });
  });
});
var functionWithDynV3Keyword = function(keywords) {
  return function(constructor) {
    return $$try(bind(bindParserT)(matchKeyword(keywords))(function() {
      return bind(bindParserT)(dynVec3xyz)(function(v) {
        return pure(applicativeParserT)(constructor(v));
      });
    }));
  };
};
var transformations = function(isOn) {
  return choice(foldableArray)([functionWithNumberKeyword(fromFoldable(foldableArray)(["vol", "volume", "volumen", "vol\xFAmen", "subele", "s\xFAbele", "pumpealo"]))(Volume.create), functionWithV2Keyword(fromFoldable(foldableArray)(["repeat", "repet", "repit", "repitelo", "rep\xEDtelo", "repitealo"]))(ChannelRepeater.create), functionWithV3Keyword(fromFoldable(foldableArray)(["muv", "movet", "muvet", "muvit", "movit", "move it", "muevelo", "mu\xE9velo", "muvetelo"]))(Movet.create), functionWithDynV3Keyword(fromFoldable(foldableArray)(["rodar", "rotate", "rotait", "rotaetelo", "rotatelo"]))(Rodar.create), functionWithStringKeyword(fromFoldable(foldableArray)(["fulcober", "fullcober", "fulcover", "fullcover"]))(Fulcober.create), functionWithNumberKeyword(fromFoldable(foldableArray)(["translucido", "traslucido", "translusido", "translucent", "traslucent", "traslusent"]))(Translucidez.create), functionWithV3Keyword(fromFoldable(foldableArray)(["color", "colour", "color it", "colorealo", "colourealo"]))(Colour.create), functionWithV3Keyword(fromFoldable(foldableArray)(["emit", "emitir", "emitear", "emitealo"]))(EmissionColour.create), functionWithNumberKeyword(fromFoldable(foldableArray)(["brillo", "brightness", "braignes", "braigtnes", "briyo"]))(EmissionIntensity.create), function() {
    if (isOn) {
      return switchFunctionWrapper;
    }
    ;
    return empty(plusParserT);
  }(), $$try(monitorFunction), $$try(scalarFunction)]);
};
var transmissionParser = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ pure(applicativeParserT)(unit))(function() {
  return bind(bindParserT)(identifier)(function(word) {
    var keyword = toLower(word);
    return discard(discardUnit)(bindParserT)(function() {
      var $11 = elem(foldableList)(eqString)(keyword)(fromFoldable(foldableArray)(["trans", "transmission", "trasmission", "trasmision", "transmicion", "transmision", "transmisssion", "trasmisssion", "trasmicion", "transmissi\xF3n", "trasmissi\xF3n", "trasmisi\xF3n", "transmici\xF3n", "transmisi\xF3n", "transmisssi\xF3n", "trasmisssi\xF3n", "trasmici\xF3n"]));
      if ($11) {
        return pure(applicativeParserT)(unit);
      }
      ;
      return empty(plusParserT);
    }())(function() {
      return bind(bindParserT)(onOrOff)(function(b) {
        var t = new LiteralTransmissionAST(b);
        return bind(bindParserT)(many2(transformations(b)))(function(xs) {
          var xs$prime = foldl(foldableList)(compose(semigroupoidFn))(identity(categoryFn))(xs);
          return pure(applicativeParserT)(xs$prime(t));
        });
      });
    });
  });
});
var broadcaster = /* @__PURE__ */ bind(bindParserT)(/* @__PURE__ */ matchKeyword(/* @__PURE__ */ fromFoldable(foldableArray)(["broadcaster", "baseurl", "url"])))(function() {
  return bind(bindParserT)(alt(altParserT)(identifier)(stringLiteral))(function(s) {
    return pure(applicativeParserT)(new Broadcaster(s));
  });
});
var statement = /* @__PURE__ */ function() {
  return choice(foldableArray)([$$try(map(functorParserT)(TransmissionAST.create)(transmissionParser)), $$try(broadcaster), $$try(onlySemiColon), $$try(onlyEOF), $$try(noTranmission)]);
}();
var statements = /* @__PURE__ */ map(functorParserT)(/* @__PURE__ */ fromFoldable(foldableList))(/* @__PURE__ */ sepBy(statement)(/* @__PURE__ */ reservedOp(";")));
var astToProgram = function(xs) {
  var go = function($copy_v) {
    return function($copy_acc) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v2, acc, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return {
              baseURL: v2,
              transmissions: acc
            };
          }
          ;
          if (v1 instanceof Cons && v1.value0 instanceof Broadcaster) {
            $tco_var_v = v1.value0.value0;
            $tco_var_acc = acc;
            $copy_v1 = v1.value1;
            return;
          }
          ;
          if (v1 instanceof Cons && v1.value0 instanceof TransmissionAST) {
            var tr = tASTtoTWithBase(v2)(v1.value0.value0);
            $tco_var_v = v2;
            $tco_var_acc = new Cons(tr, acc);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = v2;
            $tco_var_acc = acc;
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Parser (line 69, column 5 - line 69, column 60): " + [v2.constructor.name, acc.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $tco_var_acc, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var v = go("https://jac307.github.io/MultimediaSamples/Video/main/")(Nil.value)(xs);
  return {
    transmissions: reverse(v.transmissions),
    baseURL: v.baseURL
  };
};
var ast = /* @__PURE__ */ discard(discardUnit)(bindParserT)(whiteSpace)(function() {
  return bind(bindParserT)(statements)(function(x) {
    return discard(discardUnit)(bindParserT)(eof)(function() {
      return pure(applicativeParserT)(x);
    });
  });
});
var parseAST = function(x) {
  var v = runParser(x)(ast);
  if (v instanceof Left) {
    return new Left(showParseError(v.value0));
  }
  ;
  if (v instanceof Right) {
    return new Right(v.value0);
  }
  ;
  throw new Error("Failed pattern match at Parser (line 46, column 14 - line 48, column 27): " + [v.constructor.name]);
};
var parseProgram = function(x) {
  return bind(bindEither)(parseAST(x))(function(ast1) {
    return pure(applicativeEither)(astToProgram(ast1));
  });
};

// output/RenderEngine/index.js
var replaceAt = function(i) {
  return function(v) {
    return function(a) {
      if (i >= length(a)) {
        return snoc(a)(v);
      }
      ;
      if (otherwise) {
        return fromMaybe(a)(updateAt(i)(v)(a));
      }
      ;
      throw new Error("Failed pattern match at RenderEngine (line 114, column 1 - line 114, column 52): " + [i.constructor.name, v.constructor.name, a.constructor.name]);
    };
  };
};
var playVideoElementsInMonitors = function(re) {
  return function __do3() {
    var ms = read(re.monitors)();
    return traverse_(applicativeEffect)(foldableList)(playVideoElement)(ms)();
  };
};
var newMonitor = function(re) {
  return function(i) {
    return function __do3() {
      var m = read(re.monitors)();
      var dm = defMonitor();
      var m$prime = replaceAt(i)(dm)(m);
      return write(m$prime)(re.monitors)();
    };
  };
};
var launch = function(cvs) {
  return function __do3() {
    log2(monadEffectEffect)("launch now with ineffective program")();
    var scene = newScene();
    var iWidth = windowInnerWidth();
    var iHeight = windowInnerHeight();
    var camera = newPerspectiveCamera(45)(iWidth / iHeight)(0.1)(100)();
    setPosition2()(camera)(0)(0)(15)();
    var renderer = newWebGLRenderer({
      antialias: true,
      canvas: cvs
    })();
    setSize(renderer)(iHeight)(iWidth)(false)();
    var lights = newHemisphereLight(16777215)(16777215)(3)();
    addAnythingToScene(scene)(lights)();
    var program = $$new({
      transmissions: Nil.value,
      baseURL: "https://jac307.github.io/TransMit/channels/"
    })();
    var monitors = $$new(Nil.value)();
    var re = {
      scene,
      camera,
      renderer,
      program,
      monitors
    };
    return re;
  };
};
var evaluate = function(re) {
  return function(s) {
    var v = parseProgram(s);
    if (v instanceof Right) {
      return function __do3() {
        log2(monadEffectEffect)("Parsed baseURL: " + v.value0.baseURL)();
        write(v.value0)(re.program)();
        return {
          error: Nothing.value
        };
      };
    }
    ;
    if (v instanceof Left) {
      return pure(applicativeEffect)({
        error: new Just(v.value0)
      });
    }
    ;
    throw new Error("Failed pattern match at RenderEngine (line 62, column 3 - line 67, column 41): " + [v.constructor.name]);
  };
};
var alignMonitors = function(re) {
  return function(program) {
    var tLen = length(program.transmissions);
    return function __do3() {
      var ms = read(re.monitors)();
      var mLen = length(ms);
      (function() {
        var v = mLen === tLen;
        if (v) {
          return unit;
        }
        ;
        if (!v) {
          var v1 = mLen > tLen;
          if (v1) {
            var keptMonitors = take(tLen)(ms);
            var droppedMonitors = drop(tLen)(ms);
            traverse_(applicativeEffect)(foldableList)(removeMonitor(re.scene))(droppedMonitors)();
            return write(keptMonitors)(re.monitors)();
          }
          ;
          if (!v1) {
            var howMany = tLen - mLen | 0;
            var indices = range2(mLen + 1 | 0)(mLen + howMany | 0);
            return traverse_(applicativeEffect)(foldableList)(newMonitor(re))(indices)();
          }
          ;
          throw new Error("Failed pattern match at RenderEngine (line 89, column 7 - line 98, column 44): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at RenderEngine (line 86, column 3 - line 98, column 44): " + [v.constructor.name]);
      })();
      var ms$prime = read(re.monitors)();
      zipWithA(applicativeEffect)(alignMonitor(re.scene))(ms$prime)(program.transmissions)();
      return unit;
    };
  };
};
var runProgram = function(re) {
  return function(p) {
    return function __do3() {
      alignMonitors(re)(p)();
      return playVideoElementsInMonitors(re)();
    };
  };
};
var animate = function(re) {
  return function __do3() {
    var p = read(re.program)();
    runProgram(re)(p)();
    return render(re.renderer)(re.scene)(re.camera)();
  };
};

// output/Main/index.js
var launch2 = launch;
var evaluate2 = function(re) {
  return function(s) {
    return function __do3() {
      var result = evaluate(re)(s)();
      if (result.error instanceof Just) {
        return {
          success: false,
          error: result.error.value0
        };
      }
      ;
      if (result.error instanceof Nothing) {
        return {
          success: true,
          error: ""
        };
      }
      ;
      throw new Error("Failed pattern match at Main (line 16, column 3 - line 18, column 50): " + [result.error.constructor.name]);
    };
  };
};
var animate2 = animate;
export {
  animate2 as animate,
  evaluate2 as evaluate,
  launch2 as launch
};
