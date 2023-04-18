{ name = "TransMit"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "either"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "purescript-threejs"
  , "refs"
  , "strings"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
