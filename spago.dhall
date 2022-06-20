{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
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
