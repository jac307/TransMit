{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "maybe"
  , "parsing"
  , "prelude"
  , "numbers"
  , "parsing"
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
