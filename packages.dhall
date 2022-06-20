let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220527/packages.dhall
        sha256:15dd8041480502850e4043ea2977ed22d6ab3fc24d565211acde6f8c5152a799

in  upstream
  with purescript-threejs = ../purescript-threejs/spago.dhall as Location

{-
in  upstream
  with purescript-threejs =
    { dependencies = [ "prelude", "effect" ]
    , repo = "https://github.com/jac307/purescript-threejs"
    , version = "69c87f292f8062ab422172f990cbfbd2e6c0ecd8"
    }
-}
