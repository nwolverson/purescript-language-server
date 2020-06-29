{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-language-server"
, dependencies =
    [ "aff-promise"
    , "console"
    , "debug"
    , "effect"
    , "errors"
    , "foreign-generic"
    , "node-child-process"
    , "node-fs-aff"
    , "node-process"
    , "psc-ide"
    , "psci-support"
    , "stringutils"
    , "test-unit"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
