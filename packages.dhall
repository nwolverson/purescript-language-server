let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.14-20240227/packages.dhall sha256:c9633eb78193aac138d7debbc907bfedb8f2e3025ef5a874b8dbc1f35b75eef4

in  upstream
  with uuid =
    { dependencies = [ "effect", "maybe", "foreign-generic", "console", "spec" ]
    , repo = "https://github.com/megamaddu/purescript-uuid.git"
    , version = "7bb5a90c9b11f6a33ac7610608a650e4d58aeac9"
    }
  with foreign-generic =
    { dependencies =
      [ "effect"
      , "exceptions"
      , "foreign"
      , "foreign-object"
      , "identity"
      , "ordered-collections"
      , "record"
      ]
    , repo =
        "https://github.com/working-group-purescript-es/purescript-foreign-generic.git"
    , version = "53410dd57e9b350d6c233f48f7aa46317c4faa21"
    }
  with untagged-union =
    { dependencies =
      [ "assert"
      , "console"
      , "effect"
      , "either"
      , "foreign"
      , "foreign-object"
      , "literals"
      , "maybe"
      , "newtype"
      , "prelude"
      , "tuples"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/rowtype-yoga/purescript-untagged-union.git"
    , version = "ed8262a966e15e751322c327e2759a9b9c0ef3f3"
    }
  with literals.repo = "https://github.com/ilyakooo0/purescript-literals.git"
  with literals.version = "6875fb28026595cfb780318305a77e79b098bb01"
  with psc-ide =
    { dependencies =
      [ "aff"
      , "argonaut"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "control"
      , "datetime"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "foreign-object"
      , "integers"
      , "maybe"
      , "node-buffer"
      , "node-child-process"
      , "node-fs"
      , "node-path"
      , "nullable"
      , "parallel"
      , "prelude"
      , "random"
      , "strings"
      ]
    , repo = "https://github.com/kritzcreek/purescript-psc-ide"
    , version = "5cc2cd48d067f72a760b970080d0ef0a4b427fdf"
    }
  with language-cst-parser =
    { dependencies =
      [ "arrays"
      , "console"
      , "const"
      , "debug"
      , "effect"
      , "either"
      , "filterable"
      , "foldable-traversable"
      , "free"
      , "functors"
      , "maybe"
      , "numbers"
      , "psci-support"
      , "strings"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      ]
    , repo = "https://github.com/natefaubion/purescript-language-cst-parser.git"
    , version = "v0.12.1"
    }
