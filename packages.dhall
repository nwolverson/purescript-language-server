let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220706/packages.dhall
        sha256:7a24ebdbacb2bfa27b2fc6ce3da96f048093d64e54369965a2a7b5d9892b6031

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
    , repo =
        "https://github.com/rowtype-yoga/purescript-untagged-union.git"
    , version = "ed8262a966e15e751322c327e2759a9b9c0ef3f3"
    }

  with literals.repo = "https://github.com/ilyakooo0/purescript-literals.git"
  with literals.version = "6875fb28026595cfb780318305a77e79b098bb01"
  
  with psc-ide.version = "7c331b33cedebb636d72ab5fd325821e304b9833"