let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220531/packages.dhall sha256:278d3608439187e51136251ebf12fabda62d41ceb4bec9769312a08b56f853e3

in  upstream

with 
      uuid = 
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
      , repo = "https://github.com/working-group-purescript-es/purescript-foreign-generic.git"
      , version = "53410dd57e9b350d6c233f48f7aa46317c4faa21"
      }
