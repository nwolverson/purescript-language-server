let upstream =
      https://raw.githubusercontent.com/working-group-purescript-es/package-sets/main/packages.dhall sha256:642a07499270b2a0425accb35a043b72297bcd957965c2b50f0b34b558c59bc4

in  upstream
 with psc-ide = ../purescript-psc-ide/spago.dhall as Location
