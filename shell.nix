{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "cbcb53725c430de4e69f652d69c1677e17c6bcec";
      sha256 = "155f8vischacl8108ibgs51kj3r7yq1690y4yb4nnqmnjww41k9b";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs-0_15_2
    easy-ps.spago
    easy-ps.psa

  ];
}
