{ pkgs ? import <nixpkgs> {}
, ghc ? "9_2_1"
}:

let
  ghcs = import (pkgs.fetchgit {
    url = "https://gitlab.haskell.org/bgamari/ghcs-nix";
    rev = "f172edaa91a6afec5873e4ee38a6454a45486b2b";
    sha256 = "02kaalyk4xm0hnygb49cqpn6xdv2nsy5ghsghpvijsj8366rbw85";
  }) {};
in pkgs.mkShell { packages = [ ghcs."ghc-${ghc}" ghcs.cabal-install ]; }
    
