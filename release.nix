{ compiler ? "ghc843" }:

let
  pkgs = import <nixpkgs> {};
  dontCheck = pkgs.haskell.lib.dontCheck;
  doJailbreak = pkgs.haskell.lib.doJailbreak;
  haskellPkgs = pkgs.haskell.packages."${compiler}".extend (self: super: {
    try-elm = self.callPackage ./try-elm.nix {};
  });
in
  {
    try-elm = haskellPkgs.try-elm;
  }
