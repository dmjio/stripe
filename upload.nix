{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskell.lib;
let
  stripePkgs = import ./default.nix {};
in pkgs.buildEnv {
    name = "hackage-upload";
    paths = map sdistTarball (pkgs.lib.attrValues stripePkgs);
  }
