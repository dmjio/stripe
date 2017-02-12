{ pkgs ? import <nixpkgs> {} }:
let
  lib = pkgs.haskell.lib;
  stripePkgs = import ./default.nix {};
in pkgs.buildEnv {
    name = "hackage-upload";
    paths = map lib.sdistTarball (pkgs.lib.attrValues stripePkgs);
  }
