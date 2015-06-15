{ nixpkgs ? import <nixpkgs> {} }:
           (import ./default.nix { inherit nixpkgs; }).env
