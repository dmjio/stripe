{ nixpkgs ? import <nixpkgs> {} }:
            nixpkgs.pkgs.haskellPackages.callPackage ./stripe-core.nix
           { }
