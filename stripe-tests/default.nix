{ nixpkgs ? import <nixpkgs> {} }:
            nixpkgs.pkgs.haskellPackages.callPackage ./stripe-tests.nix
           { }
