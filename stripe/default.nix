{ nixpkgs ? import <nixpkgs> {} }:
            nixpkgs.pkgs.haskellPackages.callPackage ./stripe.nix
           { }
