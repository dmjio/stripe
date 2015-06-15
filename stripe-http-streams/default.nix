{ nixpkgs ? import <nixpkgs> {} }:
            nixpkgs.pkgs.haskellPackages.callPackage ./stripe-http-streams.nix
           { }
