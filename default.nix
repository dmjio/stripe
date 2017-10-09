{ compiler ? "ghc802", profiling ? false, haddock ? false, pkgs ? import <nixpkgs> {} }:
let
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        doHaddock = haddock;
        enableLibraryProfiling = profiling;
      });
      stripe-core = pkgs.haskell.lib.dontCheck (self.callPackage ./stripe-core {});
      stripe-tests = pkgs.haskell.lib.dontCheck (self.callPackage ./stripe-tests {});
      stripe-http-streams = self.callPackage ./stripe-http-streams {};
      stripe-haskell = pkgs.haskell.lib.dontCheck (self.callPackage ./stripe-haskell {});
    };
  };
in with haskellPackages; {
  inherit stripe-core stripe-tests stripe-haskell stripe-http-streams;
}
