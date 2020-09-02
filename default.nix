{ compiler ? "ghc884", check ? false, nixpkgs ? <nixpkgs> }:
let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
      	    ${compiler} = pkgs.haskell.packages.${compiler}.override {
      	      overrides = self: super: with pkgs.haskell.lib; rec {
                stripe-core = disableOptimization (self.callCabal2nix "stripe-core" ./stripe-core {});
                stripe-tests = disableOptimization (self.callCabal2nix "stripe-tests" ./stripe-tests {});
                stripe-http-client =
                  let
                    pkg = disableOptimization (self.callCabal2nix "stripe-http-client" ./stripe-http-client {});
                  in if check
                     then pkg
                     else dontCheck pkg;
                stripe-haskell = disableOptimization (self.callCabal2nix "stripe-haskell" ./stripe-haskell {});
              };
            };
          };
        };
      };
    };
in
  with (import nixpkgs { inherit config; }).haskell.packages.${compiler}; {
    inherit stripe-core stripe-tests stripe-haskell stripe-http-client;
  }

