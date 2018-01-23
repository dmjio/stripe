{ compiler ? "ghc822" }:
let
   config = {
     packageOverrides = pkgs: {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
	    ${compiler} = pkgs.haskell.packages.${compiler}.override {
	      overrides = self: super: with pkgs.haskell.lib; rec {
                stripe-core = self.callPackage ./stripe-core {};
                stripe-tests = self.callPackage ./stripe-tests { inherit stripe-core; };
                stripe-http-streams = self.callPackage ./stripe-http-streams {
                  inherit stripe-tests stripe-core;
                };
                stripe-haskell = self.callPackage ./stripe-haskell {
                  inherit stripe-http-streams stripe-core;
                };
              };
            };
          };
        };
      };
    };
in
  with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler}; {
    inherit stripe-core stripe-tests stripe-haskell stripe-http-streams;
  }

