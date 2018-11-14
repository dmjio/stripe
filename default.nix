{ compiler ? "ghc843", check ? false }:
let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
	    ${compiler} = pkgs.haskell.packages.${compiler}.override {
	      overrides = self: super: with pkgs.haskell.lib; rec {
                stripe-core = self.callPackage ./stripe-core {};
                stripe-tests = self.callPackage ./stripe-tests { inherit stripe-core; };
                stripe-http-streams = dontCheck (self.callPackage ./stripe-http-streams {
                  inherit stripe-tests stripe-core;
                });
                stripe-http-client =
                  let
                    stripe-http-client = self.callPackage ./stripe-http-client {
                      inherit stripe-tests stripe-core;
                    };
                  in if check then stripe-http-client else dontCheck stripe-http-client;
                stripe-haskell = self.callPackage ./stripe-haskell {
                  inherit stripe-http-streams stripe-core stripe-http-client;
                };
              };
            };
          };
        };
      };
    };
in
  with (import <nixpkgs> { inherit config; }).haskell.packages.${compiler}; {
    inherit stripe-core stripe-tests stripe-haskell stripe-http-streams stripe-http-client;
  }

