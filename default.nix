{ compiler ? "ghc802" }:
  let
   config = {
     packageOverrides = pkgs: {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
           ${compiler} = pkgs.haskell.packages.${compiler}.override {
              overrides = self: super: rec {
                mkDerivation = args: super.mkDerivation (args // {
                  # Only run tests on stripe-http-streams
 		  doCheck = args.pname == "stripe-http-streams";
		  doHaddock = false;
		});
                stripe-core = self.callPackage ./stripe-core { };
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
   pkgs = import <nixpkgs> { inherit config; };
   lib = pkgs.haskell.lib;
   in with pkgs.haskell.packages.${compiler}; {
     inherit stripe-core stripe-tests stripe-haskell stripe-http-streams;
   }

