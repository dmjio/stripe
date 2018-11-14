let
  eval = import ./default.nix;
in rec {
     ghc843  = eval { compiler = "ghc843"; };
   }

