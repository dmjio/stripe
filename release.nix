let
  eval = import ./default.nix;
in rec {
     ghc801  = eval { compiler = "ghc801"; };
     ghc802  = eval { compiler = "ghc802"; };
   }

