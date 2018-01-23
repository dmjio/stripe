let
  eval = import ./default.nix;
in rec {
     ghc802  = eval { compiler = "ghc802"; };
     ghc822  = eval { compiler = "ghc822"; };
   }

