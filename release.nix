let
  eval = import ./default.nix;
in rec {
     ghc784  = eval { compiler = "ghc784"; };
     ghc7102 = eval { compiler = "ghc7102"; };
     ghc7103 = eval { compiler = "ghc7103"; };
     ghc801  = eval { compiler = "ghc801"; };
     ghc802  = eval { compiler = "ghc802"; };
   }

