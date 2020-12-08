{ mkDerivation, aeson, base, bytestring, mtl, stdenv, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "stripe-core";
  version = "2.6.2";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring mtl text time transformers
    unordered-containers
  ];
  homepage = "https://github.com/dmjio/stripe-haskell";
  description = "Stripe API for Haskell - Pure Core";
  license = stdenv.lib.licenses.mit;
}
