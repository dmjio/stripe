{ mkDerivation, aeson, base, bytestring, free, hspec, hspec-core
, mtl, random, stdenv, stripe-core, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "stripe-tests";
  version = "2.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring free hspec hspec-core mtl random stripe-core
    text time transformers unordered-containers
  ];
  homepage = "https://github.com/dmjio/stripe-haskell";
  description = "Tests for Stripe API bindings for Haskell";
  license = stdenv.lib.licenses.mit;
}
