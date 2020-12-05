{ mkDerivation, base, stdenv, stripe-core, stripe-http-client }:
mkDerivation {
  pname = "stripe-haskell";
  version = "2.6.2";
  src = ./.;
  libraryHaskellDepends = [ base stripe-core stripe-http-client ];
  homepage = "https://github.com/dmjio/stripe";
  description = "Stripe API for Haskell";
  license = stdenv.lib.licenses.mit;
}
