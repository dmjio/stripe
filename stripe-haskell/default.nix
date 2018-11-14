{ mkDerivation, base, stdenv, stripe-core, stripe-http-streams, stripe-http-client }:
mkDerivation {
  pname = "stripe-haskell";
  version = "2.4.1";
  src = ./.;
  libraryHaskellDepends = [ base stripe-core stripe-http-streams stripe-http-client ];
  homepage = "https://github.com/dmjio/stripe";
  description = "Stripe API for Haskell";
  license = stdenv.lib.licenses.mit;
}
