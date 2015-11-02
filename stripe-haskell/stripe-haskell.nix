{ mkDerivation, base, stdenv, stripe-core, stripe-http-streams }:
mkDerivation {
  pname = "stripe-haskell";
  version = "2.0.1";
  src = ./.;
  buildDepends = [ base stripe-core stripe-http-streams ];
  homepage = "https://github.com/dmjio/stripe";
  description = "Stripe API for Haskell";
  license = stdenv.lib.licenses.mit;
}
