{ mkDerivation, base, stdenv, stripe-core, stripe-http-streams }:
mkDerivation {
  pname = "stripe";
  version = "2.0.0";
  src = ./.;
  buildDepends = [ base stripe-core stripe-http-streams ];
  homepage = "https://github.com/dmjio/stripe";
  description = "Stripe API for Haskell";
  license = stdenv.lib.licenses.mit;
}
