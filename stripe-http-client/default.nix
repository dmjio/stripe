{ mkDerivation, aeson, base, bytestring, free, hspec, http-client
, http-client-tls, http-types, stdenv, stripe-core, stripe-tests
, text
}:
mkDerivation {
  pname = "stripe-http-client";
  version = "2.4.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring http-client http-client-tls http-types
    stripe-core text
  ];
  preCheck = ''
   export STRIPEKEY="sk_test_igoYowTqR5IfovOKFKwigRmW"
  '';
  testHaskellDepends = [
    base free hspec http-client stripe-core stripe-tests
  ];
  description = "Stripe API for Haskell - http-client backend";
  license = stdenv.lib.licenses.mit;
}
