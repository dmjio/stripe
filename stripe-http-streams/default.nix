{ mkDerivation, aeson, base, bytestring, free, HsOpenSSL, hspec
, http-streams, io-streams, stdenv, stripe-core, stripe-tests, text
}:
mkDerivation {
  pname = "stripe-http-streams";
  version = "2.2.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring HsOpenSSL http-streams io-streams stripe-core
    text
  ];
  testHaskellDepends = [
    base free HsOpenSSL hspec http-streams stripe-core stripe-tests
  ];
  preCheck = ''
   export STRIPEKEY="sk_test_igoYowTqR5IfovOKFKwigRmW"
  '';
  description = "Stripe API for Haskell - http-streams backend";
  license = stdenv.lib.licenses.mit;
}
