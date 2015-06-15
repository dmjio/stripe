{ mkDerivation, aeson, base, bytestring, free, HsOpenSSL, hspec
, http-streams, io-streams, stdenv, stripe-core, stripe-tests, text
}:
mkDerivation {
  pname = "stripe-http-streams";
  version = "2.0.0";
  src = ./.;
  buildDepends = [
    aeson base bytestring HsOpenSSL http-streams io-streams stripe-core
    text
  ];
  testDepends = [
    base free HsOpenSSL hspec http-streams stripe-core stripe-tests
  ];
  license = stdenv.lib.licenses.mit;
}
