{ mkDerivation, aeson, base, bytestring, HsOpenSSL, http-streams
, io-streams, stdenv, stripe-core, text
}:
mkDerivation {
  pname = "stripe-http-streams";
  version = "2.0.2";
  src = ./.;
  buildDepends = [
    aeson base bytestring HsOpenSSL http-streams io-streams stripe-core
    text
  ];
  description = "Stripe API for Haskell - http-streams backend";
  license = stdenv.lib.licenses.mit;
}
