{ mkDerivation, base, containers, hashable, integer-gmp, stdenv
, unordered-containers
}:
mkDerivation {
  pname = "semirings";
  version = "0.4.2";
  sha256 = "b2748b4309b780e5a4473a31ad69bed2f04ddc5d03ef099501bb260d535ccc2d";
  revision = "1";
  editedCabalFile = "1wrkcfblq3j2688xg8f1ial05sijkssmdm2rv9sw6jfxiays60vq";
  libraryHaskellDepends = [
    base containers hashable integer-gmp unordered-containers
  ];
  homepage = "http://github.com/chessai/semirings";
  description = "two monoids as one, in holy haskimony";
  license = stdenv.lib.licenses.bsd3;
}
