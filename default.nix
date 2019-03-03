{ mkDerivation, base, BiobaseTypes, bytestring, filepath, lens
, QuickCheck, resourcet, stdenv, streaming, streaming-bytestring
, tasty, tasty-golden, tasty-hunit, tasty-quickcheck, tasty-silver
, tasty-th, text
}:
mkDerivation {
  pname = "BiobaseFasta";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base BiobaseTypes bytestring lens resourcet streaming
    streaming-bytestring
  ];
  testHaskellDepends = [
    base bytestring filepath QuickCheck resourcet streaming
    streaming-bytestring tasty tasty-golden tasty-hunit
    tasty-quickcheck tasty-silver tasty-th text
  ];
  homepage = "https://github.com/choener/BiobaseFasta";
  description = "streaming FASTA parser";
  license = stdenv.lib.licenses.gpl3;
}
