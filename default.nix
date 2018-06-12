with (import <nixpkgs> {});
with haskell.lib;

rec {
  ##currently not needed?
  #packageOverrides = haskellPackages.override {
  #  overrides = self: super: {
  #    # old doctest
  #    pipes-group = haskell.lib.dontCheck super.pipes-group;
  #  };
  #};
  hsPkgs = haskellPackages.extend (packageSourceOverrides {
    bimaps = ../Lib-bimaps;
    BiobaseFasta = ./.;
    #BiobaseBlast = ./.;
    #BiobaseENA = ../Lib-BiobaseENA;
    BiobaseTypes = ../Lib-BiobaseTypes;
    #BiobaseXNA = ../Lib-BiobaseXNA;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    OrderedBits = ../Lib-OrderedBits;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  });
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseFasta ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      bimaps
      BiobaseTypes
      DPutils
      ForestStructures
      OrderedBits
      PrimitiveArray
      SciBaseTypes
    ];
  };
}
