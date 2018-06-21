with (import <nixpkgs> {});
with haskell.lib;

rec {
  hsSrcSet = (lib.foldl' (s: p: s // (import p).hsSrcSet) {} [
    ../Lib-BiobaseTypes
  ]) // {BiobaseFasta = ./.;};
  hsPkgs = haskellPackages.extend (packageSourceOverrides hsSrcSet);
  hsShell = with hsPkgs; shellFor {
    packages = p: [ p.BiobaseFasta ];
    withHoogle = true;
    buildInputs = [
      cabal-install ghc
      BiobaseTypes
    ];
  };
}
