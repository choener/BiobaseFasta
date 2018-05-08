with import <nixpkgs> {};
let
  pipes-group-override = haskellPackages.override {
    overrides = self: super: {
      # old doctest
      pipes-group = haskell.lib.dontCheck super.pipes-group;
    };
  };
  sourceOverrides = pipes-group-override.extend (haskell.lib.packageSourceOverrides {
    BiobaseTypes = ../Lib-BiobaseTypes;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  });
in
sourceOverrides

