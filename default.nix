with import <nixpkgs> {};
let
  sourceOverrides = {
    BiobaseTypes = ../Lib-BiobaseTypes;
    DPutils = ../Lib-DPutils;
    ForestStructures = ../Lib-ForestStructures;
    PrimitiveArray = ../Lib-PrimitiveArray;
    SciBaseTypes = ../Lib-SciBaseTypes;
  };
  pipes-group-no-test = pipes-group.extend (self : super : {});
in
haskellPackages.extend (haskell.lib.packageSourceOverrides sourceOverrides)

