(import ./.).shellFor {
  packages = p: [ p.BiobaseFasta p.BiobaseTypes p.DPutils p.ForestStructures p.PrimitiveArray p.SciBaseTypes ];
  withHoogle = true;
}
