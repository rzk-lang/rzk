{ mkDerivation, aeson, alex, array, base, bifunctors, bytestring
, doctest, filepath, Glob, happy, hpack, lens, lib, mtl
, optparse-generic, QuickCheck, stm, template-haskell, text, yaml
}:
mkDerivation {
  pname = "rzk";
  version = "0.6.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bifunctors bytestring filepath Glob lens mtl
    optparse-generic stm template-haskell text yaml
  ];
  libraryToolDepends = [ alex happy hpack ];
  executableHaskellDepends = [
    aeson array base bifunctors bytestring filepath Glob lens mtl
    optparse-generic stm template-haskell text yaml
  ];
  executableToolDepends = [ alex happy ];
  testHaskellDepends = [
    aeson array base bifunctors bytestring doctest filepath Glob lens
    mtl optparse-generic QuickCheck stm template-haskell text yaml
  ];
  testToolDepends = [ alex happy ];
  prePatch = "hpack";
  homepage = "https://github.com/rzk-lang/rzk#readme";
  description = "An experimental proof assistant for synthetic ∞-categories";
  license = lib.licenses.bsd3;
}
