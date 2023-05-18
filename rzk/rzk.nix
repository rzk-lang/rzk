{ mkDerivation, array, base, bifunctors, hpack, lib, mtl
, template-haskell, alex, happy
}:
mkDerivation {
  pname = "rzk";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors mtl template-haskell alex happy
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    array base bifunctors mtl template-haskell
  ];
  testHaskellDepends = [
    array base bifunctors mtl template-haskell
  ];
  prePatch = "hpack";
  homepage = "https://github.com/fizruk/rzk#readme";
  license = lib.licenses.bsd3;
  doHaddock = false;
}
