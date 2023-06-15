{ mkDerivation, array, base, bifunctors, hpack, lib, mtl
, optparse-generic, template-haskell
}:
mkDerivation {
  pname = "rzk";
  version = "0.4.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors mtl optparse-generic template-haskell
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    array base bifunctors mtl optparse-generic template-haskell
  ];
  testHaskellDepends = [
    array base bifunctors mtl optparse-generic template-haskell
  ];
  prePatch = "hpack";
  homepage = "https://github.com/fizruk/rzk#readme";
  description = "An experimental proof assistant for synthetic ∞-categories";
  license = lib.licenses.bsd3;
}
