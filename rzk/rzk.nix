{ mkDerivation, aeson, alex, array, base, bifunctors, bytestring
, doctest, filepath, Glob, happy, hpack, lens, lib, lsp, lsp-types
, mtl, optparse-generic, QuickCheck, template-haskell, text, yaml
}:
mkDerivation {
  pname = "rzk";
  version = "0.5.7";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base bifunctors bytestring filepath Glob lens lsp
    lsp-types mtl optparse-generic template-haskell text yaml
  ];
  libraryToolDepends = [ alex happy hpack ];
  executableHaskellDepends = [
    aeson array base bifunctors bytestring filepath Glob lens lsp
    lsp-types mtl optparse-generic template-haskell text yaml
  ];
  executableToolDepends = [ alex happy ];
  testHaskellDepends = [
    aeson array base bifunctors bytestring doctest filepath Glob lens
    lsp lsp-types mtl optparse-generic QuickCheck template-haskell text
    yaml
  ];
  testToolDepends = [ alex happy ];
  prePatch = "hpack";
  homepage = "https://github.com/rzk-lang/rzk#readme";
  description = "An experimental proof assistant for synthetic ∞-categories";
  license = lib.licenses.bsd3;
}
