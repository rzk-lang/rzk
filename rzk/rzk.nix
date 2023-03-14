{ mkDerivation, ansi-terminal, base, bifunctors, bound, Cabal
, cabal-doctest, deepseq, doctest, free, Glob, hashable, hpack
, kan-extensions, lib, logict, mtl, parsers, prettyprinter
, prettyprinter-ansi-terminal, profunctors, QuickCheck
, template-haskell, text, transformers, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "rzk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  doHaddock = false;
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    ansi-terminal base bifunctors bound deepseq free hashable
    kan-extensions logict mtl parsers prettyprinter
    prettyprinter-ansi-terminal profunctors text transformers trifecta
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    ansi-terminal base bifunctors bound deepseq free hashable
    kan-extensions logict mtl parsers prettyprinter
    prettyprinter-ansi-terminal profunctors text transformers trifecta
    unordered-containers
  ];
  testHaskellDepends = [
    ansi-terminal base bifunctors bound deepseq doctest free Glob
    hashable kan-extensions logict mtl parsers prettyprinter
    prettyprinter-ansi-terminal profunctors QuickCheck template-haskell
    text transformers trifecta unordered-containers
  ];
  prePatch = "hpack";
  homepage = "https://github.com/fizruk/rzk#readme";
  license = lib.licenses.bsd3;
}
