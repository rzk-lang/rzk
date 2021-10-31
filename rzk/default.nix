with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/fb48f5eee4189a22a314920c3d41f12835171d5d.tar.gz";
  /* sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg"; */
}) {});
let indexed-traversable-src = pkgs.fetchFromGitHub {
      owner = "haskellari";
      repo = "indexed-traversable";
      rev = "master";
      sha256 = "0c4ynm16qma6a4f2qgi07ffm2hiw4j46b01rh0sllm21b6riczij";
    } + "/indexed-traversable";
    indexed-traversable-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "indexed-traversable" indexed-traversable-src {};

    prettyprinter-src = (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/prettyprinter-1.7.1/prettyprinter-1.7.1.tar.gz";
    });
    prettyprinter-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "prettyprinter" prettyprinter-src {} ;
    prettyprinter-ansi-terminal-src = (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/prettyprinter-ansi-terminal-1.1.3/prettyprinter-ansi-terminal-1.1.3.tar.gz";
    });
    prettyprinter-ansi-terminal-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "prettyprinter-ansi-terminal" prettyprinter-ansi-terminal-src { prettyprinter = prettyprinter-latest; } ;

    trifecta-src = pkgs.fetchFromGitHub {
      owner = "ekmett";
      repo = "trifecta";
      rev = "v2.1.1";
      sha256 = "1kzvhhybw5vbm5dvg13z3g2i24prnf18zk85civ7i0b5kbcsrysr";
    };
    trifecta-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "trifecta" trifecta-src {
      indexed-traversable = indexed-traversable-latest;
      prettyprinter = prettyprinter-latest;
      prettyprinter-ansi-terminal = prettyprinter-ansi-terminal-latest;
    };

in pkgs.haskell.packages.ghcjs.callPackage ./rzk.nix {
  trifecta = trifecta-latest;
  prettyprinter = prettyprinter-latest;
  prettyprinter-ansi-terminal = prettyprinter-ansi-terminal-latest;
}
