with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.3.tar.gz";
  /* sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg"; */
}) {});
let rzk-local = pkgs.haskell.packages.ghcjs.callPackage ../rzk/rzk.nix { };
in pkgs.haskell.packages.ghcjs.callCabal2nix "try-rzk" ./. { rzk = rzk-local; }
