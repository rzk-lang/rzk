# Parser generators for `make -C rzk regen-parser`. Versions must match
# .github/workflows/parser-drift.yml and rzk/Makefile.
{ pkgs, ghcVersion }:
let
  inherit (pkgs.haskell.lib) overrideCabal;

  # happy 2.x split the library into happy-lib; the ghc963 package set only
  # has happy 1.x, so both must be pulled from Hackage.
  hpkgs = pkgs.haskell.packages.${ghcVersion}.extend (self: super: {
    happy-lib = overrideCabal
      (self.callHackageDirect {
        pkg = "happy-lib";
        ver = "2.2";
        sha256 = "1j83gcfi1w11p9yb87b543lmkbf3xajyfbid7y2mv0s75jsvqgym";
      } { })
      (_: { doCheck = false; });

    happy = (overrideCabal
      (self.callHackageDirect {
        pkg = "happy";
        ver = "2.2";
        sha256 = "11xfm7y0dxb676635xqcfgqr0syq9j3hy1157f3kxpb3ljsyg85a";
      } { })
      (_: { doCheck = false; }))
      .overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ super.happy ];
      });

    alex_3_5_4_0 = (overrideCabal
      (self.callHackageDirect {
        pkg = "alex";
        ver = "3.5.4.0";
        sha256 = "14aacrnl62fm0w71bx8vcsqhn91cs068bf2iz0nvccwr0s9w6p2l";
      } { })
      (_: { doCheck = false; }))
      .overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ super.alex ];
      });
  });

  hackage = hpkgs.callHackageDirect;
in
[
  (overrideCabal
    (hackage {
      pkg = "BNFC";
      ver = "2.9.6.3";
      sha256 = "1mjify1i5bbqvzlajxrrc7w9lnchrmgn6yj12j7n79v49q24c9ai";
    } { })
    (_: { doCheck = false; }))
  hpkgs.alex_3_5_4_0
  hpkgs.happy
]
