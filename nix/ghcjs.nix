{ inputs, pkgs, scripts, rzk, rzk-js, rzk-src, rzk-js-src, ghcVersion, tools }:
let
  inherit (pkgs.haskell.lib) overrideCabal;
  misoNix = (import "${inputs.miso}/default.nix" { inherit (pkgs) system; });
  pkgsMiso = misoNix.pkgs;

  hpkgs =
    pkgsMiso.haskell.packages.ghcjs.override {
      overrides = final: prev: {
        mkDerivation = args: prev.mkDerivation (args // {
          doCheck = false;
          doHaddock = false;
        });
        hpack = pkgs.hpack;
        rzk = (
          (pkgsMiso.haskell.lib.overrideCabal
            (prev.callCabal2nix rzk rzk-src { })
            (x: {
              isLibrary = true;
              isExecutable = false;
              buildTarget = "lib:rzk";
              libraryToolDepends = [ pkgs.hpack pkgs.alex pkgs.happy ] ++ (x.libraryToolDepends or [ ]);
              testToolDepends = [ pkgs.alex pkgs.happy ] ++ (x.testToolDepends or [ ]);
            }))
        ).overrideAttrs (x: {
          installPhase = builtins.replaceStrings [ "Setup copy" ] [ "Setup copy lib:rzk" ] x.installPhase;
        });
        rzk-js = overrideCabal
          (final.callCabal2nix rzk-js rzk-js-src { })
          (x: {
            postInstall = (x.postInstall or "") + ''
              cp $out/bin/${rzk-js} .
              rm -r $out
              cp ${rzk-js} $out
            '';
          });
      };
    };

  hpkgsGHCJS_8_10_7 = pkgs.haskell.packages.ghcjs810.override ({
    overrides = final: prev: {
      integer-gmp = final.integer-gmp_1_1;
      rzk = overrideCabal
        (final.callCabal2nix rzk rzk-src { })
        (x: {
          isLibrary = true;
          isExecutable = false;
          doCheck = false;
          doHaddock = false;
          libraryToolDepends = [ pkgs.hpack pkgs.alex pkgs.happy ] ++ (x.libraryToolDepends or [ ]);
          testToolDepends = [ pkgs.hpack pkgs.alex pkgs.happy ] ++ (x.testToolDepends or [ ]);
          prePatch = "hpack --force";
        });
      rzk-js = overrideCabal
        (final.callCabal2nix rzk-js rzk-js-src { inherit (final) rzk; })
        (x: {
          postInstall = (x.postInstall or "") + ''
            rm -r $out/bin/${rzk-js}.jsexe
          '';
        });
    };
  });

  packages = {
    inherit (hpkgs) rzk rzk-js;

    # Currently not buildable
    rzk_8_10_7 = hpkgsGHCJS_8_10_7.rzk;
    rzk-js_8_10_7 = hpkgsGHCJS_8_10_7;
  };

  devShells = {
    default = (hpkgs.shellFor {
      packages = _: [ hpkgs.${rzk} hpkgs.${rzk-js} ];
      nativeBuildInputs = tools ++ [ scripts.build-rzk-js ];
    }).overrideAttrs (x: {
      buildInputs = builtins.filter (p: (p.name or "") != "ghc-8.6.4") (x.buildInputs or [ ]);
    });
  };
in
{
  inherit hpkgs devShells packages hpkgsGHCJS_8_10_7;
}
