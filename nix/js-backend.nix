# TODO fix builds with JS backend

{ inputs, pkgs, rzk, rzk-js, rzk-src, rzk-js-src, ghcVersion, tools }:
let
  inherit (pkgs.haskell.lib) overrideCabal;
  overrideJS = {
    overrides = self: super: {
      jsaddle = super.callCabal2nix "jsaddle" "${inputs.jsaddle.outPath}/jsaddle" { };
      rzk = overrideCabal
        (super.callCabal2nix rzk rzk-src { })
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
        (super.callCabal2nix rzk-js rzk-js-src { inherit (self) rzk; })
        (x: {
          postInstall = (x.postInstall or "") + ''
            rm -r $out/bin/${rzk-js}.jsexe
          '';
        });
    };
  };
  hpkgs = pkgs.pkgsCross.ghcjs.buildPackages.haskell.packages.${ghcVersion}.override overrideJS;

  # Get all dependencies of local Haskell packages excluding these local packages
  # This approach is useful for cases when a local package A depends on a local package B
  # In this case, package B won't be built by Nix as a dependency of A
  getHaskellPackagesDeps = someHaskellPackages: let l = pkgs.lib.lists; in (l.subtractLists someHaskellPackages (l.concatLists (map (package: l.concatLists (__attrValues package.getCabalDeps)) someHaskellPackages)));
  # build a GHC with the dependencies of local Haskell packages
  ghcForPackages = hpkgs_: localHaskellPackageNames: hpkgs_.ghcWithPackages (ps: (getHaskellPackagesDeps (map (x: ps.${x}) localHaskellPackageNames) ++ [ ps.haskell-language-server ])); # Why provide HLS here - https://github.com/NixOS/nixpkgs/issues/225895#issuecomment-1509991742

  ghcWithPackages = ghcForPackages hpkgs [ rzk rzk-js ];

  ghc = pkgs.pkgsCross.ghcjs.buildPackages.haskell.compiler.${ghcVersion};

  packages = {
    # TODO fix doesn't build .js
    inherit (hpkgs) rzk rzk-js;
  };

  devShells = {
    # This devshell provides a GHC compiler with packages that are dependencies of `rzk` and `rzk-js`
    # It should be a GHC with a JS backend, but it's an ordinary GHC for some reason

    default = hpkgs.shellFor {
      packages = ps: [ ps.rzk ps.${rzk-js} ];
      nativeBuildInputs = tools;
    };

    # This devshell provides a GHC compiler with packages that are dependencies of `rzk` and `rzk-js`
    # It should be a GHC with a JS backend, but it's an ordinary GHC for some reason

    ghcWithPackages = pkgs.mkShell {
      buildInputs = [ ghcWithPackages ] ++ tools;
    };

    # This devshell provides a GHC compiler with JS backend and without extra packages

    # GHC fails to build `rzk-js`
    # cabal --with-compiler=javascript-unknown-ghcjs-ghc --with-ghc-pkg=javascript-unknown-ghcjs-ghc-pkg build rzk-js
    ghcJS = pkgs.mkShell {
      buildInputs = [ ghc ] ++ tools;
    };


    # TODO incremental builds for ghc with js backend
    # Incremental builds work for ghc 9.6+
    # https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/haskell.section.md#incremental-builds-haskell-incremental-builds
  };
in
{
  inherit hpkgs devShells packages ghc ghcForPackages;
}
