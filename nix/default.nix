{ inputs, pkgs, ghcVersion, rzk, rzk-src, tools }:
let
  inherit (pkgs.haskell.lib) overrideCabal;
  # https://nixos.wiki/wiki/Haskell#Overrides
  hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
    overrides = final: prev: {
      ${rzk} = final.callCabal2nix rzk rzk-src { };
    };
  };

  devShells = {
    default =
      hpkgs.shellFor {
        shellHook = "export LANG=C.utf8";
        packages = ps: [ ps.rzk ];
        nativeBuildInputs = tools ++ [ hpkgs.haskell-language-server ];
      };
  };

  packages = {
    default = pkgs.haskell.lib.justStaticExecutables hpkgs.${rzk};
    rzk = hpkgs.${rzk};
  };
in
{
  inherit hpkgs devShells packages;
}
