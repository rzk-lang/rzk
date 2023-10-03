{ inputs, pkgs, ghcVersion, rzk, rzk-src, tools, hlsPkgs }:
let
  inherit (pkgs.haskell.lib) overrideCabal;
  # https://nixos.wiki/wiki/Haskell#Overrides
  hls = hlsPkgs.haskell-language-server-96;
  hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
    overrides = final: prev: {
      lsp = final.callHackageDirect
        {
          pkg = "lsp";
          ver = "2.2.0.0";
          sha256 = "sha256-HcEfdYUrCHufEa+10M2wESjnK41xM/msd+t6r6JwQO0=";
        }
        { };
      lsp-types = final.callHackageDirect
        {
          pkg = "lsp-types";
          ver = "2.0.2.0";
          sha256 = "sha256-Oa5HuKdsdTSQUKtuSt06zVAq19Qxq5IJZObrnPwlB6s=";
        }
        { };

      ${rzk} = final.callCabal2nix rzk rzk-src { };
    };
  };

  devShells = {
    default =
      hpkgs.shellFor {
        shellHook = "export LANG=C.utf8";
        packages = ps: [ ps.rzk ];
        nativeBuildInputs = tools ++ [ hls ];
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
