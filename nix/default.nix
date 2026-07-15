{ inputs, pkgs, ghcVersion, rzk, rzk-src, tools }:
let
  inherit (pkgs.haskell.lib) overrideCabal;
  # https://nixos.wiki/wiki/Haskell#Overrides
  hpkgs = pkgs.haskell.packages.${ghcVersion}.override {
    overrides = final: prev: {
      # free-foil is not in nixpkgs, so it is taken from Hackage at the version
      # the Stack resolver uses. Bump this together with stack.yaml.
      free-foil = final.callHackageDirect
        {
          pkg = "free-foil";
          ver = "0.3.2";
          sha256 = "sha256-B9JccfYo9jSjJ3sPt2fTRM463Qg7qvt7czDU5Vc+utA=";
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
