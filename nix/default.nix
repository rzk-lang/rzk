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

  parserToolsPkgs = pkgs.haskell.packages.${ghcVersion}.override {
    overrides = final: prev:
      let dc = pkgs.haskell.lib.dontCheck;
      in {
        happy-lib = dc (final.callHackageDirect
          { pkg = "happy-lib"; ver = "2.2"; sha256 = "sha256-1T+8tSxHg12FPy0u56Xqw61Z6SBlHbR8uiHwEB17A8k="; } { });
        happy = dc (final.callHackageDirect
          { pkg = "happy"; ver = "2.2"; sha256 = "sha256-qqDntaRj3T6HOyUED4dM2GuQ8XMM9zKMOWb1Bvyproc="; } { });
        alex = dc (final.callHackageDirect
          { pkg = "alex"; ver = "3.5.4.0"; sha256 = "sha256-VFzDkwaZM7Yt+FG4hQzQLCQLsWYb9RUOB9UJQ21mSpE="; } { });
        BNFC = dc (final.callHackageDirect
          { pkg = "BNFC"; ver = "2.9.6.3"; sha256 = "sha256-USVGBE5kp2OPFEF6Y1/NkFma+GE5d6no33itEoN3UdY="; } { });
      };
  };
  parserTools = [ parserToolsPkgs.BNFC parserToolsPkgs.alex parserToolsPkgs.happy ];

  devShells = {
    default =
      hpkgs.shellFor {
        shellHook = "export LANG=C.utf8";
        packages = ps: [ ps.rzk ];
        nativeBuildInputs = tools ++ parserTools ++ [ hpkgs.haskell-language-server ];
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
