{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/2436aaf8fad634ee66a6280fb82a19c1771c173f";
    # A recent nixpkgs, used for everything the old pin above (kept only for the
    # GHCJS/miso toolchain) is too old for: Node.js, because the old pin tops out
    # at Node 20.11 while Vite 7 — used by the playground build — needs Node
    # >= 20.19 / 22.12; and the native Haskell package set, because free-foil
    # needs GHC >= 9.8, whose set the old pin does not maintain.
    nixpkgs-recent.url = "github:NixOS/nixpkgs/nixos-25.05";
    miso = {
      url = "github:dmjio/miso/8277ac79941825abaf50b917e074e3df7ef6d213";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-filter.url = "github:numtide/nix-filter";
    jsaddle = {
      url = "github:ghcjs/jsaddle";
      flake = false;
    };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      pkgs-recent = inputs.nixpkgs-recent.legacyPackages.${system};
      nodejs = pkgs-recent.nodejs_22;

      rzk = "rzk";
      rzk-js = "rzk-js";
      # The native build needs GHC >= 9.8: free-foil requires
      # template-haskell-2.21, which first ships with GHC 9.8. That is the
      # default package set of the recent nixpkgs, so it is a maintained (and
      # cached) one. The GHCJS cross set stays on GHC 9.6.3, the version miso's
      # toolchain is built against.
      ghcVersion = "ghc984";
      ghcVersionJS = "ghc963";
      rzk-src = (inputs.nix-filter {
        root = ./${rzk};
        # grammar/, README.md and ChangeLog.md are listed in package.yaml as
        # extra source and doc files; a recent Cabal refuses to build a package
        # whose wildcards match nothing, so they have to be in the filtered
        # source tree even though nothing compiles them.
        include = [ "app" "src" "test" "grammar" "package.yaml" "README.md" "ChangeLog.md" ];
      });
      rzk-js-src = (inputs.nix-filter {
        root = ./${rzk-js};
        include = [ "Main.hs" "${rzk-js}.cabal" ];
      });

      tools = [
        pkgs.cabal-install
        pkgs.hpack
        nodejs
        pkgs.bun
      ];

      default = import ./nix/default.nix ({ inherit inputs rzk rzk-src ghcVersion tools; pkgs = pkgs-recent; });
      ghcjs = import ./nix/ghcjs.nix ({ inherit inputs pkgs scripts rzk rzk-src rzk-js rzk-js-src tools; ghcVersion = ghcVersionJS; });
      scripts = import ./nix/scripts.nix { inherit pkgs packages inputs nodejs; };


      packages = {
        default = default.packages.default;
        rzk = default.packages.${rzk};
        rzk-ghcjs = ghcjs.packages.${rzk};
        rzk-js = ghcjs.packages.${rzk-js};
      } // scripts;

      devShells = {
        default = default.devShells.default;
        ghcjs = ghcjs.devShells.default;
        release = pkgs.mkShell {
          buildInputs = [ scripts.release-rzk-playground ];
        };
      };
    in
    {
      inherit packages devShells default ghcjs;
    });

  nixConfig = {
    extra-substituters = [
      "https://miso-haskell.cachix.org"
      "https://nix-community.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "miso-haskell.cachix.org-1:6N2DooyFlZOHUfJtAx1Q09H0P5XXYzoxxQYiwn6W1e8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
