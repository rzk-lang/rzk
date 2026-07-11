{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/2436aaf8fad634ee66a6280fb82a19c1771c173f";
    # A recent nixpkgs used only for Node.js: the main pin above (kept for the
    # GHCJS/miso toolchain) tops out at Node 20.11, but Vite 7 — used by the
    # playground build — needs Node >= 20.19 / 22.12.
    nixpkgs-node.url = "github:NixOS/nixpkgs/nixos-25.05";
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
      # Node.js from a recent nixpkgs (see the nixpkgs-node input), new enough
      # for Vite 7; everything else still comes from the pinned nixpkgs.
      nodejs = inputs.nixpkgs-node.legacyPackages.${system}.nodejs_22;

      rzk = "rzk";
      rzk-js = "rzk-js";
      ghcVersion = "ghc963";
      rzk-src = (inputs.nix-filter {
        root = ./${rzk};
        include = [ "app" "src" "test" "package.yaml" ];
      });
      rzk-js-src = (inputs.nix-filter {
        root = ./${rzk-js};
        include = [ "Main.hs" "${rzk-js}.cabal" ];
      });

      parserTools = import ./nix/parser-tools.nix { inherit pkgs ghcVersion; };

      tools = [
        pkgs.cabal-install
        pkgs.hpack
        nodejs
        pkgs.bun
      ] ++ parserTools;

      default = import ./nix/default.nix { inherit inputs pkgs rzk rzk-src ghcVersion tools; };
      ghcjs = import ./nix/ghcjs.nix { inherit inputs pkgs scripts rzk rzk-src rzk-js rzk-js-src ghcVersion tools; };
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
