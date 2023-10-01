{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/e1b4c97ed4ce160afd9ef1574b6a2ff168482f2a";
    miso = {
      url = "github:dmjio/miso/49edf0677253bbcdd473422b5dd5b4beffd83910";
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

      rzk = "rzk";
      rzk-js = "rzk-js";
      ghcVersion = "ghc962";
      rzk-src = (inputs.nix-filter {
        root = ./${rzk};
        include = [ "app" "src" "test" "package.yaml" ];
      });
      rzk-js-src = (inputs.nix-filter {
        root = ./${rzk-js};
        include = [ "Main.hs" "${rzk-js}.cabal" ];
      });

      tools = [
        pkgs.cabal-install
        pkgs.hpack
        pkgs.nodejs_18
        pkgs.bun
      ];

      default = import ./nix/default.nix { inherit inputs pkgs rzk rzk-src ghcVersion tools; };
      ghcjs = import ./nix/ghcjs.nix { inherit inputs pkgs scripts rzk rzk-src rzk-js rzk-js-src ghcVersion tools; };
      scripts = import ./nix/scripts.nix { inherit pkgs packages; };


      packages = {
        default = default.packages.default;
        rzk = default.packages.${rzk};
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
