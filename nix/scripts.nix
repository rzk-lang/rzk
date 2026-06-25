{ pkgs, packages, inputs, nodejs }:
let scripts =
  {
    build-rzk-js = pkgs.writeShellApplication {
      name = "build-rzk-js";
      text =
        let
          public = "rzk-playground/public";
          result = "${public}/rzk-js";
        in
        ''
          cabal install rzk-js --ghcjs --installdir ${result}
          rm -f ${public}/rzk.js
          
          cp ${result}/rzk-js ${public}/rzk.js
          printf "Copied ${result}/rzk-js to ${public}/rzk.js\n"
          
          rm -rf ${result}
          printf "Removed ${result}\n"
        '';
    };

    save-flake = 
      let 
        cache-nix-action = pkgs.fetchgit {
          url = "https://github.com/nix-community/cache-nix-action";
          nonConeMode = true;
          fetchSubmodules = false;
          sparseCheckout = [
            "saveFromGC.nix"
          ];
          hash = "sha256-jl5OdBJTG6DXohERmZQmziSs439HF07xJAdPRdFpATw=";
        };
        saveFromGC = import "${cache-nix-action}/saveFromGC.nix";
      in
        (saveFromGC {
          inherit pkgs inputs;
          inputsInclude = [
            "flake-utils"
            "nixpkgs"
            "nixpkgs-node"
            "miso"
            "nix-filter"
          ];
          derivationsAttrs = {
            inherit (scripts) release-rzk-playground;
          };
        }).package;

    release-rzk-playground = pkgs.writeShellApplication {
      name = "release-rzk-playground";
      runtimeInputs = [ nodejs ];
      text =
        let
          playground = "rzk-playground";
          playground-rzk-js = "${playground}/public/rzk.js";
          release = "rzk-playground-release";
        in
        ''
          rm -f ${playground-rzk-js}
          mkdir -p "$(dirname ${playground-rzk-js})"
          cp ${packages.rzk-js} ${playground-rzk-js}
              
          (
            cd ${playground}
            npm i
          )

          rm -rf ${release}
          mkdir ${release}
          cp -r ${playground}/dist/* ${release}

          printf "Wrote release files to '${release}'\n"
        '';
    };
  };
in scripts
