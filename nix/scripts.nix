{ pkgs, packages, mkFlakesTools }:
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

    save-flake = pkgs.writeShellApplication {
      name = "save-flake";
      runtimeInputs = [ pkgs.jq ];
      text = ''
        # save flake inputs - # https://github.com/NixOS/nix/issues/4250#issuecomment-1146878407

        mkdir -p "/nix/var/nix/gcroots/per-user/$USER"
        
        gc_root_prefix="/nix/var/nix/gcroots/per-user/$USER/$(systemd-escape -p "$PWD")-flake-"
        echo "Adding per-user gcroots..."
        rm -f "$gc_root_prefix"*
        nix flake archive --json 2>/dev/null \
          | jq -r '.inputs | to_entries[] | "ln -fsT "+.value.path+" \"'"$gc_root_prefix"'"+.key+"\""' \
          | while read -r line; \
            do
              eval "$line"
            done
      
        # save scripts

        nix profile install --profile "$gc_root_prefix""${scripts.save-flake.name}" .#${scripts.save-flake.name}
        nix profile install --profile "$gc_root_prefix""${scripts.release-rzk-playground.name}" .#${scripts.release-rzk-playground.name}

        printf "Entries saved with prefix %s\n" "$gc_root_prefix"
      '';
    };

    release-rzk-playground = pkgs.writeShellApplication {
      name = "release-rzk-playground";
      runtimeInputs = [ pkgs.nodejs_18 ];
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

    inherit (mkFlakesTools { root = ../.; dirs = [ "." ]; }) pushToCachix;
  };
in scripts
