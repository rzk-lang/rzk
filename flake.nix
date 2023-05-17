{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    miso = {
      url = "github:dmjio/miso/5c66ed20818ce4aff81aaefbd5789007717923eb";
      flake = false;
    };
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      rzk = "rzk";
      try-rzk = "try-rzk";
      ghcVersion = "ghc927";

      # select a Haskell package set for a specified GHC version
      hpkgs = pkgs.haskell.packages.${ghcVersion};

      inherit (pkgs.haskell.lib) overrideCabal;

      # Provide overrides
      # https://nixos.wiki/wiki/Haskell#Overrides
      # An override should include a local package into the Haskell package set
      override = {
        overrides = self: super: {
          ${rzk} = overrideCabal (self.callCabal2nix rzk ./${rzk} { }) (x: {
            librarySystemDepends = [ pkgs.alex pkgs.happy ] ++ (x.librarySystemDepends or [ ]);
          });
          ${try-rzk} = overrideCabal (self.callCabal2nix try-rzk ./${try-rzk} { }) (x: {
            executableSystemDepends = [ self.ghcjs-prim self.ghcjs-base ] ++ (x.executableSystemDepends or [ ]);
          });
        };
      };
      hpkgs_ = hpkgs.override override;

      # Get all dependencies of local Haskell packages excluding these local packages
      # This approach is useful for cases when a local package A depends on a local package B
      # In this case, package B won't be built by Nix as a dependency of A
      getHaskellPackagesDeps = someHaskellPackages: let l = pkgs.lib.lists; in (l.subtractLists someHaskellPackages (l.concatLists (map (package: l.concatLists (__attrValues package.getCabalDeps)) someHaskellPackages)));

      # build a GHC with the dependencies of local Haskell packages
      ghcForPackages = localHaskellPackageNames: hpkgs_.ghcWithPackages (ps: (getHaskellPackagesDeps (map (x: ps.${x}) localHaskellPackageNames) ++ [ ps.haskell-language-server ])); # Why provide HLS here - https://github.com/NixOS/nixpkgs/issues/225895#issuecomment-1509991742

      # GHC with dependencies of local Haskell packages
      ghc = ghcForPackages [ rzk try-rzk ];

      # tools that should be available in a development shell
      tools = [
        pkgs.cabal-install
        pkgs.hpack
        # haskell-language-server is already available as a GHC package
        ghc
      ];

      misoNix = (import "${inputs.miso.outPath}/default.nix" { inherit system; });
      pkgsMiso = misoNix.pkgs;

      # TODO add jsaddle version of the app
      # https://github.com/dmjio/miso/tree/master/sample-app-jsaddle
      # try-rzk-dev =
      #   let
      #     pkgsDev = pkgsMiso.haskell.packages.ghc865;
      #     rzk = pkgsDev.callPackage rzk/rzk.nix { inherit (pkgs) hpack; };
      #   in
      #   pkgsDev.callCabal2nix try-rzk ./${try-rzk} { miso = misoNix.miso-jsaddle; rzk = rzk; };

      try-rzk-exe =
        let
          pkgsRelease = pkgsMiso.haskell.packages.ghcjs;
          rzk = pkgsRelease.callPackage rzk/rzk.nix { inherit (pkgs) hpack; };
        in
        pkgsRelease.callCabal2nix try-rzk ./${try-rzk} { rzk = rzk; };

      packages = {
        rzk = hpkgs_.${rzk};
        try-rzk = try-rzk-exe;
      };

      devShells = {
        default = pkgs.mkShell {
          shellHook = "export LANG=C.utf8";
          buildInputs = tools;
        };
        ghcjs = try-rzk-exe.env.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ pkgs.cabal-install pkgs.hpack ];
        });
      };
    in
    {
      inherit packages devShells;
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
