{
  description = "GH API retriever & Composer - GHAppy";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];

        pkgs = import nixpkgs { inherit system overlays; };

        additionalPckgs = with pkgs; [ nixfmt rlwrap pandoc ];

        additionalHaskellPckgs = with pkgs.haskellPackages; [
          stack
          structured-haskell-mode
          stylish-haskell
          apply-refact
          cabal-fmt
          cabal-install
          fourmolu
          ghcid
          hasktags
          hlint
          zlib
          haskell-language-server
          doctest
        ];

        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "GHAppy";
            root = self;
            withHoogle = true;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
              (additionalHaskellPckgs ++ additionalPckgs);
          };
      in {

        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
