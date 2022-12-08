{
  description = "GH API retriever & Composer - GHAppy";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

    pch = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, pch, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          overlays = [ ];

          pkgs = import nixpkgs { inherit system overlays; };

          # attrset contains a pre-commit-check with a shellHook
          # and a formatCheck that can be run
          pc-check = pch.lib.${system}.run {
            src = ./.;
            hooks = {
              # nix formatting, linting
              nixpkgs-fmt.enable = true;
              statix.enable = true;

              # haskell formatting, linting
              fourmolu.enable = true;
              hlint.enable = true;

              # cabal formatting
              cabal-fmt.enable = true;

              # shell formatting, linting
              shellcheck.enable = true;
              shfmt.enable = true;

              # markdown linting
              markdownlint.enable = true;
            };

            settings = { };
          };

          additionalPckgs = with pkgs; [ nixpkgs-fmt rlwrap pandoc ];

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
              root = ./.;
              withHoogle = true;
              modifier = drv:
                (pkgs.haskell.lib.addBuildTools drv
                  (additionalHaskellPckgs ++ additionalPckgs)
                );
            };
        in
        {

          # Used by `nix build` & `nix run` (prod exe)
          defaultPackage = project false;

          checks = {
            formatting = pc-check;
          };

          # Used by `nix develop` (dev shell)
          devShell = pkgs.mkShell {
            inherit (project true) buildInputs nativeBuildInputs;
            inherit (pc-check) shellHook;
          };
        }) // { herculesCI.ciSystems = [ "x86_64-linux" ]; };
}
