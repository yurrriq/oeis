{
  description = "Some sequences from OEIS";

  inputs = {
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/daf7bb95";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-24.05";
    nix-vscode-extensions = {
      inputs = {
        flake-compat.follows = "flake-compat";
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/nix-vscode-extensions";
    };
    pre-commit-hooks-nix = {
      inputs = {
        flake-compat.follows = "flake-compat";
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/git-hooks.nix";
    };
    treefmt-nix = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
      ];

      perSystem = { config, lib, pkgs, self', system, ... }: {
        _module.args.pkgs = import nixpkgs {
          overlays = [
            inputs.nix-vscode-extensions.overlays.default
          ];
          inherit system;
        };

        devShells.default = pkgs.mkShell {
          FONTCONFIG_FILE = with pkgs; makeFontsConf {
            fontDirectories = [
              (nerdfonts.override { fonts = [ "Iosevka" ]; })
            ];
          };

          inputsFrom = [
            self'.packages.oeis.env
            config.pre-commit.devShell
          ];

          nativeBuildInputs = with pkgs; [
            cabal-install
            ghcid
            haskell-language-server
            haskellPackages.pointfree
            # semver-tool
            (
              let
                version = lib.getVersion vscodium;
                extensions = forVSCodeVersion version;
              in
              vscode-with-extensions.override {
                vscode = vscodium;
                vscodeExtensions = with extensions.vscode-marketplace; [
                  editorconfig.editorconfig
                  haskell.haskell
                  jnoortheen.nix-ide
                  justusadam.language-haskell
                  mkhl.direnv
                  # sjurmillidahl.ormolu-vscode
                  tuttieee.emacs-mcx
                ];
              }
            )
          ];
        };

        packages = {
          oeis = pkgs.haskellPackages.callCabal2nix "oeis" self { };
          default = self'.packages.oeis;
        };

        pre-commit.settings = {
          hooks = {
            editorconfig-checker.enable = true;
            treefmt.enable = true;
          };
        };

        treefmt = {
          package = pkgs.treefmt1;
          projectRootFile = ./flake.nix;
          programs = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
            prettier = {
              enable = true;
              excludes = [
                "docs/**/*"
              ];
            };
          };
        };
      };
    };
}
