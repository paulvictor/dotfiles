{
  description = "Meta Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    homeManager.url = "github:nix-community/home-manager";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    emacsOverlay.inputs.nixpkgs.follows = "nixpkgs";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
    impermanence.url = "github:nix-community/impermanence";
    impermanence.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/nur";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    portable-svc.url = "git+https://tulpa.dev/cadey/portable-svc.git?ref=main";
  };

  outputs = { self, nixpkgs, emacsOverlay, neovim, portable-svc, ... }@inputs :
    let
      system = "x86_64-linux";
      lib = nixpkgs.lib;

      gllock-overlay = import ./overlays/gllock.nix;
      tomb-overlay = import ./overlays/tomb.nix;
      guix-overlay = import ./overlays/guix.nix;
      xdotool-overlay = import ./overlays/xdotool.nix;
      brotab-overlay = import ./overlays/brotab.nix;
      ripgrep-overlay = import ./overlays/ripgrep.nix;
      rofi-fuzzy = import ./overlays/rofi-fuzzy.nix;
      pass-override-overlay = import ./overlays/pass-override.nix;
      pass-extensions-overlay = import ./overlays/pass-extensions.nix;
      ffmpeg-overlay = import ./overlays/ffmpeg.nix;
      wallpaper-overlay = import ./overlays/wallpaper.nix;
      urxvt-perls-overlay = import ./overlays/urxvt-perls.nix;
      electron-apps = import ./overlays/electronApps;
      surfraw-overlay = import ./overlays/surfraw.nix;
      ql2nix-overlay = import ./overlays/ql2nix.nix;
      #   dyalog-nixos-overlay = import (fetchTarball https://github.com/markus1189/dyalog-nixos/tarball/3e09260ec111541be3e0c7a6c4e700fc042a3a8a) { inherit pkgs; } ;

      pkgs = import nixpkgs {
        inherit system;
        config = {
          permittedInsecurePackages = [ "steghide-0.5.1" ];
          keep-derivations = true;
          keep-outputs = true;
          allowUnfree = true;
          vivaldi = {
            proprietaryCodecs = true;
            enableWideVine = true;
          };
        };
        overlays = [
          gllock-overlay
          tomb-overlay
          guix-overlay
          xdotool-overlay
          brotab-overlay
          ripgrep-overlay
          rofi-fuzzy
          pass-override-overlay
          pass-extensions-overlay
          ffmpeg-overlay
          urxvt-perls-overlay
          electron-apps
          wallpaper-overlay
          surfraw-overlay
          inputs.nur.overlay
          inputs.mozilla.overlays.firefox
          emacsOverlay.overlay
          ql2nix-overlay
          portable-svc.overlay
        ];
      };

      mkHomeConfig = extraArgs: inputs.homeManager.lib.homeManagerConfiguration (rec {
        system = extraArgs.system or "x86_64-linux";
        pkgs = extraArgs.pkgs or pkgs; # Can be used to pass different value of pkgs for different systems
        configuration = {
          imports = [
            inputs.impermanence.nixosModules.home-manager.impermanence
            ./userland/home-configuration.nix
          ];
        };
        username = "viktor";
        homeDirectory = "/home/viktor";
      } // extraArgs);

    in
      {
        nixosConfigurations =
          import ./root/devices/default.nix {
            inherit lib nixpkgs system self pkgs;
            inherit (inputs) homeManager sops-nix nixos-generators;
          };
        homeConfigurations = {
          "viktor@uriel" = mkHomeConfig {
            inherit system;
            inherit pkgs;
            extraSpecialArgs = {
              hostSpecificImports = [
                ./userland/devices/uriel.nix
              ];
              withGUI = true; # Enable/disable gui programs
              isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
              isDevEnv = true; # For all dev packages
              networkInterface = "wlp2s0";
            };
          };
          "viktor@sarge" = mkHomeConfig {
            inherit system;
            inherit pkgs;
            extraSpecialArgs = {
              hostSpecificImports = [
                ./userland/devices/sarge.nix
              ];
              withGUI = true; # Enable/disable gui programs
              isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
              isDevEnv = true; # For all dev packages
              networkInterface = "wlp2s0";
            };
          };
        };
      };
}
