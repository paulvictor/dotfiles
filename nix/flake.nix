{
  description = "Meta Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    homeManager.url = "github:nix-community/home-manager/master";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacsOverlay = {
      url = "github:nix-community/emacs-overlay/master";
    };
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/nur";
    ngnk.url = "github:nathyong/ngnk-nix";
    ngnk.inputs.nixpkgs.follows = "nixpkgs";
    ngnk.inputs.flake-utils.follows = "flake-utils";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Adblock hosts
    goodbyeAds.url = "github:jerryn70/GoodbyeAds/master";
    goodbyeAds.flake = false;

    nix-cl = {
      url = "github:Uthar/nix-cl/master";
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    actual-server-repo = {
      url = "github:paulvictor/actual-server";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    guile-swayer = {
      url = "github:paulvictor/guile-swayer";
      flake = false;
    };

    # see https://determinate.systems/posts/extending-nixos-configurations/#using-private-github-inputs-in-flakes
    # to integrate private flakes

    magix = {
      url = "github:dschrempf/magix";
    };

    flake-utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs.flake-utils.follows = "flake-utils";
    };

  };

  outputs = { self, nixpkgs, emacsOverlay, flake-utils, darwin, nix-cl, ... }@inputs :
    let
      inherit (nixpkgs) lib;
      inherit (flake-utils.lib) eachDefaultSystemPassThrough eachDefaultSystem;
      gllock-overlay = import ./overlays/gllock.nix;
      brotab-overlay = import ./overlays/brotab.nix;
      ripgrep-overlay = import ./overlays/ripgrep.nix;
      rofi-fuzzy = import ./overlays/rofi-fuzzy.nix;
      pass-override-overlay = import ./overlays/pass-override.nix;
      wallpaper-overlay = import ./overlays/wallpaper.nix;
      electron-apps = import ./overlays/electronApps;
      surfraw-overlay = import ./overlays/surfraw.nix;
      ql2nix-overlay = import ./overlays/ql2nix.nix;
      pcloudcc-overlay = import ./overlays/pcloud-console-client.nix;
      fish-docker-completion = import ./overlays/fish.nix;
      rofi-theme-overlay = import ./overlays/rofi-theme-overlay.nix;
      warpd-overlay = import ./overlays/warpd.nix;
      #   dyalog-nixos-overlay = import (fetchTarball https://github.com/markus1189/dyalog-nixos/tarball/3e09260ec111541be3e0c7a6c4e700fc042a3a8a) { inherit pkgs; } ;
      overlays = [
        fish-docker-completion
        brotab-overlay
        ripgrep-overlay
        rofi-fuzzy
        pass-override-overlay
        electron-apps
        wallpaper-overlay
        surfraw-overlay
        inputs.nur.overlays.default
        ql2nix-overlay
        inputs.ngnk.overlay
        emacsOverlay.overlay
        pcloudcc-overlay
        rofi-theme-overlay
        warpd-overlay
        inputs.flake-utils-plus.overlay
      ];
      supportedFormats =
        lib.remove "all-formats" (lib.attrNames inputs.nixos-generators.nixosModules);
      nixosModules =
        # attrset of host to configuration
        import ./root/devices/default.nix { inherit inputs lib overlays; };
      imageModules =
        lib.listToAttrs
          (lib.forEach supportedFormats
            (format: lib.nameValuePair format
              (lib.mapAttrs
                (_: m: m ++ [inputs.nixos-generators.nixosModules.${format}])
                nixosModules)));
    in
      (eachDefaultSystem (system:
        {
          images =
            lib.mapAttrs
              (format: configModules:
                lib.mapAttrs
                  (_: modules:
                    inputs.nixos-generators.nixosGenerate
                      {
                        inherit modules format system;
                        specialArgs = {
                          inherit inputs;
                          isPhysicalDevice = false;
                        };
                      })
                  configModules
              )
              imageModules;
        }
      ))
      // (eachDefaultSystemPassThrough (system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          nixosConfigurations =
            lib.mapAttrs
              (_: modules:
                lib.nixosSystem {
                  inherit system pkgs modules;
                  specialArgs = {
                    inherit inputs;
                    isPhysicalDevice = true; # HACK for now
                  };
                })
              nixosModules;
        in {
          inherit nixosConfigurations imageModules;
          darwinConfigurations = import ./darwin/default.nix {
            inherit nixpkgs self pkgs;
            inherit (nixpkgs) lib;
            inherit inputs;
            overlays = overlays;
          };
          homeConfigurations = import ./userland/default.nix { inherit inputs pkgs overlays; };
        }));
}
