{
  description = "Meta Config";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/a7855f2235a1876f97473a76151fec2afa02b287";
    flake-utils.url = "github:numtide/flake-utils";
    homeManager.url = "github:nix-community/home-manager/master";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";
    homeManager.inputs.utils.follows = "flake-utils";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
    emacsOverlay.url = "github:nix-community/emacs-overlay";
    emacsOverlay.inputs.nixpkgs.follows = "nixpkgs";
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    neovim.inputs.nixpkgs.follows = "nixpkgs";
    neovim.inputs.flake-utils.follows = "flake-utils";
    impermanence.url = "github:nix-community/impermanence";
    impermanence.inputs.nixpkgs.follows = "nixpkgs";
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/nur";
    nur.inputs.nixpkgs.follows = "nixpkgs";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
    portable-svc.url = "git+https://tulpa.dev/cadey/portable-svc.git?ref=main";
    portable-svc.inputs.nixpkgs.follows = "nixpkgs";
    ngnk.url = "github:nathyong/ngnk-nix";
    ngnk.inputs.nixpkgs.follows = "nixpkgs";
    ngnk.inputs.flake-utils.follows = "flake-utils";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Adblock hosts
    stevenBlack.url = "github:StevenBlack/hosts/master";
    stevenBlack.flake = false;
    goodbyeAds.url = "github:jerryn70/GoodbyeAds/master";
    goodbyeAds.flake = false;

  };

  outputs = { self, nixpkgs, emacsOverlay, neovim, flake-utils, darwin, ... }@inputs :
    let
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
      urxvt-overlay = import ./overlays/urxvt-with-plugins.nix;
      electron-apps = import ./overlays/electronApps;
      surfraw-overlay = import ./overlays/surfraw.nix;
      ql2nix-overlay = import ./overlays/ql2nix.nix;
      pcloudcc-overlay = import ./overlays/pcloud-console-client.nix;
      pyopenssl-fix-hack = import ./overlays/pyopenssl-broken-fix-hack.nix;
      fish-docker-completion = import ./overlays/fish.nix;
      #   dyalog-nixos-overlay = import (fetchTarball https://github.com/markus1189/dyalog-nixos/tarball/3e09260ec111541be3e0c7a6c4e700fc042a3a8a) { inherit pkgs; } ;
      linuxOverlays = [
        fish-docker-completion
        neovim.overlay
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
        urxvt-overlay
        electron-apps
        wallpaper-overlay
        surfraw-overlay
        inputs.nur.overlay
        inputs.mozilla.overlays.firefox
        ql2nix-overlay
        inputs.portable-svc.overlay
        inputs.ngnk.overlay
        emacsOverlay.overlay
        pcloudcc-overlay
      ];
      darwinOverlays = [
        pyopenssl-fix-hack
        neovim.overlay
        xdotool-overlay
        brotab-overlay
        ripgrep-overlay
        pass-override-overlay
        pass-extensions-overlay
        ffmpeg-overlay
        electron-apps
        inputs.nur.overlay
        inputs.mozilla.overlays.firefox
        ql2nix-overlay
        inputs.ngnk.overlay
        emacsOverlay.overlay
      ];
      pkgsFor = system:
        let
          stdenv = (import nixpkgs { inherit system; }).stdenv;
          inherit (stdenv) isLinux;
        in
        import nixpkgs {
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
          overlays =
            if isLinux
            then linuxOverlays
            else darwinOverlays;
      };
#         lib = nixpkgs.lib // import ./ip.nix { inherit pkgs; };
    in {
      nixosConfigurations =
        import ./root/devices/default.nix {
          inherit self pkgsFor inputs;
          inherit (nixpkgs) lib;
        };
      darwinConfigurations = import ./darwin/default.nix {
          inherit nixpkgs self pkgsFor;
          inherit (nixpkgs) lib;
          inherit inputs;
          overlays = darwinOverlays;
      };
      homeConfigurations =
        import ./userland/default.nix {
          inherit pkgsFor;
          inherit (nixpkgs) lib;
          inherit (inputs) nixpkgs homeManager impermanence flake-utils;
        };
    };
}
