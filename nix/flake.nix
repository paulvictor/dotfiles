{
  description = "Meta Config";

#   nixConfig = {
#     substituters = [ "https://nix-community.cachix.org" ];
#     trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
#   };

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
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim.url = "github:nix-community/neovim-nightly-overlay";
    impermanence.url = "github:nix-community/impermanence";
    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur.url = "github:nix-community/nur";
    mozilla.url = "github:mozilla/nixpkgs-mozilla";
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

    kmonad = {
      url = "github:kmonad/kmonad/master?dir=nix";
#       inputs.nixpkgs.follows = "nixpkgs";
    };

    juspay-config = {
      url = "git+ssh://git@bitbucket.org/juspay/pauls-work-config.git";
    };

    nix-cl = {
      url = "github:Uthar/nix-cl/master";
    };

    comma = {
      url = "github:nix-community/comma/master";
    };

    nix-index-database = {
      url = "github:Mic92/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    firefox-nightly = {
      url = "github:nix-community/flake-firefox-nightly";
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

  };

  outputs = { self, nixpkgs, emacsOverlay, flake-utils, darwin, nix-cl, ... }@inputs :
    let
      gllock-overlay = import ./overlays/gllock.nix;
      tomb-overlay = import ./overlays/tomb.nix;
      xdotool-overlay = import ./overlays/xdotool.nix;
      brotab-overlay = import ./overlays/brotab.nix;
      ripgrep-overlay = import ./overlays/ripgrep.nix;
      rofi-fuzzy = import ./overlays/rofi-fuzzy.nix;
      pass-override-overlay = import ./overlays/pass-override.nix;
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
      xsecurelock-overlay = import ./overlays/xsecurelock.nix;
      rofi-theme-overlay = import ./overlays/rofi-theme-overlay.nix;
      actual-server-overlay = import ./overlays/actual-server.nix { inherit (inputs) actual-server-repo; };
      keyd-overlay = import ./overlays/keyd.nix;
      warpd-overlay = import ./overlays/warpd.nix;
      #   dyalog-nixos-overlay = import (fetchTarball https://github.com/markus1189/dyalog-nixos/tarball/3e09260ec111541be3e0c7a6c4e700fc042a3a8a) { inherit pkgs; } ;
      linuxOverlays = [
        fish-docker-completion
        tomb-overlay
        xdotool-overlay
        brotab-overlay
        ripgrep-overlay
        rofi-fuzzy
        pass-override-overlay
        ffmpeg-overlay
        urxvt-overlay
        electron-apps
        wallpaper-overlay
        surfraw-overlay
        inputs.nur.overlay
        inputs.mozilla.overlays.firefox
        ql2nix-overlay
        inputs.ngnk.overlay
        emacsOverlay.overlay
        pcloudcc-overlay
        xsecurelock-overlay
        inputs.kmonad.overlays.default
        inputs.comma.overlays.default
        rofi-theme-overlay
        actual-server-overlay
        warpd-overlay
      ];
      darwinOverlays = [
        pyopenssl-fix-hack
        xdotool-overlay
        brotab-overlay
        ripgrep-overlay
        pass-override-overlay
        ffmpeg-overlay
        electron-apps
        inputs.nur.overlay
        inputs.mozilla.overlays.firefox
        ql2nix-overlay
        inputs.ngnk.overlay
        emacsOverlay.overlay
        inputs.comma.overlays.default
      ];
      pkgsFor = system:
        let
          _nixpkgs = import nixpkgs { inherit system; };
          inherit (_nixpkgs.stdenv) isLinux;
          patched-nixpkgs = _nixpkgs.applyPatches {
            name = "nixpkgs-patched";
            src = nixpkgs;
            patches = import ./patches.nix { pkgs = _nixpkgs; };
          };
        in
        import nixpkgs {
          inherit system;
          config = {
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
#       deploy.nodes = createNixDeploy self.nixosConfigurations;
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
          inherit (inputs) nixpkgs homeManager impermanence flake-utils nix-index-database firefox-nightly;
        };
    };
}
