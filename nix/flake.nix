{
  description = "Meta Config";
  nixConfig = {
    substituters =
      "https://paulvictor.cachix.org https://nix-community.cachix.org https://cache.nixos.org https://cuda-maintainers.cachix.org";
    trusted-substituters =
      "https://paulvictor.cachix.org https://cache.nixos.org https://cuda-maintainers.cachix.org https://cache.flox.dev";
    trusted-public-keys =
      "paulvictor.cachix.org-1:tuOSw1NsHLZJUXZC9L0QdegOFqpLvcXdQyGKTU+Cic4= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E= flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs=";
  };

  # https://elis.nu/blog/2022/10/outsourcing-nixos-compile-time-to-microsoft/
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts.url = "github:hercules-ci/flake-parts";
    homeManager.url = "github:nix-community/home-manager/master";
    homeManager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";

    emacsOverlay.url = "github:nix-community/emacs-overlay/master";

    nur.url = "github:nix-community/nur";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Adblock hosts
    goodbyeAds.url = "github:jerryn70/GoodbyeAds/master";
    goodbyeAds.flake = false;

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

    nix-on-droid = {
      url = "github:nix-community/nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # see https://determinate.systems/posts/extending-nixos-configurations/#using-private-github-inputs-in-flakes
    # to integrate private flakes

    magix.url = "github:dschrempf/magix";

    flake-utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs.flake-utils.follows = "flake-utils";
    };

    vieb-nix.url = "github:tejing1/vieb-nix";
    vieb-nix.inputs.nixpkgs.follows = "nixpkgs";

    microvm.url = "github:microvm-nix/microvm.nix";
    microvm.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    schemesh = {
      url = "github:cosmos72/schemesh";
      flake = false;
    };

  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = inputs.nixpkgs.lib.systems.flakeExposed;
    imports =
      [inputs.homeManager.flakeModules.home-manager] ++
      (with builtins;
        map
          (fn: ./flake-parts/${fn})
          (attrNames (readDir ./flake-parts)));
  };
}
