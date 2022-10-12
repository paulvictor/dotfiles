{ self, pkgsFor, inputs, ... }:

let
  inherit (inputs) sops-nix nixos-generators flake-utils homeManager nixpkgs kmonad;

  inherit (builtins) attrNames isAttrs readDir listToAttrs elem;

  inherit (nixpkgs.lib) filterAttrs hasSuffix mapAttrs' nameValuePair removeSuffix hasPrefix forEach mkIf optionals nixosSystem;

  setupNixPath = {config, lib, ...}: {
    environment.etc =
      mapAttrs'
        (name: value: { name = "nix/inputs/${name}"; value = { source = value.outPath; }; })
        inputs;
    nix.nixPath = [ "/etc/nix/inputs" ];
  };

  moduleArgs = {
    inherit (inputs) stevenBlack goodbyeAds kmonad neovim;
  };

  mkModules = hostName: system:
    let
      common = {
        imports = [
          ../guix/modules/services/guix.nix
          ../common-config.nix
          ../caches.nix
        ];

        system.configurationRevision = mkIf (self ? rev) self.rev;
        networking.hostName = hostName;
        nixpkgs.pkgs = pkgsFor system;
        nix.registry.nixpkgs.flake = nixpkgs;
      };

      machine = import "${toString ./.}/${hostName}/default.nix";

    in [
      setupNixPath
      common
      sops-nix.nixosModule
      homeManager.nixosModule
      kmonad.nixosModules.default
      machine
      ../modules/viktor.nix
      ../modules/kmonad.nix
      ../modules/workstations.nix
      ../modules/ssh.nix
    ];

  mkNixosSystem = hostName: system:
    let
      pkgs = pkgsFor system;
    in nixosSystem {
      inherit system pkgs;
      modules = mkModules hostName system;
      specialArgs = moduleArgs // { inherit system; isPhysicalDevice = true;} ;
    };

  deviceConfigs = import ./all-devices.nix;

in
listToAttrs
  (forEach
    deviceConfigs
    (deviceConfig:
      let
        inherit (deviceConfig) hostName system extraModules format isPhysicalDevice;
        generatedImage = nixos-generators.nixosGenerate {
          pkgs = pkgsFor system;
          format = format;
          modules =
            mkModules hostName system
            ++ (optionals (deviceConfig ? extraModules) deviceConfig.extraModules);
          specialArgs = moduleArgs // { inherit system; isPhysicalDevice = false;};
        };
      in
        {
          name = hostName;
          value =
            if !(deviceConfig ? format)
            then mkNixosSystem hostName system
            else generatedImage;
        }))
