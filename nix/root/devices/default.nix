{ self, pkgsFor, inputs, ... }:

let
  inherit (inputs) sops-nix nixos-generators flake-utils homeManager nixpkgs kmonad;

  inherit (builtins) attrNames isAttrs readDir listToAttrs elem;

  inherit (nixpkgs.lib) filterAttrs hasSuffix mapAttrs' nameValuePair removeSuffix hasPrefix forEach mkIf optionals nixosSystem mkForce;

  setupNixPath = {config, lib, ...}: {
    environment.etc =
      mapAttrs'
        (name: value: { name = "nix/inputs/${name}"; value = { source = value.outPath; }; })
        inputs;
    nix.nixPath = [ "/etc/nix/inputs" ];
  };

  moduleArgs = {
    inherit (inputs) stevenBlack goodbyeAds kmonad neovim nixpkgs;
  };

  mkModules = args:
    let
      inherit (args) hostName system ;
      customisations = args.customisations or {};
      common = {
        imports = [
          inputs.impermanence.nixosModules.impermanence
          ../common-config.nix
          ../caches.nix
#           ../tailscale.nix
          ../modules/actual-server.nix
        ];

        system.configurationRevision = mkIf (self ? rev) self.rev;
        networking.hostName = hostName;
        nixpkgs.pkgs = mkForce (pkgsFor system);
        nix.registry.nixpkgs.flake = nixpkgs;
      };

      machine = import "${toString ./.}/${hostName}/default.nix";

    in [
      setupNixPath
      common
      sops-nix.nixosModules.sops
      homeManager.nixosModule
      machine
      ../modules/viktor.nix
      ../modules/workstations.nix
      ../modules/ssh.nix
    ] ++ (optionals (customisations.isWorkMachine or false) [ inputs.juspay-config.nixosModules.${system}.juspay-cachix ]);

  mkNixosSystem = {hostName, system, customisations, isPhysicalDevice, extraModules}:
    let
      pkgs = pkgsFor system;
    in nixosSystem {
      inherit system pkgs;
      modules = mkModules {inherit hostName system customisations;} ++ extraModules;
      specialArgs =
        moduleArgs // { inherit system customisations hostName isPhysicalDevice; } ;
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
            mkModules {inherit hostName system; customisations = deviceConfig.customisations or {};}
            ++ (optionals (deviceConfig ? extraModules) deviceConfig.extraModules);
          specialArgs = moduleArgs // { inherit system; isPhysicalDevice = false;};
        };
      in
        {
          name = hostName;
          value =
            if !(deviceConfig ? format)
            then
              mkNixosSystem {
                inherit hostName system;
                customisations = deviceConfig.customisations or {};
                isPhysicalDevice = deviceConfig.isPhysicalDevice or true;
                extraModules = deviceConfig.extraModules or [];
              }
            else generatedImage;
        }))
