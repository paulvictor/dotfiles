{pkgs, inputs, ...}:
let
  inherit (pkgs) lib;
  # Evaluated standalone so the primary username can name the configs
  # (home-manager looks up `$USER@$HOSTNAME` by default)
  me = (lib.evalModules {
    modules = [ ../modules/me.nix { _module.args = { inherit pkgs; }; } ];
  }).config.me;
in
lib.mapAttrs'
  (hostname: attrs:
    lib.nameValuePair "${me.username}@${hostname}"
      (inputs.homeManager.lib.homeManagerConfiguration ({
        inherit pkgs;
        extraSpecialArgs = {inherit inputs hostname;};
        modules = [
          inputs.nix-index-database.homeModules.nix-index
          inputs.nixpi.homeModules.default
          inputs.pi-packages.homeModules.default
          ../modules/me.nix
          ./home-configuration.nix
          ({config, ...}: {
            home.username = lib.mkDefault config.me.username;
            home.homeDirectory = lib.mkDefault "/home/${config.me.username}";
          })
        ]
        ++ (lib.optional (attrs.withGUI or true) ./gui-config.nix)
        ++ (lib.optional (attrs.isDevEnv or true)  ./dev-config.nix )
        ++ (lib.optional (attrs.isDesktop or true) ./desktop-config.nix)
        ++ attrs.additionalModules;
      })))
  (import ./all-devices.nix)
