{
  "viktor@uriel" = {
    additionalModules = [
      {
        home.username = "viktor";
        home.homeDirectory = "/home/viktor";
        home.stateVersion = "21.05";
        services.batteryAlert.enable = true;
      }
    ];
    withGUI = true; # Enable/disable gui programs
    isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
    isDevEnv = true; # For all dev packages
  };
  "viktor@sorlag" = {
    additionalModules = [
      {
        home.username = "viktor";
        home.homeDirectory = "/home/viktor";
        home.stateVersion = "22.05";
      }
    ];
    withGUI = true; # Enable/disable gui programs
    isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
    isDevEnv = true; # For all dev packages
  };
  "paul@crash" = {
    extraSpecialArgs = {
      hostSpecificImports = [
        ({config, pkgs, lib,...}: {
          home.sessionPath = [ "/run/current-system/sw/bin" ];
          home.sessionVariables = {
#             NIX_PATH = "nixpkgs=${nixpkgs.outPath}";
          };
        })
      ];
    };
    withGUI = false; # Enable/disable gui programs
    isDesktop = false; # Desktop environment setup. Roughly if any of the X related things should be enabled
    isDevEnv = true; # For all dev packages
    additionalModules = [
      {
        home.username = "paul";
        home.homeDirectory = "/Users/paul";
        home.stateVersion = "22.05";
      }
    ];
  };
}
