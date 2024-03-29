system: nixpkgs:
{
  "viktor@sarge" = {
    system = system.x86_64-linux;
    extraSpecialArgs = {
      hostSpecificImports = [
        ./devices/sarge.nix
      ];
      withGUI = true; # Enable/disable gui programs
      isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
      isDevEnv = true; # For all dev packages
      networkInterface = "wlp2s0";
      isLinux = true;
      hostName = "sarge";
    };
    username = "viktor";
    homeDirectory = "/home/viktor";
    stateVersion = "21.05";
  };
  "viktor@uriel" = {
    system = system.x86_64-linux;
    extraSpecialArgs = {
      hostSpecificImports = [
        ./devices/uriel.nix
      ];
      withGUI = true; # Enable/disable gui programs
      isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
      isDevEnv = true; # For all dev packages
      networkInterface = "wlp59s0";
      isLinux = true;
      hostName = "uriel";
    };
    username = "viktor";
    homeDirectory = "/home/viktor";
    stateVersion = "21.05";
  };
  "viktor@sorlag" = {
    system = system.x86_64-linux;
    extraSpecialArgs = {
      hostSpecificImports = [
        ./devices/sorlag.nix
      ];
      withGUI = true; # Enable/disable gui programs
      isDesktop = true; # Desktop environment setup. Roughly if any of the X related things should be enabled
      isDevEnv = true; # For all dev packages
      networkInterface = "wlp5s0";
      isLinux = true;
      hostName = "sorlag";
    };
    username = "viktor";
    homeDirectory = "/home/viktor";
    stateVersion = "22.05";
  };
  "paul@crash" = {
    system = system.aarch64-darwin;
    extraSpecialArgs = {
      hostSpecificImports = [
        ({config, pkgs, lib,...}: {
          home.sessionPath = [ "/run/current-system/sw/bin" ];
          home.sessionVariables = {
            NIX_PATH = "nixpkgs=${nixpkgs.outPath}";
          };
        })
      ];
      withGUI = false; # Enable/disable gui programs
      isDesktop = false; # Desktop environment setup. Roughly if any of the X related things should be enabled
      isDevEnv = true; # For all dev packages
      networkInterface = "wlp2s0";
      isLinux = false;
      hostName = "crash";
    };
    username = "paul";
    homeDirectory = "/Users/paul";
    stateVersion = "22.05";
  };
}
