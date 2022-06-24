system:
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
    };
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
      networkInterface = "wlp2s0";
    };
  };
  "paul@crash" = {
    system = system.aarch64-darwin;
    extraSpecialArgs = {
      hostSpecificImports = [
      ];
      withGUI = false; # Enable/disable gui programs
      isDesktop = false; # Desktop environment setup. Roughly if any of the X related things should be enabled
      isDevEnv = false; # For all dev packages
      networkInterface = "wlp2s0";
    };
  };
}
