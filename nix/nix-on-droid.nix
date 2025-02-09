inputs:

with inputs;
nix-on-droid.lib.nixOnDroidConfiguration {
  pkgs = import nixpkgs { system = "aarch64-linux"; };
  modules = [
    ({pkgs, ...}: {
      environment.packages = [ pkgs.nvim ];
      android-integration = {
        am.enable = true;
        termux-open.enable = true;
        termux-open-url.enable = true;
        termux-setup-storage.enable = true;
        termux-wake-lock.enable = true;
        termux-wake-unlock.enable = true;
        xdg-open.enable = true;
      };
      system.stateVersion = "24.05";
    })
  ];
}
