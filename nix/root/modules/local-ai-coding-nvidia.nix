{config, inputs, pkgs, ...}:

{
  imports = [ inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime ];
  # Your 4090 specific settings remain here
  services.xserver.enable = false;
  services.xserver.videoDrivers = [ "nvidia" ];
#   hardware.graphics.enable = true;
  hardware.nvidia = {
    modesetting.enable = true; # For wayland
    powerManagement.enable = true;
    # Essential for the 4090's architecture
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    open = true;
  };
  # Use zram for faster memory compression
  zramSwap.enable = true;
  # Environment Variables for Wayland + NVIDIA
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "nvidia";
    GBM_BACKEND = "nvidia-drm";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    # Fixes flickering in Electron apps (VS Code, Discord, etc)
    NIXOS_OZONE_WL = "1";
  };

  services.ollama = {
    enable = true;
    package = pkgs.ollama-cuda;
    loadModels = [ "qwen3-coder-next:latest" "deepseek-coder-v2:latest" ];
  };
  systemd.services.ollama.serviceConfig = {
    MemoryMax = "108G";
  };
  services.open-webui.enable = true;
  nixpkgs.config.cudaSupport = true;
  nix.settings.substituters = [
    "https://cuda-maintainers.cachix.org" "https://cache.flox.dev"
  ];
  nix.settings.trusted-public-keys =
    [
      "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
    ];
}

