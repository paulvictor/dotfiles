# NVIDIA GPU server running ollama (CUDA) for local coding models; only for the nvidia host
{ inputs, ... }: {
  allowedUnfree = [
    "open-webui"
    "nvidia-x11" "nvidia-settings"
    "cuda_cudart" "libcublas" "cuda_cccl" "cuda_nvcc" "cuda_cupti" "cuda_nvml_dev" "cuda_nvrtc"
    "cuda_nvtx" "libcufft" "libcufile" "libcurand" "libcusolver" "libnvjitlink" "libcusparse"
    "libcusparse_lt" "cudnn" "cuda_profiler_api" "cuda_cuobjdump" "cuda_nvdisasm" "cuda-merged"
    "cuda_gdb" "cuda_nvprune" "cuda_cuxxfilt" "cuda_sanitizer_api" "libnpp" "libnvvm" "cccl" "cuda_crt"
  ];

  flake.modules.nixos.local-ai-server = { config, pkgs, ... }: {
    imports = [ inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime ];

    services.xserver.videoDrivers = [ "nvidia" ];
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
      host = "0.0.0.0";
      openFirewall = true;
      package = pkgs.ollama-cuda;
      environmentVariables = {
        OLLAMA_ORIGINS = "*";
        OLLAMA_MAX_LOADED_MODELS = "2";
        OLLAMA_KEEP_ALIVE = "120";
        OLLAMA_CONTEXT_LENGTH = toString (64 * 1024);
      };
      loadModels = [
        "qwen3-coder-next:latest"
        "qwen3.6:latest"
        "gemma4:31b"
      ];
    };
    systemd.services.ollama.serviceConfig.MemoryMax = "108G";

    services.open-webui = {
      enable = false;
      port = 3333;
      openFirewall = true;
    };

    nixpkgs.config.cudaSupport = true;
    nix.settings.substituters = [
      "https://cache.nixos-cuda.org"
      "https://cache.flox.dev"
    ];
    nix.settings.trusted-public-keys = [
      "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
      "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
    ];
  };
}
