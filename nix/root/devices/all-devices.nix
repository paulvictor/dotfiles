[
  {
    hostName = "sorlag";
    system = "x86_64-linux";
    customisations = {
      isWorkMachine = false;
      onZFS = false;
    };
  }
  {
    hostName = "sarge";
    system = "x86_64-linux";
    customisations = {
      isWorkMachine = true;
      onZFS = true;
    };
  }
  {
    hostName = "uriel";
    system = "x86_64-linux";
    customisations = {
      isWorkMachine = true;
      onZFS = false;
    };
  }
  {
    hostName = "orbb";
    format = "amazon";
    system = "aarch64-linux";
    extraModules = [
      (_: { amazonImage.sizeMB = 8 * 1024; })
    ];
  }
  {
    isPhysicalDevice = false;
    hostName = "orbb-on-the-machine";
    system = "aarch64-linux";
    extraModules = [
      ({ config, modulesPath, lib, pkgs, ...}: {
        imports = [
          "${modulesPath}/virtualisation/amazon-options.nix"
          "${modulesPath}/virtualisation/amazon-image.nix"
        ];
      })
    ];
  }
  {
    hostName = "bones";
    format = "do";
    system = "x86_64-linux";
  }
  {
    hostName = "crash";
    format = "gce";
    system = "x86_64-linux";
  }
  {
    hostName = "patriot";
    format = "vm-nogui";
    system = "x86_64-linux";
  }
]
