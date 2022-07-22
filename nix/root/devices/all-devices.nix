[
  {
    hostName = "sarge";
    system = "x86_64-linux";
  }
#   {
#     hostName = "uriel";
#     system = "x86_64-linux";
#   }
  {
    hostName = "uriel";
    format = "install-iso";
    system = "x86_64-linux";
    extraModules = [
      ({lib, ...}: { boot.loader.grub.enable = lib.mkForce false; })
    ];
  }
  {
    hostName = "lucy";
    format = "amazon";
    system = "x86_64-linux";
    extraModules = [
      (_: { amazonImage.sizeMB = 16 * 1024; })
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
