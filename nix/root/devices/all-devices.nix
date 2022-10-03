[
  {
    hostName = "sarge";
    system = "x86_64-linux";
  }
  {
    hostName = "uriel";
    system = "x86_64-linux";
  }
  {
    hostName = "lucy";
    format = "amazon";
    system = "x86_64-linux";
    extraModules = [
      (_: { amazonImage.sizeMB = 32 * 1024; })
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
