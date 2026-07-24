{ config, ... }:

{
  sops.secrets."builder-key" = {
    sopsFile = ../../../secrets/builder-key.conf;
    format = "binary";
    mode = "0400";
  };

  environment.etc."nix/build-capacity.conf".text = ''
    max-jobs = 2
  '';

  nix.distributedBuilds = true;
  nix.settings.builders-use-substitutes = true;
  nix.buildMachines = [{
    hostName = "anarki"; # tailscale hostname of the build machine
    systems = [ "x86_64-linux" "aarch64-linux" ];
    maxJobs = 32;
    speedFactor = 8;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    sshUser = "builder";
    sshKey = config.sops.secrets."builder-key".path;
    protocol = "ssh-ng";
  }];
}
