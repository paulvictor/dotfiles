# sshd + mosh on the NixOS side, the ssh client on the home-manager side
{
  flake.modules.nixos.ssh = { config, lib, ... }: {
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin =  "prohibit-password";
        GatewayPorts = "yes";
        PasswordAuthentication = false;
        PubkeyAuthentication = true;
        KbdInteractiveAuthentication = false;
        StrictModes = false;
        UsePAM = false;
        AllowUsers = [ config.me.username ];
      };
    };
    programs.mosh.enable = true;
  };

  flake.modules.homeManager.ssh = { config, lib, ... }: {
    options.custom.ssh.identityFile = lib.mkOption {
      type = lib.types.str;
      default =
        if config.services.gpg-agent.enable
        then "~/.ssh/id_rsa.pub" # the private key lives on the smartcard
        else "~/.ssh/id_rsa";
      description = "Identity used for every host";
    };

    config.programs.ssh = {
      enable = true;
      enableDefaultConfig = false;
      settings = {
        "github" = {
          HostName = "github.com";
          User = "git";
          IdentityFile = config.custom.ssh.identityFile;
        };
        "bitbucket" = {
          HostName = "bitbucket.org";
          User = "git";
          IdentityFile = config.custom.ssh.identityFile;
        };
        "*" = lib.hm.dag.entryAfter [ "github" "bitbucket" ] {
          Compression = true;
          ControlMaster = "auto";
          ControlPath = "/tmp/ssh_mux_%h_%p_%r";
          ControlPersist = "24h";
          ForwardAgent = true;
          HashKnownHosts = true;
          IdentityFile = config.custom.ssh.identityFile;
          ServerAliveInterval = 30;
          ServerAliveCountMax = 5;
        };
      };
    };
  };
}
