# GPG with a YubiKey: smartcard daemon, udev/polkit access on NixOS; gpg and gpg-agent on home-manager
{
  flake.modules.nixos.gpg-yubikey = { config, pkgs, ... }: {
    environment.systemPackages = [ pkgs.gnupg ];

    services.udev.packages = with pkgs; [
      yubikey-personalization
      libu2f-host
    ];
    services.pcscd.enable = true;
    users.groups.ykusers = { };
    users.users.${config.me.username}.extraGroups = [ "ykusers" ];
    security.polkit.extraConfig = ''
      polkit.addRule(function (action, subject) {
        if (
          (action.id == "org.debian.pcsc-lite.access_pcsc" ||
            action.id == "org.debian.pcsc-lite.access_card") &&
           subject.isInGroup("ykusers")
        ) {
          return polkit.Result.YES;
        }
      });
    '';
  };

  flake.modules.homeManager.gpg-yubikey = { config, pkgs, lib, ... }: {
    home.packages = with pkgs; [
      pcsc-tools
      pcsclite
      yubico-piv-tool
      yubikey-manager
      yubikey-personalization
    ];

    programs.gpg = {
      enable = true;
      # Imported (and trusted) on every activation; idempotent
      publicKeys = lib.optional (config.me.gpgKey != null) {
        source = config.me.gpgKey;
        trust = "ultimate";
      };
      settings = {
        default-key = "0xA96C9B89755DF7D2";
        default-recipient-self = true;
        require-cross-certification = true;
        no-symkey-cache = true;
      };
      scdaemonSettings = {
        disable-ccid = true;
        pcsc-driver = "${pkgs.pcsclite.lib}/lib/libpcsclite.so.1";
        card-timeout = "1";
        "reader-port Yubico Yubikey" = true;
      };
    };

    systemd.user.sessionVariables.GNUPGHOME = "${config.home.homeDirectory}/.gnupg";

    # Not sure if we can run on darwin
    services.gpg-agent = {
      enable = true;
      enableExtraSocket = true;
      enableSshSupport = true;
      defaultCacheTtl = 3600;
      defaultCacheTtlSsh = 3600;
      maxCacheTtl = 6 * 3600;
      maxCacheTtlSsh = 6 * 3600;
      pinentry.package = pkgs.pinentry-qt;
      enableScDaemon = true;
      sshKeys = [
        "# id_rsa"
        "1F525695DC054E59E3D357E2F76F05DE63F2AD0D"
      ];
    };
    systemd.user.services.yubikey-touch-detector = {
      Install.WantedBy = [ "default.target" ];
      Unit.Description = "Detects when your YubiKey is waiting for a touch";
      Service =
        {
          Environment="PATH=${pkgs.gnupg}/bin:/run/wrappers/bin";
          ExecStart = "${pkgs.yubikey-touch-detector}/bin/yubikey-touch-detector --libnotify";
          Type = "simple";
        };
    };
  };
}
