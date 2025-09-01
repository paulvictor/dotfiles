{ config, pkgs, lib, specialArgs, ... }:

let
  shareLink = pkgs.callPackage ./scripts/shareLink.nix { inherit pkgs config; };
  customizedEmacs = pkgs.callPackage ./packages/emax {};
  inherit (pkgs.stdenv) isLinux;
  gpg-agent-enabled = config.services.gpg-agent.enable;
  privKey = if gpg-agent-enabled then "~/.ssh/id_rsa.pub" else "~/.ssh/id_rsa";
in
with pkgs;
{
  home.packages = [
    ddgr
    doctl
    lsof
    moreutils
    nix-bundle
    opensc
    pbgopy
    prettyping
    skim
    stow
    tree
    unzip
    yt-dlp
  ] ++
  [ procs
    bat
    fd
    tokei
    bottom
    tealdeer
    bandwhich
    grex
  ] ++
  lib.optionals (isLinux) [
    ffmpeg-full
    pcsctools
    pcsclite
    qrcp
    rclone
    rclone-browser
    shareLink
    watchexec # because of https://github.com/NixOS/nixpkgs/issues/160876
  ];

  imports = [
    ./zsh.nix
    ./himalaya.nix
  ]; # ++ lib.optional specialArgs.isLinux ./email.nix;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
    enableBashIntegration = true;
    stdlib = ''
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
        echo "''${direnv_layout_dirs[$PWD]:=$(
            echo -n "$XDG_CACHE_HOME"/direnv/layouts/
            echo -n "$PWD" | shasum | cut -d ' ' -f 1)}"
      }
    '';
  };

  programs.ssh = {
    enable = true;
    compression = true;
    controlMaster = "auto";
    controlPath = "/tmp/ssh_mux_%h_%p_%r";
    controlPersist = "24h";
    forwardAgent = true;
    hashKnownHosts = true;
    serverAliveInterval = 30;
    serverAliveCountMax = 5;
    extraConfig = ''
      IdentityFile ${privKey}
    '';
    matchBlocks = {
      "github" = {
        host = "github";
        hostname = "github.com";
        user = "git";
        identityFile = privKey;
      };
      "bitbucket" = {
        host = "bitbucket";
        hostname = "bitbucket.org";
        identityFile = privKey;
        user = "git";
      };
      "setup-tunnel-1" = {
        host = "setup-tunnel-1";
        hostname = "paulvictor.xyz";
        user = "viktor";
        identityFile = privKey;
        extraOptions = {
          "SessionType" = "none";
          "LocalForward" = "8888 127.0.0.1:8080";
          "RequestTTY" = "no";
        };
      };

      "setup-socks-proxy" = {
        host = "setup-socks-proxy";
        hostname = "localhost";
        user = "paul";
        port = 8888;
        forwardAgent = true;
        identityFile = privKey;
        extraOptions = {
          "SessionType" = "none";
          "DynamicForward" = "127.0.0.1:6565";
          "RequestTTY" = "no";
        };
      };
    };
  };

  programs.gpg = {
    enable = isLinux;
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
  programs.password-store = {
    enable = true;
    package = pass-with-extensions;
    settings = {
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
      PASSWORD_STORE_X_SELECTION = "SECONDARY";
    };
  };

  services.emacs = {
    enable = isLinux;
    package = customizedEmacs;
    client = {
      enable = true;
      arguments = [ "-c" "-n" "-a" "emacs" ];
    };
  };

  programs.emacs = {
    enable = true;
    package = customizedEmacs;
  };

  programs.eza = {
    enable = isLinux;
  };

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    settings.skin = {
      default = "gray(23) none / gray(20) none";
    };
  };

  systemd.user.sessionVariables = lib.optionalAttrs isLinux {
    GNUPGHOME = "${config.home.homeDirectory}/.gnupg";
    EDITOR = "emacsclient -c -n";
  };


  programs.nix-index-database.comma.enable = true;

  home.activation = {
    createSchemeshDirs = lib.hm.dag.entryAfter ["writeBoundary"] ''
      run mkdir -p $VERBOSE_ARG $HOME/.cache/schemesh
      run [ -f $HOME/.cache/schemesh/history.txt ] || touch $HOME/.cache/schemesh/history.txt
      run mkdir -p $VERBOSE_ARG $HOME/.config/schemesh
    '';
  };

}
