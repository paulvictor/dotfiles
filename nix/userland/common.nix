{ config, pkgs, lib, specialArgs, ... }:

let

  shareLink = pkgs.callPackage ./scripts/shareLink.nix { inherit pkgs config; };

  customizedEmacs = pkgs.callPackage ./packages/emax {};

  mk-jail-profile = import ./config/jail-profile.nix;

  pms = pkgs.callPackage ./packages/pms {};

in
with pkgs;
{
  home.packages = [
    awscli2
    comma
    ddgr
    doctl
    ffmpeg-full
    fzf
    haskellPackages.niv
    # jailed-firefox
    keybase
    lsof
    moreutils
    mpc_cli
    ncmpcpp
    nix-bundle
    nix-index
    opensc
    openssh
    openssl
    pbgopy
    #pCloudCC
    pcsclite
    pcsctools
    pms
    prettyping
    qrcp
    rclone
    rclone-browser
    shareLink
    skim
    stow
    #texlive.combined.scheme-full
    tomb
    tree
    unzip
    vifm
    watchexec
    weechat
    youtube-dl
    z-lua
  ] ++
  [ procs
    bat
    fd
    tokei
    bottom
    tealdeer
    bandwhich
    grex
  ];

  imports = [
    ./zsh.nix
    ./email.nix
  ];
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

#       use_flake() {
#         watch_file flake.nix
#         watch_file flake.lock
#         eval "$(nix print-dev-env --profile "$(direnv_layout_dir)/flake-profile")"
#       }
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
      IdentityFile ~/.ssh/id_rsa.pub
    '';
    matchBlocks = {
      "github" = {
        host = "github";
        hostname = "github.com";
        user = "git";
        identityFile = "~/.ssh/id_rsa.pub";
      };
      "bitbucket" = {
        host = "bitbucket";
        hostname = "bitbucket.org";
        user = "git";
        identityFile = "~/.ssh/id_rsa.pub";
      };
    };
  };

  programs.gpg = {
    enable = pkgs.stdenv.isLinux;
    settings = {
      default-key = "0xA96C9B89755DF7D2";
      default-recipient-self = true;
      require-cross-certification = true;
      no-symkey-cache = true;
    };
    scdaemonSettings = {
      disable-ccid = true;
      pcsc-driver = "${pkgs.pcsclite.out}/lib/libpcsclite.so";
      card-timeout = "1";
    };
  };
  programs.password-store = {
    enable = true;
    package = pass-with-extensions;
    settings = {
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
      PASSWORD_STORE_KEY = "3E6925C73B18D3DB43A2104EA96C9B89755DF7D2";
      PASSWORD_STORE_X_SELECTION = "SECONDARY";
    };
  };

  services.emacs = {
    enable = pkgs.stdenv.isLinux;
    package = customizedEmacs;
    client= {
      enable = true;
      arguments = [ "-c" "-n" "-a" "emacs" ];
    };
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };

  programs.neovim.enable = true;

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    skin = {
      default = "gray(23) none / gray(20) none";
    };
  };
} // lib.mkIf stdenv.isLinux {
  systemd.user.sessionVariables = {
    GNUPGHOME = "${config.home.homeDirectory}/.gnupg";
  };
}

