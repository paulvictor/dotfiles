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
    comma
    ddgr
    doctl
    ffmpeg-full
    fzf
    # jailed-firefox
    keybase
    lsof
    moreutils
    ncmpcpp
    nix-bundle
    nix-index
    opensc
    openssh
    openssl
    pbgopy
    pms
    prettyping
    qrcp
    skim
    stow
    #texlive.combined.scheme-full
    tree
    unzip
    vifm
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
  ] ++
  lib.optionals (!python310Packages.pyopenssl.meta.broken) [
    awscli2
    haskellPackages.niv
    mpc_cli
  ] ++
  lib.optionals (specialArgs.isLinux) [
    pcsctools
    pcsclite
    rclone
    rclone-browser
    shareLink
    tomb
    watchexec # because of https://github.com/NixOS/nixpkgs/issues/160876
  ];

  imports = [
    ./zsh.nix
  ] ++ lib.optional specialArgs.isLinux ./email.nix;
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
    enable = specialArgs.isLinux;
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
    enable = specialArgs.isLinux;
    package = customizedEmacs;
    client = {
      enable = true;
      arguments = [ "-c" "-n" "-a" "emacs" ];
    };
  };

  programs.emacs = {
    enable = !specialArgs.isLinux;
    package = customizedEmacs;
  };

  programs.exa = {
    enable = specialArgs.isLinux;
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
  systemd.user.sessionVariables = lib.optionalAttrs specialArgs.isLinux {
    GNUPGHOME = "${config.home.homeDirectory}/.gnupg";
  };

}

