# zsh with direnv, eza, broot and the usual command line tools
{ inputs, ... }: {
  flake.modules.homeManager.shell = { config, pkgs, lib, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) isLinux;
    in
    {
      imports = [
        inputs.nix-index-database.homeModules.nix-index
        ./_zsh.nix
      ];

      programs.nix-index-database.comma.enable = true;

      xdg.configFile."zsh/themes/spaceship.zsh-theme".source =
        "${pkgs.spaceship-prompt}/lib/spaceship-prompt/spaceship.zsh";

      home.packages = with pkgs; [
        dante
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
        procs
        bat
        fd
        tokei
        bottom
        tealdeer
        bandwhich
        grex
      ] ++ lib.optionals isLinux [
        ffmpeg
        qrcp
        watchexec # because of https://github.com/NixOS/nixpkgs/issues/160876
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
        '';
      };

      programs.eza.enable = isLinux;

      programs.broot = {
        enable = true;
        enableZshIntegration = true;
        settings.skin = {
          default = "gray(23) none / gray(20) none";
        };
      };
    };
}
