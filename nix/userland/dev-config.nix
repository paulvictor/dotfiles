{ config, pkgs, lib, specialArgs, ... }:

with pkgs;
lib.mkIf
  specialArgs.isDevEnv
  (
    let
      tmuxWithConfig = pkgs.callPackage ./packages/tmux {};

      easy-purescript-nix =  pkgs.fetchFromGitHub {
        owner = "justinwoo";
        repo = "easy-purescript-nix";
        rev = "3630943b74f681289ed87a0ed6c3e502556ddebb";
        sha256 = "fhr5sisPsm2uzm0VenH7Urpc+AcI39OGpPNaUVTD/8Q=";
      };
    in
      {
        home.file.".ghc/ghci.conf".text = ''
          :def hoogle \s -> return $ ":! hoogle search --color --count=15 \"" ++ s ++ "\""
          :def doc \s -> return $ ":! hoogle search --color --info \"" ++ s ++ "\""
        '';
        home.file.".gitconfig".text = builtins.readFile ./config/git-config;
        programs.fzf = {
          enable = true;
          enableZshIntegration = true;
        };
        home.packages =
          [
            silver-searcher
#             android-studio
            axel
            bind
            bindfs
            #dyalog-nixos.dyalog #dyalog-nixos.ride
            enscript
            entr
            gnumake
            google-cloud-sdk
            jq
            k9s
            kubectl
            kubectx
            kube-prompt
            kubetail
            msgpack-tools
            nix-bundle
            nix-index
            #     nixos-generators
            nodejs
            nodePackages.bower
            nodePackages.bower2nix
            nodePackages.node2nix
            pv
            ruby
            sbcl
            tmate
            tmuxWithConfig
            ql2nix
          ] ++
          (with easy-purescript-nix; [ psc-package purescript ]);
      })

