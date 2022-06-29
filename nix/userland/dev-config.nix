{ config, pkgs, lib, specialArgs, ... }:

with pkgs;
lib.mkIf
  specialArgs.isDevEnv
  (
    let
      tmuxWithConfig = pkgs.callPackage ./packages/tmux {};

      easy-purescript-nix-repo =  pkgs.fetchFromGitHub {
        owner = "justinwoo";
        repo = "easy-purescript-nix";
        rev = "0ad5775c1e80cdd952527db2da969982e39ff592";
        sha256 = "bwbpXSTD8Hf7tlCXfZuLfo2QivvX1ZDJ1PijXXRTo3Q=";
      };
      easy-purescript-nix = import easy-purescript-nix-repo { inherit pkgs; };
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
            axel
            bind
            bindfs
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
            nodejs
            pv
            ql2nix
            ruby
            sbcl
            silver-searcher
            tmate
            tmuxWithConfig
            pscid
            k # https://github.com/nathyong/ngnk-nix/blob/master/flake.nix && https://codeberg.org/ngn/k
          ] ++
          (with easy-purescript-nix; [ purs-0_14_7 spago spago2nix purescript-language-server ]);
      })

