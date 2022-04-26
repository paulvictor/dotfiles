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
        rev = "d56c436a66ec2a8a93b309c83693cef1507dca7a";
        sha256 = "T96xGZV2AEP07smv/L2s5U7jY1LTdJEiTnA90gJ3Fco=";
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
            nodePackages.purescript-language-server
            pv
            ql2nix
            ruby
            sbcl
            silver-searcher
            tmate
            tmuxWithConfig
          ] ++
          (with easy-purescript-nix; [ psc-package purescript spago pscid spago2nix pulp ]);
      })

