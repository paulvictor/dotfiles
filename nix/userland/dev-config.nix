{ config, pkgs, lib, specialArgs, ... }:

with pkgs;
let
  inherit (pkg.stdenv) system isLinux;
in
  {
    home.file.".ghc/ghci.conf".text = ''
      :def hoogle \s -> return $ ":! hoogle search --color --count=15 \"" ++ s ++ "\""
      :def doc \s -> return $ ":! hoogle search --color --info \"" ++ s ++ "\""
    '';
    imports = [
      ./config/gitconfig.nix
    ];
    programs.fzf = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.readline = {
      enable = true;
      extraConfig = ''
        # Color the common prefix
        set colored-completion-prefix On
        # Color the common prefix in menu-complete
        set menu-complete-display-prefix On
      '';
      variables = {
        "completion-ignore-case" = true;
        "completion-prefix-display-length" = 3;
        "mark-symlinked-directories" = true;
        "show-all-if-ambiguous" = true;
        "show-all-if-unmodified" = true;
        "visible-stats" = true;
        "enable-bracketed-paste" = false;
      };
      bindings = {
        "\\C-p" = "history-search-backward";
        "\\C-n" = "history-search-forward";
      };
    };
    home.packages =
      [
        axel
        cachix
        entr
        gnumake
        guile_3_0
        guile_3_0.dev
        guile_3_0.info
        jq
        nodejs
        pv
        ruby
        sbcl
        tmate
        chez
        pscid
        leiningen clojure
        specialArgs.inputs.magix.packages.${system}.magix
#         visidata # for visualizing data
      ] ++
      (lib.optionals isLinux [ bindfs k ]);
  }

