# Development tools: git, language tooling, AI coding agents
{ inputs, ... }: {
  allowedUnfree = [ "claude-code" "consul" "nomad" "terraform" ];

  flake.modules.homeManager.dev = { config, pkgs, lib, ... }: {
    imports = [
      inputs.nixpi.homeModules.default
      inputs.pi-packages.homeModules.default
      ./_gitconfig.nix
      ./_ai-tools.nix
    ];

    home.file.".ghc/ghci.conf".text = ''
      :def hoogle \s -> return $ ":! hoogle search --color --count=15 \"" ++ s ++ "\""
      :def doc \s -> return $ ":! hoogle search --color --info \"" ++ s ++ "\""
    '';

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

    home.packages = with pkgs; [
      awscli2
      oci-cli
      axel
      cachix
      entr
      gnumake
      guile_3_0
      guile_3_0.dev
      guile_3_0.info
      jq
      pv
      sbcl
      sbclPackages.qlot-cli
      chez
      cbqn
      inputs.magix.packages.${pkgs.stdenv.hostPlatform.system}.magix
    ];
  };
}
