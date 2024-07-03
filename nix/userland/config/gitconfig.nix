{ config, pkgs, specialArgs, ... }:

{
  programs.git = {
    enable = true;
    package = pkgs.gitAndTools.gitFull;


    aliases = {
      br = ''branch'';
      cb = ''"!echo \"$(git rev-parse --abbrev-ref HEAD 2>/dev/null)\""'';
      ci = ''commit'';
      co = ''checkout'';
      df = ''diff'';
      go = ''"!f() { git checkout -b \"$1\" 2> /dev/null || git checkout \"$1\"; }; f"'';
      l = ''log --relative-date --graph --abbrev-commit -10'';
      ll = ''log --relative-date --graph --abbrev-commit'';
      lg = ''log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative'';
      lola = ''log --graph --decorate --pretty=oneline --abbrev-commit --all'';
      lol = ''log --graph --decorate --pretty=oneline --abbrev-commit'';
      ls = ''ls-files'';
      pb = ''"!git push origin \"$(git rev-parse --abbrev-ref HEAD)\""'';
      pr = ''"!open \"$(git remote -v | grep origin | grep push | cut -f 2 | cut -d \" \" -f 1 | sed -e \"s|git@\\(.*\\):\\(.*\\).git|https://\\1/\\2|\")/pull/new/$(git rev-parse --abbrev-ref HEAD)\""'';
      st = ''status'';
      unstage = ''reset HEAD --'';
      up = ''"!git pull origin \"$(git rev-parse --abbrev-ref HEAD)\""'';
    };
    diff-so-fancy.enable = true;
    merge.conflictstyle = "diff3";
    extraConfig = {
      core = {
        whitespace = "trailing-space,space-before-tab";
        editor = "nvim";
      };
      color.ui = true;
      pull.rebase = true;
      format.pretty = "format:%C(yellow)%h %Cblue%>(12)%ad %Cgreen%<(7)%aN%Cred%d %Creset%s";
      help.autocorrect = 1;
      init.defaultBranch = "main";

    };
    signing.key = "3264D1648F8FB1A4A3F74C9318497AC961BB2FB6";
    userEmail = "paulvictor@gmail.com";
    userName = "Paul Victor Raj";
    lfs.enable = true;

    includes = [
      {
        condition = "gitdir:Juspay/";
        contents = {
          user = {
            email = "paul.victor@juspay.in";
          };
        };
      }
    ];

  };
}


