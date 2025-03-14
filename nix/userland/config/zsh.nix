{ pkgs, config }:

with pkgs;
let
  nix-shell-spaceship-fn = ''
    SPACESHIP_NIX_SHELL_SHOW="''${SPACESHIP_NIX_SHELL_SHOW=true}"
    SPACESHIP_NIX_SHELL_PREFIX="''${SPACESHIP_NIX_SHELL_PREFIX="$SPACESHIP_PROMPT_DEFAULT_PREFIX"}"
    SPACESHIP_NIX_SHELL_SUFFIX="''${SPACESHIP_NIX_SHELL_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"}"
    SPACESHIP_NIX_SHELL_COLOR="''${SPACESHIP_NIX_SHELL_COLOR="51"}"

    spaceship_nixShell() {
      # If SPACESHIP_NIX_SHELL_SHOW is false, don't show nix-shell section
      [[ $SPACESHIP_NIX_SHELL_SHOW == false || -z $IN_NIX_SHELL ]] && return

      # Display foobar section
      spaceship::section \
        "$SPACESHIP_NIX_SHELL_COLOR" \
        "$SPACESHIP_NIX_SHELL_PREFIX" \
        "$SPACESHIP_NIX_SHELL_SYMBOL" \
        "$SPACESHIP_NIX_SHELL_SUFFIX"
    }
  '';
  manPager =
    runCommand "lezz" {buildInputs = [ makeWrapper ];} ''
      mkdir -pv $out/bin
      makeWrapper ${less}/bin/less $out/bin/lezz \
        --set LESS_TERMCAP_mb $(printf '\e[01;31m') \
        --set LESS_TERMCAP_md $(printf '\e[01;35m') \
        --set LESS_TERMCAP_me $(printf '\e[0m')  \
        --set LESS_TERMCAP_se $(printf '\e[0m')  \
        --set LESS_TERMCAP_so $(printf '\e[01;33m')  \
        --set LESS_TERMCAP_ue $(printf '\e[0m')  \
        --set LESS_TERMCAP_us $(printf '\e[04;36m')
    '';
  pl-zsh-nix-shell = {
    name = "zsh-nix-shell";
    file = "nix-shell.plugin.zsh";
    src = pkgs.zsh-nix-shell;
  };
  pl-nix-zsh-completions = {
    name = "nix-zsh-completions";
    file = "nix-zsh-completions.plugin.zsh";
    src = pkgs.nix-zsh-completions;
  };
  pl-zsh-completions = {
    name = "zsh-completions";
    file = "zsh-completions.plugin.zsh";
    src = pkgs.zsh-completions;
  };
in
{
  enable = true;
  autosuggestion.enable = true;
  dotDir = ".zsh";
  syntaxHighlighting.enable = true;
  history = {
    path = "${config.home.homeDirectory}/plain/zsh/zsh_history";
    expireDuplicatesFirst = true;
    extended = false;
    ignoreDups = true;
    save = 100000;
    share = true;
    size = 100000;
  };
  plugins = [
    pl-zsh-nix-shell
    pl-zsh-completions
    pl-nix-zsh-completions
  ];


  initExtra = ''
    [[ $TERM == "dumb" ]] \
      && unsetopt zle \
      && unsetopt prompt_cr \
      && unsetopt prompt_subst \
      && unfunction precmd \
      && unfunction preexec \
      && PS1='$ ' \
      && return
    [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"
    export LESS="-QR"
    setopt interactivecomments
    setopt autocd
    setopt cdablevars
    unsetopt correct_all
    setopt incappendhistory
    export PAGER=less
    setopt nolistambiguous
    unsetopt EXTENDED_HISTORY
    export LANG=en_US.UTF-8
    export LC_COLLATE="en_US.UTF-8"
    export LC_CTYPE="en_US.UTF-8"
    export LC_MESSAGES="en_US.UTF-8"
    export LC_MONETARY="en_US.UTF-8"
    export LC_NUMERIC="en_US.UTF-8"
    export LC_TIME="en_US.UTF-8"
    export LC_ALL="en_US.UTF-8"
    function new-tmux-from-dir-name {
      dir_name=$(echo `basename $PWD` | tr '.' '-')
      tmux new-session -As $dir_name
    }
    # Better SSH/Rsync/SCP Autocomplete
    zstyle ':completion:*:(scp|rsync):*' tag-order ' hosts:-ipaddr:ip\ address hosts:-host:host files'
    zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
    zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'

    # Allow for autocomplete to be case insensitive
    zstyle ':completion:*' matcher-list ' ' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|?=** r:|?=**'

    autoload -U compinit && compinit
    zstyle ":completion:*:commands" rehash 1
    SPACESHIP_CHAR_SYMBOL="*> "
    SPACESHIP_PROMPT_ADD_NEWLINE=false
    SPACESHIP_EXIT_CODE_SHOW=true
    SPACESHIP_EXIT_CODE_PREFIX="["
    SPACESHIP_EXIT_CODE_SUFFIX="]"

    [[ $TERM = "rxvt-unicode-256color" ]] &&
      (for (( i=1; i<=$LINES; i++ )); do echo; done; clear)

    export EDITOR="emacsclient -c"
    #eval "$(${lua}/bin/lua ${z-lua}/bin/z --init zsh enhanced once fzf)"
    _ZL_ECHO=1
    _ZL_MATCH_MODE=1

    function qqbc() { echo "scale=''${2:-2}; $1" | bc -l }

    function gen-passwd () { ${pkgs.gnupg}/bin/gpg --gen-random --armor 0 $1:-24 }
    source ${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh
    macsman() {
      emacsclient -c -n --eval "(let ((Man-notify-method 'bully)) (man \"$1\"))"
    }
  '';
  shellAliases = {
    # grep = "GREP_COLORS=\"1;33;40\" LANG=C egrep --color=always";
    gen_new_cert = "${pkgs.openssl}/bin/openssl req -new -x509 -key ~/.ssh/id_rsa -out cacert.pem -days 1095";
    ee = "emacsclient -c -n $1";
    man = "macsman";
#     nextp = "mpc next";
#     prevp = "mpc prev";
#     tnew = "new-tmux-from-dir-name";
    cat = "bat";
    ping = "prettyping";
    ssh = "TERM=xterm-color ssh";
    screen-grab = "ffmpeg -f x11grab -video_size 1920x1080 -framerate 30 -i :1 -vcodec libx264 -preset ultrafast -qp 0 -pix_fmt yuv444p video.mkv";
  };
  oh-my-zsh = {
    enable = true;
    plugins = [ "git" ];
    theme = "spaceship";
    custom = "$HOME/.config/zsh";
  };

}
