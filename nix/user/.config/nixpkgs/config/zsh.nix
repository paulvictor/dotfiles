{ pkgs }:

with pkgs;
let
  zsh-nix-shell = fetchFromGitHub {
    "owner" = "chisui";
    "repo" = "zsh-nix-shell";
    "rev" = "5dd73237d598563e03ea8e84ff9deb6a6ed70848";
    "sha256" = "0zkb1yf52r2bz39wjzybdaaa33lz6lhhlj7khcwrhlc6p47hwp6n";
  };
  nix-shell-spaceship-fn = ''
    SPACESHIP_NIX_SHELL_SHOW="''${SPACESHIP_NIX_SHELL_SHOW=true}"
    SPACESHIP_NIX_SHELL_PREFIX="''${SPACESHIP_NIX_SHELL_PREFIX="$SPACESHIP_PROMPT_DEFAULT_PREFIX"}"
    SPACESHIP_NIX_SHELL_SUFFIX="''${SPACESHIP_NIX_SHELL_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"}"
    SPACESHIP_NIX_SHELL_SYMBOL="''${SPACESHIP_NIX_SHELL_SYMBOL=" "}"
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
in {
  enable = true;
  enableAutosuggestions = true;
  dotDir = ".zsh";
  history = {
    expireDuplicatesFirst = true;
    extended = true;
    ignoreDups = true;
    save = 100000;
    share = true;
    size = 100000;
  };
  initExtra = ''
    export MANPAGER="nvim -c 'set ft=man' -"
    export LESS="-QR"
    setopt interactivecomments
    setopt autocd
    setopt cdablevars
    unsetopt correct_all
    export PAGER=less
    setopt nolistambiguous
    conf() { [ "$1" != "" ] && cp "$1" "$1".bak-`date +%d%m%y`; vim "$1"; }
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
    autoload -U compinit && compinit
    zstyle ":completion:*:commands" rehash 1
    export NIX_PATH=easy-purescript-nix=https://github.com/justinwoo/easy-purescript-nix/tarball/master:$NIX_PATH
    SPACESHIP_NIX_SHELL_PREFIX="in "
    #SPACESHIP_NIX_SHELL_SUFFIX=""
    SPACESHIP_CHAR_SYMBOL="*> "
    SPACESHIP_PROMPT_ADD_NEWLINE=false
    SPACESHIP_EXIT_CODE_SHOW=true
    SPACESHIP_EXIT_CODE_PREFIX="["
    SPACESHIP_EXIT_CODE_SUFFIX="]"
    ${nix-shell-spaceship-fn}
    SPACESHIP_PROMPT_ORDER=(
      dir
      git
      exec_time
      exit_code
      line_sep
      char
    )
    SPACESHIP_RPROMPT_ORDER=( nixShell )


    [[ $TERM = "rxvt-unicode-256color" ]] &&
      (for (( i=1; i<=$LINES; i++ )); do echo; done; clear)

    export EDITOR=vim

    source ${zsh-nix-shell.out}/nix-shell.plugin.zsh
  '';
  shellAliases = {
    grep = "GREP_COLOR=\"1;33;40\" LANG=C egrep --color=always";
    gen_new_cert = "'openssl req -new -x509 -key ~/.ssh/id_rsa -out cacert.pem -days 1095'";
    tmux = "TERM=screen-256color-bce tmux -u -S /tmp/default";
    ls = "ls --color=auto";
    nextp = "mpc next";
    prevp = "mpc prev";
    tnew = "new-tmux-from-dir-name";
    cat = "bat";
    ping = "prettyping";
    ssh = "'TERM=xterm-color ssh'";
  };
  oh-my-zsh = {
    enable = true;
    plugins = [ "git" "stack" "zsh-completions" "fzf" ];
    theme = "spaceship";
    custom = "$HOME/.config/zsh";
  };
}
