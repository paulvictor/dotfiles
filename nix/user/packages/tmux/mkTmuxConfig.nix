{ pkgs } :

with pkgs;
let
  themePack = fetchFromGitHub {
    owner = "jimeh";
    repo = "tmux-themepack";
    rev = "7c59902f64dcd7ea356e891274b21144d1ea5948";
    sha256 = "1kl93d0b28f4gn1knvbb248xw4vzb0f14hma9kba3blwn830d4bk";
    fetchSubmodules = true;
  };
in
writeText "tmux.conf" ''
set-window-option -g mode-keys vi
set -g terminal-overrides 'xterm:colors=256'
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R
bind-key -rn C-h select-window -t :-
bind-key -rn C-l select-window -t :+

# super useful when using "grouped sessions" and multi-monitor setup
setw -g aggressive-resize on

#https://github.com/tmux/tmux/issues/1246
set -g default-terminal "xterm-256color"
set -ga terminal-overrides ",*256col*:Tc"

set-option -g status-interval 1
set -g focus-events on

set -g default-shell ${zsh}/bin/zsh

set-option -g prefix `
# set-option -g prefix C-a

unbind-key C-b
bind-key C-a last-window
bind-key ` last-window
bind-key a send-prefix

###################################################

# 0 is too far from ` ;)
set -g base-index 1

set-option -g status-keys vi
set-option -g set-titles-string 'tmux #D #W #S' # window number,program name,active (or not)

setw -g mode-keys vi
setw -g monitor-activity on

set -g history-limit 100000

set -sg escape-time 0
# For copy-paste
unbind [
bind Escape copy-mode
unbind p
bind p paste-buffer
bind-key -Tcopy-mode-vi 'v' send -X begin-selection
bind-key -Tcopy-mode-vi 'y' send -X copy-pipe-and-cancel "${xclip}/bin/xclip -i -selection p -f | ${xclip}/bin/xclip -i -selection c"
bind-key -Tcopy-mode-vi Escape send -X cancel
bind-key -Tcopy-mode-vi V send -X rectangle-toggle

# hsplit
unbind %
bind-key '\' split-window -h

# vsplit
unbind '"'
bind-key - split-window -v

# Resize
unbind C-Right
unbind C-Left
unbind C-Up
unbind C-Down
bind-key -rn C-Up resize-pane -U
bind-key -rn C-Down resize-pane -D
bind-key -rn C-Left resize-pane -L
bind-key -rn C-Right resize-pane -R

unbind '='


source ${themePack}/powerline/double/cyan.tmuxtheme

${
  let
    plugins = with tmuxPlugins; [ extrakto fpp open tmux-fzf resurrect continuum sessionist sidebar ];
  in lib.concatMapStringsSep "\n" (p: "run-shell ${p.rtp}") plugins
}

set -g @continuum-boot 'on'

set -g @extrakto_split_direction 'p'

set -g @sidebar-tree-command '${tree}/bin/tree -C'
set -g @sidebar-tree '='

TMUX_FZF_LAUNCH_KEY="F"
''
