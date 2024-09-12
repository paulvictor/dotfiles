{pkgs, config, lib, ...}:
let
  inherit (pkgs) warpd;
in
{
  home.packages = [ warpd ];

  # take inspiration from https://gist.github.com/joe-scotto/f0922fcacf73256dafa5adda98290a97
  xdg.configFile."warpd/config" = {
    enable = true;
    text = ''
      buttons: unbind
      oneshot_buttons: S-backspace M O
      buttons: backspace m o

      exit: C-g
      hint_exit: esc
      grid_exit: esc
      drag: v

      hint: colon
      hint2: Return
      grid: Tab
      screen: space
      hint_chars: naerisot
      hint2_chars: uyqneihjk
      screen_chars: arst

      scroll_down: unbind
      scroll_down: J
      scroll_up: unbind
      scroll_up: K

      grid_cut_up: unbind
      grid_cut_down: unbind
      grid_cut_left: unbind
      grid_cut_right: unbind
      grid_up: unbind
      grid_down: unbind
      grid_left: unbind
      grid_right: unbind
      grid_border_color: #00ff00
      grid_border_size: 2

      grid_keys: unbind
      grid_cut_up: k
      grid_cut_down: j
      grid_cut_left: h
      grid_cut_right: l
      grid_up: K
      grid_down: J
      grid_right: L
      grid_left: H

      indicator: topright
      indicator_color: #ffff55
      indicator_size: 25
      normal_system_cursor: 1
    '';
  };

}


# top: N
#       bottom: I
#       middle: E

  # up: i
#       down: e
#       left: n
  #       right: o
  # oneshot_buttons: S-backspace M O
