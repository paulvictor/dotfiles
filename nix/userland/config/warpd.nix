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
      buttons: 1 2 3
      hint: x
      hint2: X
      grid: g
      grid_exit: esc
      hint_chars: neiohjkluyq
      hint2_chars: uyqneihjk
      screen_chars: hljk
      oneshot_buttons: unbind

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

      grid_cut_up: I
      grid_cut_down: E
      grid_cut_left: N
      grid_cut_right: O
      grid_up: i
      grid_down: e
      grid_left: n
      grid_right: o

      indicator: topright
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
