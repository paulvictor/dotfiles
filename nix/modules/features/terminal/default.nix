# Terminal emulators
{
  flake.modules.homeManager.terminal = { pkgs, lib, ... }: {
  programs.alacritty = {
    enable = true;
    settings = {
      window.startup_mode = "Maximized";
      window.decorations = "none";
      window.title = "Alacritty";
      scrolling.multiplier = 10;
      font.normal.family = "VictorMono Nerd Font Mono";
      font.normal.style = "SemiBold";
      font.bold.family = "VictorMono Nerd Font Mono";
      font.bold.style = "Bold";
      font.italic.family = "VictorMono Nerd Font Mono";
      font.italic.style = "SemiBold Italic";
      font.bold_italic.family = "VictorMono Nerd Font Mono";
      font.bold_italic.style = "Bold Italic";
      font.size = lib.mkDefault 12.0;
      bell.animation = "EaseOutSine";
      selection.save_to_clipboard = true;
      cursor.style.shape = "Block";
      cursor.style.blinking = "Always";
      cursor.blink_interval = 500;
      cursor.blink_timeout = 10;
      cursor.vi_mode_style.shape = "Beam";
      terminal.shell = "${lib.getExe pkgs.zsh}";
      keyboard.bindings = [
        { key = "V"; mods = "Control|Shift"; action = "Paste"; }
        { key = "C"; mods = "Control|Shift"; action = "Copy"; }
        { key = "Space"; mods = "Control"; action = "ToggleViMode"; }
        { key = "I"; mode = "Vi"; action = "ToggleViMode"; }
        { key = "Escape"; mode = "Vi"; action = "ClearSelection"; }
        { key = "H"; mode = "Vi"; action = "Left"; }
        { key = "J"; mode = "Vi"; action = "Down"; }
        { key = "K"; mode = "Vi"; action = "Up"; }
        { key = "L"; mode = "Vi"; action = "Right"; }
        { key = "Y"; mode = "Vi"; action = "Copy"; }
        { key = "Y"; mode = "Vi"; action = "ClearSelection"; }
        { key = "B"; mods = "Shift"; mode = "Vi"; action = "ScrollToBottom"; }
        { key = "B"; mode = "Vi"; action = "ScrollToTop"; }
        { key = "B"; mods = "Control"; mode = "Vi"; action = "ScrollPageUp"; }
        { key = "F"; mods = "Control"; mode = "Vi"; action = "ScrollPageDown"; }
        { key = "U"; mods = "Control"; mode = "Vi"; action = "ScrollHalfPageUp"; }
        { key = "D"; mods = "Control"; mode = "Vi"; action = "ScrollHalfPageDown"; }
        { key = "Slash"; mods = "Shift"; mode = "Vi"; action = "SearchBackward"; }
        { key = "Slash"; mode = "Vi"; action = "SearchForward"; }
        { key = "N"; mods = "Shift"; mode = "Vi"; action = "SearchPrevious"; }
        { key = "N"; mode = "Vi"; action = "SearchNext"; }
      ];
      colors.draw_bold_text_with_bright_colors = true;
      window.opacity = 0.8;
      colors = {
        primary = {
          background = "0x181818";
          foreground = "0xd8d8d8";
        };
        cursor = {
          text = "0xd8d8d8";
          cursor = "0xd8d8d8";
        };
        normal = {
          black =   "0x181818";
          red =     "0xab4642";
          green =   "0xa1b56c";
          yellow =  "0xf7ca88";
          blue =    "0x7cafc2";
          magenta = "0xba8baf";
          cyan =    "0x86c1b9";
          white =   "0xd8d8d8";
        };
        bright = {
          black =   "0x585858";
          red =     "0xab4642";
          green =   "0xa1b56c";
          yellow =  "0xf7ca88";
          blue =    "0x7cafc2";
          magenta = "0xba8baf";
          cyan =    "0x86c1b9";
          white =   "0xf8f8f8";
        };
      };
    };
  };
    xdg.configFile."kitty/kitty.conf".source = import ./_kitty.nix { inherit pkgs; };
  };
}
