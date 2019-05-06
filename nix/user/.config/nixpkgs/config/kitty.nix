{ pkgs }:

with pkgs;
writeText "kitty.conf" ''
  font_family      FuraCode Nerd Font
  #font_family      Fira Code
  bold_font        auto
  italic_font      auto
  bold_italic_font auto
  font_size        12.0
  font_size_delta 2
  scrollback_lines 20000
  scrollback_in_new_tab no

  scrollback_pager less +G  --RAW-CONTROL-CHARS +INPUT_LINE_NUMBER
  wheel_scroll_multiplier 5.0

  url_color #0087BD
  url_style curly
  open_url_modifiers ctrl+shift
  open_url_with default
  copy_on_select yes
  rectangle_select_modifiers ctrl+v
  select_by_word_characters :@-./_~?&=%+#
  click_interval -1.0
  visual_bell_duration 1.0
  window_alert_on_bell yes

  # Change the sizes of the lines used for the box drawing unicode characters
  # These values are in pts. They will be scaled by the monitor DPI to arrive at
  # a pixel value. There must be four values corresponding to thin, normal, thick,
  # and very thick lines;
  box_drawing_scale 0.001, 1, 1.5, 2

  # The foreground color
  foreground       #839496

  # The background color
  background       #002b36

  background_opacity 0.0

  # The foreground for selections
  selection_foreground #002b36

  # The background for selections
  selection_background #586e75
  # The cursor color
  cursor           #93a1a1
  enabled_layouts *
  # Tab-bar customization
  active_tab_foreground #d3d4c4
  active_tab_background #404552
  active_tab_font_style normal
  inactive_tab_foreground #7c838f
  inactive_tab_background #383c4a
  inactive_tab_font_style normal
  tab_separator " ┇ "
  # solarized dark
  color0   #073642
  color8   #002b36

  color1   #dc322f
  color9   #cb4b16

  color2   #859900
  color10  #586e75

  color3   #b58900
  color11  #657b83

  color4   #268bd2
  color12  #839496

  color5   #d33682
  color13  #6c71c4

  color6   #2aa198
  color14  #93a1a1

  color7   #eee8d5
  color15  #fdf6e3

  map ctrl+shift+v        paste_from_clipboard
  map ctrl+shift+s        no_op
  map ctrl+shift+c        copy_to_clipboard
  map shift+insert        no_op
  map ctrl+equal          increase_font_size
  map ctrl+minus          decrease_font_size
  prefer_color_emoji yes
''
