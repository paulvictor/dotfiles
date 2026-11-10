{
  global = {
    font                 = "SauceCodePro Nerd Font Mono 10";
    format               = "%s\\n%b";
    transparency         = "40";
    ignore_newline       = "no";
    show_indicators      = "yes";
    separator_color      = "frame";
    frame_color = "#8caaee";
    sort                 = "yes";
    enable_posix_regex = "yes";
    alignment            = "left";

    word_wrap            = "yes";
    indicate_hidden      = "yes";
    show_age_threshold   = "60";
    idle_threshold       = "120";

    width = "(0, 300)";
    height = "100";
    offset = "50x30";
    origin = "top-right";
    corner_radius = "6";

    shrink               = "no";
    line_height          = "0";
#     notification_height  = "100";
    separator_height     = "2";
    padding              = "16";
    horizontal_padding   = "12";
    monitor              = "0";
    follow               = "none";
    sticky_history       = "yes";
    history_length       = "20";
    icon_position        = "left";
#     startup_notification = "false";
  };

  urgency_low = {
    background = "#303446";
    foreground = "#c6d0f5";
    timeout = 10;
  };

  urgency_normal = {
    background = "#303446";
    foreground = "#c6d0f5";
    timeout = 5;
  };

  urgency_critical = {
    background = "#303446";
    foreground = "#c6d0f5";
    frame_color = "#ef9f76";
    timeout = 0;
  };
  # the v_ suffix makes sure it is the last part of the list
  v_sway-keymap = {
    background = "#eff1f5";
    foreground = "#4c4f69";
    category = ".*sway.keymap.*";
  };
}
