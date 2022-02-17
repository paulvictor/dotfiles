let
  urxvt-vim-scrollback = builtins.fetchurl {
    url = https://github.com/bechampion/urxvt-vim-scrollback/raw/master/vim-scrollback;
    sha256 = "0dvzk2j14p2fw4rw6psv4c1f7qkwh9fw5mf3lflljz3cy2fi4a85";
  };
in
{
  "Xft.antialias" = true;
  "Xft.autohint" = false;
  "Xft.dpi" = 96;
  "Xft.hinting" = true;
  "Xft.hintstyle" = "hintslight";
  "Xft.lcdfilter" = "lcddefault";
  "Xft.rgba" = "rgb";

  "URxvt*font" = "xft:FuraCode\\ Nerd\\ Font:antialias=true:hinting=true:style=Regular:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  "URxvt*boldFont" = "xft:FuraCode\\ Nerd\\ Font:antialias=true:hinting=true:style=Bold:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  #"URxvt*italicFont" = "xft:FuraCode\\ Nerd\\ Font:antialias=true:hinting=true:style=Medium\\ Italic:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  #"URxvt*boldItalicFont" = "xft:FuraCode\\ Nerd\\ Font:antialias=true:hinting=true:style=Bold\\ Italic:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  #"URxvt*font" = "xft:SauceCodePro\\ Nerd\\ Font\\ Mono:antialias=true:hinting=true:style=Regular:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  #"URxvt*boldFont" = "xft:SauceCodePro\\ Nerd\\ Font\\ Mono:antialias=true:hinting=true:style=Bold:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  #"URxvt*italicFont" = "xft:SauceCodePro\\ Nerd\\ Font\\ Mono:antialias=true:hinting=true:style=Medium\\ Italic:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  #"URxvt*boldItalicFont" = "xft:SauceCodePro\\ Nerd\\ Font\\ Mono:antialias=true:hinting=true:style=Bold\\ Italic:size=12:minspace=False, xft:DejaVu Sans:pixelsize=12";
  "rofi.font" = "Hack Nerd Font Mono 11";

  "URxvt*termName"  = "rxvt-unicode-256color";
  "URxvt.depth" = 32;
  "URxvt.letterSpace" = 0;
  "URxvt.colorUL" = "#4682B4";
  #"URxvt.tabbed.saveLines" = 10000;
  "URxvt.saveLines" = 10000;
  "URxvt.secondaryScroll" = true;
  "URxvt.keysym.Control-space" = "perl:keyboard-select:activate";
  "URxvt.keysym.Shift-Escape" = "perl:keyboard-select:activate";
  "URxvt.keyboard-select.clipboard" = true;

  "URxvt.scrollBar" = false;
  "URxvt.perl-ext-common" = "default,matcher,tabbedex,keyboard-select,autocomplete-ALL-the-things,selection-to-clipbard";
  "URxvt.perl-ext-blacklist" = "tabs";
  #"URxvt.tabbed.new-button" = true;
  #"URxvt.tabbed.autohide" = true;
  "URxvt.url-launcher" = "vivaldi";
  #! https://github.com/muennich/urxvt-perls/issues/34
  "URxvt.matcher.pattern.0" = "(?:https?:/ /|ftp:/ /|news:/ /|mailto:|file:/ /|\\bwww\\.) [\\w\\-\\@;\\/?:&=%\\$.+!*\\x27,~#]* ( \\([\\w\\-\\@;\\/?:&=%\\$.+!*\\x27,~#]*\\) | [\\w\\-\\@;\\/?:&=%\\$+*~])+";
  "URxvt.matcher.pattern.1" = "[a-f0-9]{7,10}\\.\\.[a-f0-9]{7,10}";

  "URxvt.matcher.button" = 1;
  #"URxvt.tabbed.tabbar-fg" = 3;
  #"URxvt.tabbed.tabbar-bg" = 0;
  #"URxvt.tabbed.tab-fg" = 0;
  #"URxvt.tabbed.tab-bg" = 1;

  "URxvt.cursorBlink" = true;
  "!URxvt.cursorUnderline" = true;

  #manage font size;
  "URxvt.keysym.C-S-plus" = "font-size:increase";
  "URxvt.keysym.C-KP_Add" = "font-size:increase";
  "URxvt.keysym.C-underscore" = "font-size:decrease";
  "URxvt.keysym.C-KP_Subtract" = "font-size:decrease";
  "URxvt*scrollWithBuffer" = false;
  # color scheme which works
  "URxvt*background" = "#002b36";
  "URxvt*foreground" = "#839496";
  "URxvt*transparent" = true;
  "URxvt.intensityStyles" = false;
  "URxvt*visualBell" = "true            ! blink window on bell";
}
