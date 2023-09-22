{ xclip }:
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
  "Xft.hintstyle" = "hintnone";
  "Xft.lcdfilter" = "lcddefault";
  "Xft.rgba" = "rgb";

  "URxvt.perl-ext-common" = "default,matcher,tabbedex,keyboard-select,autocomplete-ALL-the-things,resize-font,-confirm-paste";

  "URxvt.font" =
    "xft:IosevkaTerm\ Nerd\ Font\ Mono:antialias=true:size=13:style=Regular:hinting=true:minspace=False,xft:Symbols\ Nerd\ Font:style=Regular:size=13:antialias=true:hinting=true";
  "URxvt.boldFont" =
    "xft:IosevkaTerm\ Nerd\ Font\ Mono:antialias=true:hinting=true:style=Bold:size=13:minspace=False,xft:Symbols\ Nerd\ Font:style=Regular:size=13:antialias=true:hinting=true";
  "URxvt.italicFont" =
    "xft:IosevkaTerm\ Nerd\ Font\ Mono:antialias=true:hinting=true:style=Italic:size=13:minspace=False,xft:Symbols\ Nerd\ Font:style=Regular:size=13:antialias=true:hinting=true";
  "URxvt.boldItalicFont" =
    "xft:IosevkaTerm\ Nerd\ Font\ Mono:antialias=true:hinting=true:style=Bold\ Italic:size=13:minspace=False,xft:Symbols\ Nerd\ Font:style=Regular:size=13:antialias=true:hinting=true";
  "rofi.font" = "Hack Nerd Font Mono 11";

  "URxvt*termName"  = "rxvt-unicode-256color";
  "URxvt.depth" = 32;
  "URxvt.letterSpace" = 0;
  "URxvt.colorUL" = "#4682B4";
  "URxvt.saveLines" = 1000000;
  "URxvt.secondaryScroll" = true;

  #manage font size;
  "URxvt.keysym.C-minus" = "resize-font:smaller";
  "URxvt.keysym.C-plus" = "resize-font:bigger";
  "URxvt.keysym.C-equal" = "resize-font:bigger";
  "URxvt.keysym.C-0" = "resize-font:reset";

  "URxvt.keysym.Control-space" = "perl:keyboard-select:activate";
  "URxvt.keysym.Shift-Escape" = "perl:keyboard-select:activate";
  "URxvt.keyboard-select.clipboard" = true;

  "URxvt.scrollBar" = false;

  "URxvt.matcher.button" = 1;

  "URxvt.cursorBlink" = true;
  "!URxvt.cursorUnderline" = true;

  "URxvt*scrollWithBuffer" = false;
  "URxvt.intensityStyles" = false;
  "URxvt*visualBell" = "true            ! blink window on bell";
  "URxvt.keysym.Control-Tab" =  "perl:aAtt:word-complete";
  "URxvt.keysym.M-Tab" = "perl:aAtt:fuzzy-complete";
  "URxvt.keysym.C-quotedbl" =  "perl:aAtt:undo";

  "URxvt.clipboard.autocopy" = true;
}
