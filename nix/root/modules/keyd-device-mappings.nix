let
  global = {
    overload_tap_timeout = 200;
    chord_timeout = 100;
    layer_indicator = true;
  };
  main = {
    capslock = "layer(meta)";
    enter = "overload(control, enter)";
    leftshift = "overload(shift, esc)";
    space = "overload(alt,space)";
    leftcontrol = "overload(control, C-x)";
    rightcontrol = "overload(control, C-c)";
    esc = "toggle(base)";
  };
  commonBindings = {
    "c" = "overload(numbers, space)";  "m" = "overload(controlAlt,enter)";
    "v" = "overload(symbols, tab)";    "," = "backspace";

    "i+o" = "escape";
    "v+m" = "semicolon";
    "c+," = ":";
  };
  base = {
    esc = "toggle(main)";
    "1" = "-";                             "7" = "z";
    "2" = "w";                             "8" = "u";
    "3" = "f";                             "9" = "y";
    "4" = "p";                             "0" = "q";
    "5" = "b";                             "-" = "'";

    q = "overload(meta, a)";         u = "m";
    w = "overload(alt, r)";          i = "overload(shift, n)";
    e = "s";                         o = "e";
    r = "overload(shift, t)";        p = "overload(alt, i)";
    t = "g";                         "leftbrace" = "overload(control, o)";

    a = "overload(control, C-x)";     j = ".";
    s = "x";                          k = "h";
    d = "c";                          l = "j";
    f = "d";                          ";" = "k";
    g = "v";                          "'" = "l";
  } // commonBindings;
  numbers = {
    "1" = "noop";                             "7" = "`";
    "2" = "slash";                            "8" = "4";
    "3" = "backslash";                        "9" = "5";
    "4" = "|";                                "0" = "6";
    "5" = "noop";                             "-" = "+";

    q = "<";        u = "?";
    w = "(";        i = "1";
    e = ")";        o = "2";
    r = ">";        p = "3";
    t = "noop";    "leftbrace" = "0";

    a = "{";             j = ".";
    s = "leftbrace";     k = "7";
    d = "rightbrace";    l = "8";
    f = "}";        ";" = "9";
    g = "noop";     "'" = "=";
  } // commonBindings;
  symbols = {
    "7" = "~";
    "8" = "$";
    "9" = "%";
    "0" = "^";
    "-" = "noop";

    u = "&";
    i = "!";
    o = "@";
    p = "#";
    "[" = "*";

    j = "noop";
    k = "left";
    l = "down";
    ";" = "up";
    "'" = "right";
  } // commonBindings;

in {
  uriel.builtin = {
    ids = ["00001:0001"];
    settings = {
      inherit base global main numbers symbols;#  toBase;
#      "controlAlt:C-A" = "controlAlt:C-A";
    };
    extraConfig = ''
      [controlAlt:C-A]
    '';
  };

}
