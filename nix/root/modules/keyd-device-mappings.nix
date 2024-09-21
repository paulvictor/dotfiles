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
    "k+l" = "toggle(capsWord)";
  };

  base = {
    esc = "setlayout(main)";
    "1" = "-";                             "7" = "z";
    "2" = "w";                             "8" = "u";
    "3" = "f";                             "9" = "y";
    "4" = "p";                             "0" = "q";
    "5" = "b";                             "-" = "'";

    q = "overload(meta, a)";         u = "m";
    w = "overload(alt, r)";
    # i = "lettermod(n, pseudoShift, 200, 300)";
    i = "overloadi(n, overloadt(pseudoShift, n, 250), 150)"; # allows for quick tap and tap and hold. See the discussion on https://github.com/rvaiya/keyd/issues/608
#     i = "overload(pseudoShift, n)";
    e = "s";                         o = "e";
    r = "overload(pseudoShift, t)";        p = "overload(alt, i)";
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

  # https://github.com/rvaiya/keyd/issues/711
  "capsWord:S" = {
    c = "togglem(capsWord, space)";
    m = "togglem(capsWord, enter)";
    j = "togglem(capsWord, comma)";
  };

in {
  uriel.builtin = {
    ids = ["0001:0001:098cf552"];
    settings = {
      inherit base global main numbers symbols;#  toBase;
      inherit "capsWord:S";
#      "controlAlt:C-A" = "controlAlt:C-A";
    };
    extraConfig = ''
      [controlAlt:C-A]

      [pseudoShift:S]
      j=,
    '';
  };

}
