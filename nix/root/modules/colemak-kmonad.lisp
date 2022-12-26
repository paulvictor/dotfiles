(defsrc
  grv     1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab     q    w    e    r    t    y    u    i    o    p    [    ]    \
  caps    a    s    d    f    g    h    j    k    l    ;    '    ret
  lsft      z    x    c    v    b    n    m    ,    .    /    rsft     up
  lctl    lmet lalt           spc            ralt rmet cmp  rctl left down right)

(deflayer qwerty
  grv         1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab         q    w    e    r    t    y    u    i    o    p    [    ]    \\
  @fx      a    s    d    f    g    h    j    k    l    ;    '    @ret-ctl
  @lsft-esc     z    x    c    v    b    n    m    ,    .    /    @rsft-C-c             up
  @lctl-C-x lmet lalt           @spc-alt            ralt rmet cmp @colemak-toggle left down right)

(defalias cma
    (around
     (sticky-key 500 lctl)
     (around
      (sticky-key 500 lmet)
      (sticky-key 500 lalt))))
(defalias
  ret-ctl (tap-hold-next 200 ret lctl)
  fx (tap-next C-M-A-up C-M)
  colemak-toggle (layer-toggle colemak-dh)
  lctl-C-x (tap-hold-next 200 C-x lctl)
  spc-alt (tap-hold-next 200 spc lalt)
  rsft-C-c (tap-hold-next 200 C-c rsft)
  lsft-esc (tap-hold-next 200 esc lsft))

(deflayer colemak-dh
  grv      1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab      q    w    f    p    b    j    l    u    y    ;    [    ]    \\
  caps      a    r    s    t    g    m    n    e    i    o    '    ret
  lsft       x    c    d    v    z    k    h    ,    .    /    rsft     up
  lctl     lmet lalt           spc            ralt rmet _    rctl left down right)
