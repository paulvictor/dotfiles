(defsrc
  esc     f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
  grv     1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab     q    w    e    r    t    y    u    i    o    p    [    ]    \
  caps    a    s    d    f    g    h    j    k    l    ;    '    ret
  lsft      z    x    c    v    b    n    m    ,    .    /    rsft     up
  lctl    lmet lalt           spc            ralt rmet cmp  rctl left down right)

(deflayer qwerty
  esc     @qwe   @col   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
  grv         1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab         q    w    e    r    t    y    u    i    o    p    [    ]    \\
  @fx          a    s    d    f    g    h    j    k    l    ;    @apos-colemak    @ret-ctl
  @lsft-esc     z    x    c    v    b    n    m    ,    .    /    @rsft-C-c             up
  @lctl-C-x lmet lalt           @spc-alt            ralt rmet cmp @rctl-M-x left down right)

;; (defalias cma
;;     (around
;;      (sticky-key 500 lctl)
;;      (around
;;       (sticky-key 500 lmet)
;;       (sticky-key 500 lalt))))
(defalias
  qwe (layer-switch qwerty)
  col (layer-switch colemak-dh)
  apos-colemak (tap-next apos (layer-toggle colemak-dh))
  apos-qwerty (tap-next apos (layer-toggle qwerty))
  ret-ctl (tap-hold-next 200 ret lctl)
  fx (tap-hold-next 500 C-M-A-up C-M-lalt)
  rctl-M-x (tap-hold-next 200 A-x rctl)
  lctl-C-x (tap-hold-next 200 C-x lctl)
  spc-alt (tap-hold-next 200 spc lalt)
  rsft-C-c (tap-hold-next 200 C-c rsft)
  lsft-esc (tap-hold-next 200 esc lsft))

(deflayer colemak-dh
  esc     @qwe   @col   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
  grv      1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab      q    w    f    p    b    j    l    u    y    ;    [    ]    \\
  @fx      a    r    s    t    g    m    n    e    i    o    @apos-qwerty    @ret-ctl
  @lsft-esc       x    c    d    v    z    k    h    ,    .    /    @rsft-C-c     up
  @lctl-C-x     lmet lalt           @spc-alt            ralt rmet _    @rctl-M-x left down right)
