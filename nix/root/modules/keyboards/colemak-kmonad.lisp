(defsrc
  esc     f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
  grv     1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab     q    w    e    r    t    y    u    i    o    p    [    ]    \
  caps    a    s    d    f    g    h    j    k    l    ;    '    ret
  lsft      z    x    c    v    b    n    m    ,    .    /    rsft     up
  lctl    lmet lalt           spc            ralt rmet cmp  rctl left down right)

(deflayer qwerty
  esc            f1   @col   @cust-0   f4   f5   f6   f7   f8   f9   f10  f11  f12
  grv            1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab            q    w    e    r    t    y    u    i    o    p    [    ]    \\
  @fx            a    s    d    f    g    h    j    k    l    ;    @apos-colemak    @ret-ctl
  @lsft-esc      z    x    c    v    b    n    m    ,    .    /    @rsft-C-c             up
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
  fx (tap-hold-next 200 C-M-A-up C-M-lalt)
  rctl-M-x (tap-hold-next 200 A-x rctl)
  lctl-C-x (tap-hold-next 200 C-x lctl)
  spc-alt (tap-hold-next 200 spc lalt)
  rsft-C-c (tap-hold-next 200 C-c rsft)
  lsft-esc (tap-hold-next 200 esc lsft)

  cust-0 (layer-switch customized-0)
  dbl-quot (around lsft apos)
  c-fx (tap-hold-next 200 a C-M-lalt)
  alt-or-r (tap-hold-next 200 r lalt)
  alt-or-i (tap-hold-next 200 i lalt)
  ctrl-or-o (tap-hold-next 200 o lctl)
  shift-or-s (tap-hold-next 200 s lsft)
  shift-or-e (tap-hold-next 200 e lsft)
  num-1  (tap-hold-next 200 (layer-switch num-1) spc)
  shifted-2  (tap-hold-next 200 (layer-switch shifted-2) spc))

(deflayer colemak-dh
  esc     @qwe   @col   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12
  grv      1    2    3    4    5    6    7    8    9    0    -    =    bspc
  tab      q    w    f    p    b    j    l    u    y    ;    [    ]    \\
  @fx      a    r    s    t    g    m    n    e    i    o    @apos-qwerty    @ret-ctl
  @lsft-esc       x    c    d    v    z    k    h    ,    .    /    @rsft-C-c     up
  @lctl-C-x     lmet lalt           @spc-alt            ralt rmet _    @rctl-M-x left down right)

(deflayer customized-0
  XX      @qwe     XX          XX              XX           XX     XX    XX   XX    XX              XX            XX              XX
  XX      esc      w           f               p            b      XX    q    u     y               ret           bspc            XX    XX
  XX      @c-fx    @alt-or-r   @shift-or-s     t            g      XX    m    n     @shift-or-e     @alt-or-i     @ctrl-or-o      XX    XX
  XX      z        x     c     v               d            XX     -     h    j     k               l             XX
  XX      XX       XX    XX    @num-1          XX     XX    lsft         XX   XX    XX              XX            XX
  XX      XX       XX          @shifted-2      XX    XX           XX    XX    XX    XX              XX)

(deflayer num-1
  XX      XX     XX    XX    XX      XX     XX    XX     XX      XX     XX    XX      XX
  XX      ?      /     \\    |       `      XX    6      7       8      9     0       XX    XX
  XX      <      ,     .     >       '      XX    1      2       3      4     5       XX    XX
  XX      {      [     ]     }       ;      XX    =      left    down   up    right   XX
  XX      XX     XX    XX    XX      XX     XX    XX     XX      XX     XX    XX      XX
  XX      XX     XX          XX      XX     XX    XX     XX      XX     XX    XX)

(deflayer shifted-2
  XX      XX     XX    XX    XX      XX           XX    XX      XX      XX     XX    XX      XX
  XX      f9     f10   f11   f12     ~            XX    ^       &       *      \(    \)      XX    XX
  XX      f5     f6    f7    f8      @dbl-quot    XX    !       @       #      $     %       XX    XX
  XX      f1     f2    f3    f4      :            XX    +       left    down   up    right   XX
  XX      XX     XX    XX    XX      XX           XX    XX      XX      XX     XX    XX      XX
  XX      XX     XX          XX      XX           XX    XX      XX      XX    XX     XX)
