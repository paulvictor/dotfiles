(defsrc
 esc
 1    2    3    4    5    6    7    8    9    0
 q    w    e    r    t    y    u    i    o    p    del
 a    s    d    f    g    h    j    k    l    ret
 sft            c    v         n    m
)

(defcfg
  process-unmapped-keys   yes
  concurrent-tap-hold     yes
  allow-hardware-repeat   no
  linux-dev /dev/input/by-path/platform-i8042-serio-0-event-kbd
  )

(deflayer qwerty
  @whky
  1    2    3    4    5    6    7    8    9    0
  q    w    e    r    t    y    u    i    o    p    del
  a    s    d    f    g    h    j    k    l    @ctrlret
  @sftesc        c    v         n    m)

(defvar
  tap-repress-timeout 100
  hold-timeout 200
  tt $tap-repress-timeout
  chord-timeout 100
  ct $chord-timeout
  ht $hold-timeout)

(defalias
  whky   (layer-switch whky)
  qwerty (layer-switch qwerty)
  pipe    S-\
  plus    S-=
  lt      S-,
  gt      S-.
  lparen  S-9
  rparen  S-0
  ques    S-/
  ctrlret (tap-hold-release $tt $ht ret lctl)
  ctrlx   (tap-hold-release $tt $ht C-x lctl)
  ctrlo   (tap-hold-release $tt $ht o rctl)
  metaa   (tap-hold-release $tt $ht a lmet)
  altr    (tap-hold-release $tt $ht r lalt)
  sftt    (tap-hold-release $tt $ht t lsft)
  sftn    (tap-hold-release $tt $ht n rsft)
  alti    (tap-hold-release $tt $ht i ralt)
  lcrly   S-lbrc
  rcrly   S-rbrc
  undr    S--
  grv     S-`
  dllr    S-4
  perc    S-5
  caret   S-6
  amp     S-7
  excl    S-1
  at      S-2
  hash    S-3
  astr    S-8
  sftesc  (tap-hold-release $tt $ht esc lsft)
  spcth   (tap-hold-release $tt $ht spc (layer-while-held num))
  tabth   (tap-hold-release $tt $ht tab (layer-while-held sym))
  retth   (tap-hold-release $tt $ht ret (multi lctl lalt))
  dotcomm (fork Period comm (lsft))
  bspcdel (fork bspc del (lsft))
)

(deflayer whky
  @qwerty
  -         w         f     p        b     z            u       y     q         apos
  @metaa    @altr     s     @sftt    g     m            @sftn   e     @alti     @ctrlo       @ctrlo
  @ctrlx    x         c     d        v     @dotcomm     h       j     k         l

  XX                @spcth  @tabth        @retth  @bspcdel
)

(deflayer num
  @qwerty
  -        /          \           @pipe      XX     `         4     5     6     @plus
  @lt      @lparen    @rparen     @gt        XX     @ques     1     2     3     0       0
  @lcrly   lbrc       rbrc        @rcrly     XX     .         7     8     9     =

  XX                  XX           XX                @retth    @bspcdel
)

(deflayer sym
  @qwerty
  XX       XX       XX       XX       XX     @grv      @dllr     @perc    @caret    apos
  XX       XX       XX       XX       XX     @ques     @excl     @at      @hash     @astr       @astr
  XX       XX       XX       XX       XX     .         left      down     up        rght

  XX                         XX       XX               @retth    @bspcdel
)

(defchordsv2
 (n e)      esc    $ct all-released (qwerty)
 (spc bspc) S-scln $ct all-released (qwerty)
 (tab ret)  scln   $ct all-released (qwerty)
 (h j)      (caps-word-toggle 2000) $ct all-released () ;; Even in the base layer
 )
