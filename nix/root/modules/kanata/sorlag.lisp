(defsrc
 esc
 1    2    3    4    5     7    8    9    0      -
 q    w    e    r    t     u    i    o    p      lbrc
 caps a    s    d    f  g  j    k    l    scln   apos          ret
 sft            c    v          m    ,    f23
 ;; TODO we need to also map Ctrl to Ctrl/CtrlX in the qwerty layer
)

(deflayer qwerty
  esc
  1    2    3    4    5     7    8    9    0      -
  q    w    e    r    t     u    i    o    p      lbrc
  caps a    s    d    f  g  j    k    l    scln   apos         @ctrlret
  @sftesc        c    v          m    ,    @whky)

(defvar
  tap-repress-timeout 200
  hold-timeout 200
  tt $tap-repress-timeout
  chord-timeout 100
  ct $chord-timeout
  ht $hold-timeout)

(defchordsv2
 (i o)   esc    $ct all-released (qwerty)
 (c ,)   S-scln $ct all-released (qwerty)
 (v m)   scln   $ct all-released (qwerty)
 (k l)   (caps-word-toggle 2000) $ct all-released () ;; Even in the base layer
 )

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
  ctrlo   (tap-hold-release $tt $ht o lctl)
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
  dotcomm (fork  . (unshift comm) (lsft rsft))
  bspcdel (fork bspc (unshift del) (lsft rsft)))

(deflayer whky
  @qwerty
  -         w         f     p        b     z            u          y     q         apos
  @metaa    @altr     s     @sftt    g     m            @sftn      e     @alti     @ctrlo
  XX        @ctrlx    x     c        d     v            @dotcomm   h     j         k        l    XX
  XX                @spcth  @tabth        @retth  @bspcdel  @qwerty)

(deflayer num
  XX
  -        /          \           @pipe      XX     `         4     5     6     @plus
  @lt      lbrc       rbrc        @gt        XX     @ques     1     2     3     0
  XX       @lcrly   @lparen    @rparen     @rcrly     XX      .     7     8     9       =        XX
  XX                  XX           XX                @retth    @bspcdel   XX)

(deflayer sym
  @qwerty
  XX       XX       XX       XX       XX     @grv      @dllr     @perc    @caret    apos
  XX       XX       XX       XX       XX     @amp      @excl     @at      @hash     @astr
  XX       XX       XX       XX       XX       XX     .         left      down      up        rght       XX
  XX                         XX       XX               @retth    @bspcdel  XX)


