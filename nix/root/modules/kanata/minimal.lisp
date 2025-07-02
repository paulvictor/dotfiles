(defsrc
 esc
 w    e    p
 c
)

(deflayer qwerty
          @whky
          w    e    p
          c)
(deflayer whky
          @qwerty
          @altr     s     @ctrlo
          @spcth
  )
(deflayer num
          @qwerty
          @lparen    @rparen     0
          XX
  )
(defvar
  tap-repress-timeout 200
  hold-timeout 200
  tt $tap-repress-timeout
  chord-timeout 100
  ct $chord-timeout
  ht $hold-timeout)

(defchordsv2
 (u i)   esc    $ct all-released (qwerty)
 (c m)   S-scln $ct all-released (qwerty)
 (v n)   scln   $ct all-released (qwerty)
 (j k)   (caps-word-toggle 2000) $ct all-released () ;; Even in the base layer
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

  retth   (tap-hold-release $tt $ht ret (multi lctl lalt))
  dotcomm (fork  . (unshift comm) (lsft rsft))
  bspcdel (fork bspc (unshift del) (lsft rsft))
)

