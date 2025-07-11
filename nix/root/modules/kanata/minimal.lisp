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

(defalias
  whky   (layer-switch whky)
  qwerty (layer-switch qwerty)
  lparen  S-9
  rparen  S-0

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

  spcth   (tap-hold-release $tt $ht spc (layer-while-held num))

)

