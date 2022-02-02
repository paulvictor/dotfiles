
(use-package lsp-mode
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  (lsp-log-io t)
  :init
  (lsp-modeline-diagnostics-mode))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  (setq lsp-haskell-server-args ())
  )

