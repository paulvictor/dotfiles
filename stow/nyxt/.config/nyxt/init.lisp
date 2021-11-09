(in-package #:nyxt-user)

(define-configuration (buffer web-buffer)
  ((default-modes (append '(nyxt::emacs-mode) %slot-default%))))
(define-configuration (buffer web-buffer)
  ((default-modes (append '(nyxt::vi-normal-mode) %slot-default%))))

(start-swank)
