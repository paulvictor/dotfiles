;; (define-module (passdo))

(use-modules (ice-9 popen))
(use-modules (ice-9 ftw))
(use-modules (ice-9 receive))
(use-modules (ice-9 string-fun))
(use-modules (ice-9 rdelim))
(use-modules (srfi srfi-1))

(define (collect-gpg-files file-path)
  (let* ((enter? (lambda (name stat res)
                   (not (equal? (basename name) ".git"))))
         (without-prefix (lambda (fname)
                           (string-replace-substring fname file-path "")))
         (without-suffix (lambda (fname)
                           (string-replace-substring fname ".gpg" "")))
         (sanitized-name (compose without-prefix without-suffix))
         (leaf (lambda (name stat res)
                 (cons (sanitized-name name) res)))
         (down (lambda (name stat result)
                 result))
         (error (lambda (name stat errno result)
                  (result))))
    (file-system-fold enter? leaf down down down error '() file-path)))

(define (show-menu-and-get-selection)
  (let* ((store-dir (or (getenv "PASSWORD_STORE_DIR")
                        (string-append (getenv "HOME") "/" ".password-store")))
         (file-list (collect-gpg-files store-dir))
         (without-leading-/ (map
                             (lambda (str) (string-drop str 1))
                             file-list))
         (file-list-str (string-join without-leading-/ "\n" 'suffix))
         (rofi-command "rofi -i -matching fuzzy -dmenu")

;;          (rofi-command "fuzzel --dmenu --fuzzy-max-distance=100 --fuzzy-min-length=2 --fuzzy-max-length-discrepancy=100")
         (selected (let ((port
                           (open-input-output-pipe rofi-command)))
                     (display file-list-str port)
                     (read-line port))))
    selected))

(define (get-password selected-password)
  (let* ((otp-prefix
          (if (string-contains selected-password "TOTP") "otp" ""))
         (pass-command
          (string-join (list "pass" otp-prefix selected-password)))
         (port (open-input-pipe pass-command)))
    (read-line port)))

(define (type-out str)
  (system* "ydotool" "type" "--key-delay" "40" str))

(define type-password
  (let ((selected-password (show-menu-and-get-selection)))
    (unless (eof-object? selected-password)
      (let ((passwd (get-password selected-password)))
        (type-out passwd)
        (exit #t)))))
