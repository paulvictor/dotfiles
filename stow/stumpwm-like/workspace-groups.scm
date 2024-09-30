(use-modules
 (swayipc)
 (modules workspace-groups))

(define OUTPUTS
  (map sway-output-name
       (sway-get-outputs)))

;; (define WS-GROUPS
;;   (let ((workspace-names '("dev" "browsing" "communication" "system" "music" "misc")))
;;     (map (lambda (ws-name ws-idx)
;;            (let* ((outputs-count (length OUTPUTS)))
;;              (map (lambda (o-idx)
;;                     (format "~a~a-~a" o-idx ws-idx ws-name))
;;                   (iota outputs-count))))
;;          workspace-names (iota (length (workspace-names))))))
