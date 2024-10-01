(use-modules
 (swayipc)
 (modules workspace-groups))

(define OUTPUTS
  (map sway-output-name
       (sway-get-outputs)))

(define workspace-names  '("devel" "browsing" "communication" "system" "personal" "music" "misc"))
(define WS-GROUPS
  (map (lambda (ws-name ws-idx)
         (let* ((outputs-count (length OUTPUTS)))
           (map (lambda (o-idx)
                  (format #f "~a~a: ~a" ws-idx o-idx ws-name))
                (iota outputs-count 1))))
       workspace-names
       (iota (length workspace-names) 1)))

(workspace-groups-configure #:outputs OUTPUTS #:groups WS-GROUPS)
(workspace-groups-init)
