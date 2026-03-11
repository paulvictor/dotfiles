(use-modules (ice-9 pretty-print)
             (guile-swayer swayipc)
             (srfi srfi-1)
             (ice-9 popen)
             (ice-9 string-fun)
             (ice-9 rdelim))

(define* (window-list pred #:optional (tree (sway-get-tree)))
  "Fold over the tree of windows and collect nodes which satisfy the predicate"
  (define (acc-nodes node acc)
    (let* ((from-children (fold acc-nodes '() (sway-tree-nodes node)))
           (new-l (append acc from-children)))
      (if (pred node)
          (cons node new-l)
          new-l)))
  (fold acc-nodes '() (sway-tree-nodes tree)))

(define (focusable-windows-and-ids)
  (map
   (lambda (n) (cons (sway-tree-name n) (sway-tree-id n)))
   (window-list
    (lambda (n) (and
                 (string= (sway-tree-type n) "con")
                 (not (eq? 'null (sway-tree-name n))))))))

(define* (goto-open-windows #:optional (choice-cmd "fuzzel --dmenu"))
  (let* ((choices (focusable-windows-and-ids))
         (window-names (map car choices))
         (window-choice-str (string-join window-names "\n" 'suffix))
         (selected (let ((port
                           (open-input-output-pipe choice-cmd)))
                     (display window-choice-str port)
                     (force-output port)
                     (read-line port))))
    (let ((w
           (find (lambda (name-id)
                   (string= selected (car name-id)))
                 choices)))
      (when w
        (sway-focus-container-criteria
         (sway-criteria #:con-id (cdr w)))))))
