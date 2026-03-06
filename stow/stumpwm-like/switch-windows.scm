(use-modules (ice-9 pretty-print)
             (libs sway-tree-helper)
             (srfi srfi-1)
             (ice-9 rdelim)
             (swayipc))

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

(define (get-choice)
  (let* ((choice-cmd "fuzzel --dmenu")
         (choices (focusable-windows-and-ids))
         (focusable-windows (map car choices))
         (window-choice-str (string-join focusable-windows "\n" 'suffix))
         (selected (let ((port
                           (open-input-output-pipe choice-cmd)))
                     (display window-choice-str port)
                     (read-line port))))
    (pretty-print window-choice-str)
    selected))
