;; Lifted from org-increment-number-at-point
(cl-defun inc-at-point (&key (inc 1) (line-number nil))
  "Increment the number at point.
With a keyword prefix numeric argument INC, increment using
this numeric value.
If line-number is t, then increment by current line number - inc"
  (interactive "p")
  (if (not (number-at-point))
      (user-error "Not on a number")
;;     (unless inc (setq inc 1))
    (save-excursion
      (let* ((pos (point))
             (beg (skip-chars-backward "0-9"))
             (end (skip-chars-forward "0-9"))
             (nap (buffer-substring-no-properties
                   (+ pos beg) (+ pos beg end))))
        (setq inc
              (if line-number (- (line-number-at-pos) inc) inc))
        (delete-region (+ pos beg) (+ pos beg end))
        (insert (calc-eval (concat (number-to-string inc) "+" nap)))))))
