(defun top-level-line? (line)
  "true if the passed in string is a top level statement in python"
  (cond
   ((string= "" line) nil)
   ((string= "\n" (substring line 0 1)) nil)
   ((not (string= " " (substring line 0 1))) t)
   (nil nil)))

(defun python-prev-chunk ()
  "gets the previous chunk of code"
  (setq initial-point (point))
  (setq position-start (line-beginning-position))
  (setq position-end (line-end-position))
  ;; While the line starts with a space, move up. 
  (while (not (top-level-line? 
	       (buffer-substring-no-properties position-start position-end)))
    (previous-line)
    (setq position-start (line-beginning-position)))
  (goto-char initial-point)
  (buffer-substring-no-properties position-start position-end))

(defun python-eval-print-last-sexp ()
  "Print result of evaluating current line into current buffer."
  (interactive)
  (let ((res (python-shell-send-string-no-output
              ;; modify to get a different sexp
              (python-prev-chunk)))
        (standard-output (current-buffer)))
    (when res
      (terpri)
      (princ res)
      (terpri))))

