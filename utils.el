(require 'cl)

(defun split-region (del rep)
  "Spilts a region by the given delimiter and replaces it with the given rep"
  (interactive
   (list (read-string "delimeter: " "")
	 (read-string "replace: " "\n")))
  (let ((s (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (replace-regexp-in-string del rep s t t))))

(defun extend-comment ()
  (interactive)
  (let ((line (buffer-substring-no-properties
	       (line-beginning-position)
	       (line-end-position))))
    (cond ((< (length line) (length ";;")) (newline))
	  ((string= (substring line 0 (length ";;")) ";;")
	   (progn (move-end-of-line nil)
		  (newline)
		  (insert (concat ";;" " "))))
	  (t (newline)))))

(defun yank-fn (fn)
  "Applies a function before yanking text"
  (interactive "sEnter a function: ")
  (insert (funcall (eval (read fn)) (car kill-ring))))

(defun fncall (fn)
  "Runs a function on the region"
  (interactive "sEnter a function: ")
  (insert (funcall (eval (read fn)) (buffer-substring (region-beginning) (region-end)))))

(defun tokenize-transform-join (delimeter fn join)
  "Splits a region using the delimeter into tokens, applyies a function to each token and then joins it using the join"
  (interactive "sEnter the delimeter: \nsEnter the function: \nsEnter the join:")
  (let* ((s (buffer-substring-no-properties (region-beginning) (region-end)))
	 (tokens (split-string s delimeter))
	 (results (mapcar (eval (read fn)) tokens))
	 (joined (seq-reduce (lambda (head tail) (concat head join tail)) results "")))
    (delete-region (region-beginning) (region-end))
    (insert joined)))


(defun terminal ()
  "Command for opening and switching to a terminal. If the terminal buffer is closed it will open a new one. If it is open but not current it will switch to it. If it is open and the current buffer it will pop to the last buffer (ie hide it)"
  (interactive)
  (if (string= (buffer-name) "terminal") (previous-buffer)
  (let ((buffer (get-buffer "terminal")))
    (if buffer (switch-to-buffer "terminal")
      (progn (vterm)
	     (rename-buffer "terminal"))))))







