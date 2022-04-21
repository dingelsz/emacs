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




(defun read-file (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun load-ssh-hosts ()
  "Loads all the hosts from the ssh config file"
  (mapcar (lambda (x) (second (split-string x " "))) 
	  (remove-if-not (lambda (x) (string= "Host " (substring x 0 5)))
			 (remove-if (lambda (x) (string= x ""))
				    (mapcar #'string-trim-left
					    (split-string (read-file "~/.ssh/config") "\n"))))))

(defun ssh ()
  "Opens a new shell and ssh's into the given host"
  (interactive)
  (let ((address (ido-completing-read "Address: " (load-ssh-hosts) :REQUIRE-MATCH t)))
    (progn 
      (vterm)
      (rename-buffer address)
      (vterm-send-string (format "ssh %s" address))
      (vterm-send-return))))

(defun zen (width)
  "Centers a buffer in the window"
  (interactive "nBuffer width: ")
  (let* ((adj          (- (window-text-width)
                          width))
         (total-margin (+ adj
                          left-margin-width
                          right-margin-width)))
    (setq left-margin-width  (/ total-margin 2))
    (setq right-margin-width (- total-margin left-margin-width)))
  (set-window-buffer (selected-window) (current-buffer)))
