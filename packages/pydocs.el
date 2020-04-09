
;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.
(defun tokenize (input)
  """ Tokenizes a list of arguments"""
  (let ((chunks (split-string input ","))
	)
    chunks))

;; An argument is made up of a name, optional type and optional default value
(cl-defstruct argument name type default)

(defun parse-argument (input)
  "creates an argument from a string"
  (when (string-match "\s*\\([^:[:blank:]=]*\\)\s*:?\s*\\([^=[:blank:]]*\\)\s*=?\s*['\"]?\\([^'\"]*\\)\s*" input)
    (make-argument :name (match-string 1 input)
		   :type (match-string 2 input)
		   :default (match-string 3 input))))


(defun argument-docstring (arg)
  "returns a string representation of the argument"
  (concat
   (if (eq (argument-name arg) "") "" (concat ":param " (argument-name arg) ":"))
   (if (eq (argument-default arg) "") "" (concat " , defaults to " (argument-default arg)))
   (if (eq (argument-name arg) "") "" "\n")
   (if (eq (argument-type arg) "") "" (concat ":type " (argument-name arg) ": " (argument-type arg)))))

(defun make-docstring (input)
  "makes a docstring form a list of agurmnets"
  (let ((result nil)
	(args (mapcar 'parse-argument (tokenize input))))
    (while args
      (setq result (push (argument-docstring (car args)) result))
      (setq args (cdr args)))
    (mapconcat 'identity (reverse result) "\n")))
