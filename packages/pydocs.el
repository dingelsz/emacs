
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
  (when (string-match "\s*\\([^:\s=]*\\)\s*:?\s*\\([^=\s]*\\)\s*=?\s*['\"]?\\([^'\"]*\\)\s*" input)
    (make-argument :name (match-string 1 input)
		   :type (match-string 2 input)
		   :default (match-string 3 input))))


(defun argument-docstring (arg)
  "returns a string representation of the argument"
  (concat
   (if (string= (argument-name arg) "") "" (concat ":param " (argument-name arg) ":"))
   (if (string= (argument-default arg) "") "" (concat " , defaults to `" (argument-default arg) "`"))
   (if (string= (argument-type arg) "") "" "\n")
   (if (string= (argument-type arg) "") "" (concat ":type " (argument-name arg) ": " (argument-type arg)))))


(defun make-docstring (input)
  "makes a docstring form a list of agurmnets"
  (let ((result nil)
	(args (mapcar 'parse-argument (remove-all-str (split-string input ",") "self"))))
    (if (string= input "") ""
      (while args
	(setq result (push (argument-docstring (car args)) result))
	(setq args (cdr args)))
      ;; Concat all of the doc strings together (flip them because of the stack)
      (let ((docstring (mapconcat 'identity (reverse result) "\n")))
	;; If theres more than 1 argument there will be a trailing \n
	(if (< (length result) 2) docstring
	  (substring docstring 0 -1))))))


(defun remove-all-str (list target)
  "removes all occurances of a word from a list"
  (cond
   ((null list) ())
   ((not (stringp (car list))) (cons (car list) (remove-all (cdr list) target)))
   ((string= (car list) target) (remove-all (cdr list) target))
   (t (cons (car list) (remove-all (cdr list) target)))))



