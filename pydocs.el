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
  (let ((name (argument-name arg))
	(type (argument-type arg))
	(default (argument-default arg)))
    (concat
     (if (string= name "") ""
       (concat name))
     (if (string= type "") ""
       (concat " (" type ")"))
     (if (string= name"") "" ": ")
     (if (string= default	"") ""
       (concat "Defaults to `" default "`.")))))


(defun make-docstring (input)
  "makes a docstring form a list of agurmnets"
  (let ((result nil)
	(args (mapcar 'parse-argument (remove "self" (split-string input ",")))))
    (if (string= input "") ""
      (progn
	(push "Args: " result)
	(while args
	  (setq result (push (concat "    " (argument-docstring (car args))) result))
	  (setq args (cdr args)))
	;; Concat all of the doc strings together (flip them because of the stack)
	(let ((docstring (mapconcat 'identity (reverse result) "\n")))
	  ;; If theres more than 1 argument there will be a trailing \n
	  (if (< (length result) 2) docstring
	    (substring docstring 0 -1)))))))
  

;; Tests
;;(setq input "arg1: str='hello', arg2: int=2")
;;(setq args (mapcar 'parse-argument (remove "self" (split-string input ","))))

;; (argument-docstring (car args))

;; (make-docstring "arg1")
;; (make-docstring "arg1: str")
;; (make-docstring "arg1: str='hello'")

;; (make-docstring "self, arg1: str='hello'")

;; (make-docstring "self, n, arg1: str='hello'")
;; (make-docstring "self, n=2, arg1: str='hello'")
;; (make-docstring "self, n:int = 2, arg1: str='hello'")
