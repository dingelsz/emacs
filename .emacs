(print "Loading ~/.emacs ...")
(setq start_time (float-time))


;;  _____                 _   _                 
;; |  ___|   _ _ __   ___| |_(_) ___  _ __  ___ 
;; | |_ | | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _|| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
(print "Defining functions...")

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

 ;;  _  __            ____  _           _ _                 
 ;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___ 
 ;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
 ;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
 ;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
 ;;           |___/                               |___/     
(print "Binding keys...")

(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c C-d") 'sphinx-doc)
(global-set-key (kbd "C-c [") 'python-indent-shift-left)
(global-set-key (kbd "C-c ]") 'python-indent-shift-right)
(global-set-key (kbd "C-x /") 'comment-line)

(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c d") 'jedi:show-doc)))


;;  _____ __  __    _    ____ ____  
;; | ____|  \/  |  / \  / ___/ ___| 
;; |  _| | |\/| | / _ \| |   \___ \ 
;; | |___| |  | |/ ___ \ |___ ___) |
;; |_____|_|  |_/_/   \_\____|____/ 
(print "Configuring emacs")

;; Set the window dimensions
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Theme/font
(load-theme 'leuven)
(set-default-font "Fira Code Light 18")

;; Disable noisey UI features
(scroll-bar-mode 0) 
(column-number-mode 1)
(tool-bar-mode -1) 

;; Unbold keywords
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:bold nil)))))

;; Silence any noises
(setq ring-bell-function 'ignore)

;; Backup files to the path below. Version controll them as well!
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Whenever a buffer named py is opened set it to python mode
(setq-default major-mode
  (lambda ()
  (if (string= "py" (buffer-name))
      (python-mode)(emacs-lisp-mode))))
 ;;  ____            _                    
 ;; |  _ \ __ _  ___| | ____ _  __ _  ___ 
 ;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \
 ;; |  __/ (_| | (__|   < (_| | (_| |  __/
 ;; |_|   \__,_|\___|_|\_\__,_|\__, |\___|
 ;;                            |___/      w
(print "Setting up packages...")

;; Path to packages
(setq package-path "~/.emacs.d/packages/")

;; Add the package path to the list of places to load packages from
;; And tell package-install to install new packages there
(add-to-list 'load-path package-path)
(setq package-user-dir package-path)

(package-initialize)
(require 'package)

;; Add package repos refresh
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
;;(package-refresh-contents)
					;
; ---- Builtins
(ido-mode 1)
(setq ido-separator "\n")

; ---- REQUIRED PACKAGES
(require 'iedit)
(require 'magit)

;; Org Mode - Log time a task is completed
(setq org-log-done 'time)

(require 'tramp)
(customize-set-variable 'tramp-default-method "ssh")

(require 'yasnippet)
(yas-global-mode 1)

;; python interpreter
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) 
(setq venv-location "~/projects/venv/")
(venv-workon "emacs")

;; Python - documentation, turn off autocomplete
(add-hook 'python-mode-hook 'jedi:ac-setup)
(auto-complete-mode 0)

;; Weird annoying yet harmless bug with python mode
(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)


;; Custom code for making docstrings w\ yasnippet
(load "~/.emacs.d/packages/pydocs.el")
;;  _____ _       _     _     
;; |  ___(_)_ __ (_)___| |__  
;; | |_  | | '_ \| / __| '_ \ 
;; |  _| | | | | | \__ \ | | |
;; |_|   |_|_| |_|_|___/_| |_|
(print (format "Finished in %.2f seconds." (- (float-time) start_time)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (virtualenvwrapper jedi-core company-jedi jedi-direx yasnippet-snippets pyenv-mode virtualenv elpy zenburn-theme yaml-mode writeroom-mode sphinx-doc magit-popup magit jedi iedit haskell-mode ghub exec-path-from-shell ess dracula-theme docker csv-mode conda atom-one-dark-theme)))
 '(tramp-default-method "ssh"))
