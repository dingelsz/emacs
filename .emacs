;; -----------------------.emacs --- Emacs init file----------------------------
;; Author: Zach Dingels
;; A solid emacs config. This config aims to have as little dependencies as
;; possible in order to make it more portable. The config file should be easy
;; read and navigate.

;; --------------------------------- General -----------------------------------

(setq default-directory "~/") 
(add-to-list 'exec-path "/usr/local/bin/")
(global-set-key (kbd "C-x C-x") 'execute-extended-command)

(show-paren-mode 1)
(setq ring-bell-function 'ignore)

;; A menu to view everything in the kill ring
(global-set-key "\M-y" '(lambda ()
			  (interactive)
			  (popup-menu 'yank-menu)))

;; Backup files to the path below. Version controll them as well!
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old


;; ------------------------------ Packages -------------------------------------
;; Store packages in a specific folder for version control
(setq package-path "~/.emacs.d/packages/")
(add-to-list 'load-path package-path)
(setq package-user-dir package-path)
(let ((default-directory  package-path))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(package-initialize)

;; Add repos
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Setup use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

(use-package ace-window
  :config
  (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h))
  :bind ("C-x o" . ace-window))

(use-package counsel
  )

(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :bind ("C-c j" . dante-eval-block)
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR for flymake support:
  (add-hook 'haskell-mode-hook 'flymake-mode)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

  (add-hook 'haskell-mode-hook 'dante-mode)
  )

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match) )

(use-package ido
  :init
  (setq ido-enable-regexp t)
  (setq ido-separator "\n")
  (ido-mode 1)
  (add-hook 'ido-setup-hook 'ido-my-keys)
  )

;;(use-package iedit
;;  :ensure t
;;  :bind ("C-x ;" . iedit-mode)
;;  )

(use-package magit
  :bind ("C-x g" . magit-status)
  )

(use-package man
  :bind ("C-x m" . man)
  :config
  (custom-set-variables '(Man-notify-method 'bully))
  )


(use-package multiple-cursors
  :bind ("C-x ;" . mc/mark-all-words-like-this)
  :bind ("C-x '" . set-rectangular-region-anchor)
  )

(use-package org
  :config 
  (setq org-log-done 'time)
  ;; Setup org mode for Kanban
  (setq org-todo-keywords 
	'((sequence "BACKLOG" "ON DECK" "SPECIFY" "SPECIFY-DONE" "IMPLEMENTING" "IMPLEMENTING-DONE"  "TESTING" "|" "DONE" "DELEGATED")))
  (setq org-agenda-files '("~/.emacs.d/org"))
  (setq org-agenda-sorting-strategy '((agenda todo-state-down)))
  ;; Setup org mode babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (C . t) (sqlite . t) (shell . t)))
  :bind ("C-c a" . org-agenda)
  )

(use-package python
  :defer t
  :bind (("C-c [" . python-indent-shift-left)
	 ("C-c ]" . python-indent-shift-right)
	 ("C-c /" . comment-line)
	 ("C-j" . python-eval-print-last-sexp))
  :init
  (setq python-indent-guess-indent-offset t)  
  (setq python-indent-guess-indent-offset-verbose nil)
  (load "~/.emacs.d/packages/pydocs.el") ;; Custom yasnippet documentation
  (load "~/.emacs.d/packages/pyfun.el") ;; Runs python code like interactive elisp
  (add-hook 'python-mode-hook 'run-python)
  )

(use-package tramp
  :config
  (customize-set-variable 'tramp-default-method "ssh")
  )

(use-package vterm
  :ensure t
)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

;; -------------------------------- Nano ---------------------------------------
;; Window layout (optional)
(require 'nano-layout)

;; Theme
(require 'nano-faces)
(nano-faces)
(require 'nano-theme)
(nano-theme)

(require 'nano-counsel)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

(provide 'nano)

;; -------------------------------- Random -------------------------------------
;; Don't show the splash screen - go straight to scratch
(setq inhibit-splash-screen t)
(setq initial-scratch-message ";;  ----------------------------------------------------------------------------
;;  Write tests before code
;;  Write equations before tests
;;  Test quantitatively with simulation data
;;  Test qualitatively with real data
;;  Automate tests
;;  Use a package instead of DIY
;;  Test the package
;;  Optimize code later
;;  Optimize code for readability before speed
;;  Functions should be short, less than 25 lines
;;  Files should be short, less than 500 lines
;;  Write programs that do one thing and do it well
;;  Write programs to work together
;;  Complexity is the enemy
;;  ----------------------------------------------------------------------------

")

;; --------------------------------- Misc --------------------------------------
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-x C-S-E") 'eval-and-replace)
