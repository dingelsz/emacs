;; -----------------------.emacs --- Emacs init file----------------------------
;; Author: Zach Dingels
;; A solid emacs config. This config aims to have as little dependencies as
;; possible in order to make it more portable. The config file should be easy
;; read and navigate.

;; --------------------------------- General -----------------------------------
(setq ring-bell-function 'ignore)

(add-to-list 'exec-path "/usr/local/bin/")
(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(global-set-key (kbd "C-x m") 'man)

(show-paren-mode 1)

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

;; -------------------------------- Visuals ------------------------------------
;; Configure the line mode bar at the bottom of emacs
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
      '((:eval (simple-mode-line-render
                ;; left side
                (format-mode-line "%+%b [%m]")
                ;; right side
                (format-mode-line "Line: %l | Column: %c")))))

;; GUI settings
(menu-bar-mode -1)
(if (window-system)
    (progn
      (add-to-list 'default-frame-alist '(height . 24))
      (add-to-list 'default-frame-alist '(width . 80))
      
      (set-default-font "Source Code Pro Light 20")
      ;; Disable noisey UI features
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (column-number-mode 1)
      ;; Smooth scrolling
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-scroll-amount '(1 ((shift) .1) ((control) . nil)))
      ))

;; Set the custom face stuff to save to a different file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; ------------------------------ Packages -------------------------------------
(require 'package)

;; Store all packages in the package path
(setq package-path "~/.emacs.d/packages/")
(add-to-list 'load-path package-path)
(setq package-user-dir package-path)

;; Add repos
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?b ?c ?d ?e ?f ?g ?h))
  :bind ("C-x o" . ace-window))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dark+ t)
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

;; (use-package iedit
;;   :ensure t
;;   :bind ("C-x ;" . iedit-mode)
;;   )

(use-package magit
  :bind ("C-x g" . magit-status)
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

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

;; -------------------------------- Random -------------------------------------
;; Don't show the splash screen - go straight to scratch
(setq inhibit-splash-screen t)
(setq initial-scratch-message
";;  ----------------------------------------------------------------------------
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

(global-set-key (kbd "C-x C-e") 'eval-and-replace)
