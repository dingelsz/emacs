;; -----------------------.emacs --- Emacs init file----------------------------
;; Author: Zach Dingels
;; About: 
;; Code to get a solid emacs running
;; -----------------------------------------------------------------------------
(add-to-list 'exec-path "/usr/local/bin/")
;;(add-to-list 'exec-path "/Library/TeX/texbin/")
(setq-default TeX-PDF-mode t)
(setq-default TeX-engine 'xetex)

(global-set-key (kbd "C-x C-x") 'execute-extended-command)

;; ---------------------------------- GUI --------------------------------------

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(setq-default mode-line-format
      '((:eval (simple-mode-line-render
                ;; left
                (format-mode-line "%+%b [%m]")
                ;; right
                (format-mode-line "Line: %l | Column: %c")))))

(menu-bar-mode -1)
(if (window-system)
    (progn
      (add-to-list 'default-frame-alist '(height . 24))
      (add-to-list 'default-frame-alist '(width . 80))
      
;;      (set-default-font "Fira Code Light 18")
      (set-default-font "Major Mono Display 18")
      ;; Disable noisey UI features
      (scroll-bar-mode 0)
      (tool-bar-mode 0)
      (column-number-mode 1)
      ))

(show-paren-mode 1)

;; --------------------------------- Others ------------------------------------

(setq ring-bell-function 'ignore)

;; Backup files to the path below. Version controll them as well!
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

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

(use-package doom-themes
  :init
  (if (window-system) (load-theme 'doom-tomorrow-day t)
    (load-theme 'doom-dark+ t))
  )

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package ido
  :init
  (setq ido-enable-regexp t)
  (setq ido-separator "\n")
  (ido-mode 1)
  )

(add-hook 'ido-setup-hook 'ido-my-keys)

(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match) )


(use-package iedit)

;;(use-package ivy
;;  :hook (after-init . ivy-mode)
;;  )

(use-package jedi
  :hook (jedi:ac-setup . python-mode)
  :init
  (setq ac-auto-show-menu nil)
  (add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c d") 'jedi:show-doc)))
  )

(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable)
  )

(use-package magit
  :bind ("C-x g" . magit-status)
  )

(use-package org
  :config 
  (setq org-log-done 'time)
  )

(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (setenv "JAVA_HOME" "/Library/Internet Plug-Ins/JavaAppletPlugin.plugin/Contents/Home")
  (add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
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
  )


(use-package tramp
  :config
  (customize-set-variable 'tramp-default-method "ssh")
  )

(use-package virtualenvwrapper
  :config
  (progn
    (venv-initialize-interactive-shells) 
    (setq venv-location "~/.venv/")
    (venv-workon "emacs")
    ))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  )

;; -------------------------------- Random -------------------------------------

;; Don't show the splash screen - go to straight to scratch
(setq inhibit-splash-screen t)
(setq initial-scratch-message
";;  ---------------------------------------------------------------------------
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
;;  Write programs that do one thing and do it well. 
;;  Write programs to work together. 
;;  ----------------------------------------------------------------------------

")

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

;; run python code in any buffer and output below the buffer
(global-set-key (kbd "C-c C-j") 'python-eval-print-last-sexp)



