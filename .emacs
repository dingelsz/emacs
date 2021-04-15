;; -----------------------.emacs --- Emacs init file----------------------------
;; Author: Zach Dingels
;; A solid emacs config. This config aims to have as little dependencies as
;; possible in order to make it more portable. The config file should be easy
;; read and navigate.

;; --------------------------------- General -----------------------------------
(setq org-src-fontify-natively t)
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
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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

;; Notes on use package:
;; :init runs code at startup as if it was placed directly in .emacs
;; :config runs code once the package has been loaded. 

;; Move to a point in the buffer using tree search
(use-package avy  :bind ("C-'" . avy-goto-char-timer)
  )

;; A collection of useful commands
(use-package crux
  :config
  :bind ("C-c o" . crux-open-with)
  ("C-c e" . crux-eval-and-replace)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c I" . crux-find-user-init-file)
  
  )

;; Path completion
(global-set-key (kbd "C-x /") 'comint-dynamic-complete-filename)

;; A haskell IDE
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

;; IRC Client
(use-package erc
  :config
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#haskell" "#python" "#mysql")))
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  )
	      

;; Execute shell commands from the current path
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )


(defun ido-my-keys ()
  "Keybindings for navigating ido results ."
  (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match) )

(use-package ido
  :init
  (setq ido-enable-regexp t)
  (setq ido-separator "\n")
  (ido-mode 1)
  (add-hook 'ido-setup-hook 'ido-my-keys)
  )

;; Edit multiple occurences of a word
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode)
)

;; Git client
(use-package magit
  :bind ("C-x g" . magit-status)
  )

;; Manual pages
(use-package man
  :bind ("C-x m" . man)
  :config
  (custom-set-variables '(Man-notify-method 'bully))
  )


(use-package multiple-cursors
;;  :bind ("C-x ;" . mc/mark-all-words-like-this)
;;  :bind ("C-x '" . set-rectangular-region-anchor)
  )

;; Organization mode
(use-package org
  :bind ("C-c a" . org-agenda)
  :config 
  (setq org-log-done 'time)
  ;; Setup org mode for Kanban
  (setq org-todo-keywords 
	'((sequence "BACKLOG" "ON DECK" "SPECIFY" "SPECIFY-DONE" "IMPLEMENTING" "IMPLEMENTING-DONE"  "TESTING" "|" "DONE" "DELEGATED")))
  (setq org-agenda-files '("~/Google Drive/org"))
  (setq org-agenda-sorting-strategy '((agenda todo-state-down)))
  ;; Setup org mode babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (C . t) (sqlite . t) (shell . t)))
  ;; Org-crypt for encryption
  (require 'org-crypt)
  (require 'epa-file)
;;  (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
  (epa-file-enable)
  
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  )

;; Project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
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
  (setq python-shell-interpreter "Projects/Programming/Python/miniconda3/envs/main/bin/ipython"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
  (load "~/.emacs.d/packages/pydocs.el") ;; Custom yasnippet documentation
  (load "~/.emacs.d/packages/pyfun.el") ;; Runs python code like interactive elisp
  (setq python-python-command "/Users/zach/Projects/Programming/Python/miniconda3/bin/python")
  )

;; HTTP library 
(use-package request)

;; Shows multiple options in interactive mode
(use-package selectrum
  :config (selectrum-prescient-mode) (selectrum-mode +1)
  )

;; Connect to remote machines
(use-package tramp
  :config
  (customize-set-variable 'tramp-default-method "ssh")
  )

;; Tracks undo history as a tree
(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :bind
  ("C-c /" . undo-tree-visualize)
  )

;; Good terminal
(use-package vterm
  :ensure t
)

;; Snippet library
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

;; --------------------------------- Themes ------------------------------------
;; Themes
(if (window-system)
    (progn
      (require 'nano-layout)

      ;; Theme
      (require 'nano-faces)
      (nano-faces)
      (require 'nano-theme)
      (require 'nano-theme-light)
      (nano-theme)

      ;; Nano header & mode lines (optional)
      (require 'nano-modeline)
      
      (provide 'nano)
      )
  (load-theme 'doom-dark+ t)
)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ein paredit geiser racket-mode magit restclient pdf-tools docker-compose-mode dockerfile-mode docker undo-tree crux selectrum-prescient selectrum projectile request lpy org-bullets zenburn-theme yaml-mode writeroom-mode vterm virtualenvwrapper virtualenv use-package sphinx-doc sml-mode smart-mode-line-atom-one-dark-theme slime posframe plantuml-mode parseclj parsec multiple-cursors lv latex-preview-pane iedit htmlize exec-path-from-shell dracula-theme doom-themes dired-toggle dired-subtree dante csv-mode counsel auctex atom-one-dark-theme all-the-icons ace-window))
 '(python-shell-buffer-name "vterm"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
