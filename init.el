;; -----------------------.emacs --- Emacs init file----------------------------
;; Author: Zach Dingels
;; A solid emacs config. This config aims to have as little dependencies as
;; possible in order to make it more portable. The config file should be easy
;; read and navigate.
;; --------------------------------- General -----------------------------------
(setq org-src-fontify-natively t
      default-directory "~/"
      ring-bell-function 'ignore
      tab-always-indent 'complete)

(add-to-list 'exec-path "/usr/local/bin/")
(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(global-set-key (kbd "M-g") 'goto-line)

;; Smooth scrolling & Turn off text resizing via scroll
;; (pixel-scroll-precision-mode)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)

;; Path completion
(global-set-key (kbd "C-x ~") #'(lambda ()
				  (interactive)
				  (insert "~/")
				  (comint-dynamic-complete-filename)))
(global-set-key (kbd "C-x /") #'(lambda ()
				  (interactive)
				  (insert "/")
				  (comint-dynamic-complete-filename)))
(global-set-key (kbd "C-x .") #'(lambda ()
				  (interactive)
~/				  (insert "./")
				  (comint-dynamic-complete-filename)))

(show-paren-mode 1)

;; A menu to view everything in the kill ring
(global-set-key (kbd "C-x y") '(lambda ()
				(interactive)
				(popup-menu 'yank-menu)))

(global-visual-line-mode 1)
;; Backup and AutoSave. Note, backup saves old versions of files when we
;; overwrite them. Autosave makes saves after we make changes but before
;; we save.
;; Backup files to the path below. Version controll them as well!
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backup")))
      backup-by-copying      t  ;; Don't de-link hard links
      version-control        t  ;; Use version numbers on backups
      delete-old-versions    t  ;; Automatically delete excess backups:
      kept-new-versions      20 ;; how many of the newest versions to keep
      kept-old-versions      5) ;; and how many of the old
(unless (file-directory-p "~/.emacs.d/auto-save")
  (make-directory "~/.emacs.d/auto-save"))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; Tabs
(add-to-list 'completion-styles 'initials t)

;; Window size management
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<up>") 'shrink-window)
(global-set-key (kbd "C-S-<down>") 'enlarge-window)

;; Change buffer name like tmux
(global-set-key (kbd "C-x ,") 'rename-buffer)

;; -----------------------------------Custom------------------------------------
(when (file-exists-p "~/.emacs.d/utils.el")
  (load "~/.emacs.d/utils.el"))

(defun my-extend-return ()
  (local-set-key (kbd "<C-return>") #'extend-comment))

(defalias 'yes-or-no-p 'y-or-n-p)

;; A quick terminal
(global-set-key (kbd "C-x t") #'terminal)
;; Same for python
(global-set-key (kbd "C-c p") #'python-terminal)

(add-hook 'emacs-lisp-mode-hook #'my-extend-return)
(add-hook 'lisp-mode-hook #'my-extend-return)
(add-hook 'lisp-interaction-mode-hook #'my-extend-return)
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; --------------------------------- Themes ------------------------------------
;; Terminal
(unless (window-system)
    (progn
      (load-theme 'wombat t)
      (menu-bar-mode -1)))
 (when (window-system)
   (progn
    (add-to-list 'load-path "/Users/zach/projects/repos/nano-emacs")
     (require 'nano-theme)
     (require 'nano-layout)
     (setq nano-fonts-use t)
     (require 'nano-theme-light)

(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

;; Nano default settings (optional)
(require 'nano-defaults)

;; Undo nano emacs settings Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-option-key-is-meta t
        mac-command-key-is-meta nil
        mac-command-modifier 'control
        mac-option-modifier 'meta
        mac-use-title-bar nil))

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
(require 'nano-modeline)

     (pixel-scroll-precision-mode t)
     ))

;; ------------------------------ Packages -------------------------------------
;; Store packages in a specific folder for version control
(unless (file-directory-p "~/.emacs.d/packages")
  (make-directory "~/.emacs.d/packages" t))
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

;; Notes on use package:
;; :init runs code at startup as if it was placed directly in .emacs
;; :config runs code once the package has been loaded. 
(use-package ace-window
  :bind ("C-x o" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setf aw-dispatch-alist '()))

(use-package auto-yasnippet
  :bind
  ("M-w" . #'aya-create)
  ("M-y" . #'aya-expand)
  :init (setq aya-marker "$"))

;; Move to a point in the buffer using tree search
(use-package avy
  :bind
  ("C-o" . avy-goto-char-timer))

;; A collection of useful commands
(use-package crux
  :bind
  ("C-c o" . crux-open-with)
  ("C-c C-e" . crux-eval-and-replace)
  ("C-c k" . crux-kill-other-buffers)
  ("C-c I" . crux-find-user-init-file))

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

  (add-hook 'haskell-mode-hook 'dante-mode))

;; Mode line package
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-encoding nil))


(use-package electric
  :hook
  (java-mode . electric-pair-mode)
  (c-mode . electric-pair-mode)
  (c++-mode . electric-pair-mode))

;; IRC Client
(use-package erc
  :config
  (setq erc-autojoin-channels-alist '(("libera.chat"
				       "#emacs"
				       "#python"
				       "#lisp"
				       "#commonlisp"
				       "#crypto-feed"
				       "#trading"
				       ))
	erc-hide-list '("JOIN" "PART" "QUIT")
	erc-server "irc.libera.chat"
	erc-nick "cuz"
	erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")
	erc-track-shorten-function nil
	erc-join-buffer 'bury))

(defun erc-clear-notifications ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)
	      

;; Execute shell commands from the current path
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package geiser
  :defer t
  :init (add-hook 'scheme-mode-hook 'geiser-mode)
  :commands geiser-mode)

(use-package geiser-guile
  :defer t)

(defun ido-my-keys ()
  "Keybindings for navigating ido results ."
  (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match))

;; Hippie Expand
(global-set-key (kbd "C-<tab>") 'hippie-expand)

(use-package ido
  :init
  (setq ido-enable-regexp t)
  (setq ido-separator "\n")
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always)
  (setq ido-enable-flex-matching t)
  (ido-mode 1)
  (add-hook 'ido-setup-hook 'ido-my-keys)
  :bind ("C-<tab>" . ido-hippie-expand)
  )

;; Edit multiple occurences of a word
(use-package iedit
  :ensure t				
  :bind ("C-\\" . iedit-mode))

;; Git client
(use-package magit
  :bind ("C-x g" . magit-status))

;; Manual pages
(use-package man
  :bind ("C-x m" . man)
  :config
  (custom-set-variables '(Man-notify-method 'bully)))

;; Have more than one cursor at a time
(use-package multiple-cursors)

;; Organization mode
(use-package org
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :hook
  (org-mode-hook . visual-line-mode)
  (org-agenda-finalize-hook hl-line-mode)
  :config
  ;; General
  (setq org-directory "~/org"
	org-startup-folded t
	org-hide-emphasis-markers t
	org-agenda-window-setup 'other-window
	org-tags-column 1
    org-html-htmlize-output-type 'css)
  ;; Agenda
  (setq org-agenda-files '("~/org")
	org-agenda-span 'day
	org-agenda-window-setup 'current-window 
	org-use-speed-commands t
	org-speed-commands-user '(("d" . org-decrypt-entry)
				  ("e" . org-encrypt-entry)
				  ("s" . org-save-all-org-buffers)
				  ("w" . org-refile)
				  ("z" . org-add-note)))
  ;; TODO list
  (setq org-log-done 'time
	org-reverse-note-order t
	org-use-fast-todo-selection t
	org-highest-priority 1
	org-lowest-priority 3
	org-default-priority 4
	org-clock-in-switch-to-state "ACTIVE"
	org-clock-out-when-done t
	org-clock-persists-query-resume nil
	org-todo-keywords '((sequence "TODO(t!)" "ACTIVE(a!)" "|" "DONE(d!)")
			    (sequence "BLOCKED(b@/!)" "|" "CANCELLED(c@/!)"))
	org-todo-keyword-faces '(("TODO" :foreground "orange" :weight bold)
				 ("ACTIVE" :foreground "light blue" :weight bold)
				 ("BLOCKED" :foreground "red" :weight bold)
				 ("DONE" :foreground "forest green" :weight bold)
				 ("CANCELLED" :foreground "forest green" :weight bold)))

  ;; Org Capture
  (setq org-capture-templates '(("m" "Meeting" entry (file "~/org/stagging.org")
				 "* TODO %?\nCREATED: %U\nSCHEDULED: %U\n- Questions\n- Notes\n- Action Items\n")
				("t" "Task" entry (file "~/org/stagging.org")
				 "* TODO %?\n"))
	org-refile-targets  '((nil :maxlevel . 1) (org-agenda-files :maxlevel . 3))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil)

  (defun org-search ()
    (interactive)
    (org-refile '(4)))
  (setq completion-styles '(substring partial-completion)
	org-completion-use-ido t)

  ;; Agenda
  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil
	org-agenda-compact-blocks t
	org-agenda-restore-window-after-quite t)
  (setq org-agenda-custom-commands
	'(("o" "Organizer" 
	   ((tags "REFILE"
		   ((org-agenda-overriding-header "Tasks to Refile")
		    (org-tags-match-list-sublevels nil)))
	    (tags-todo "*/ACTIVE"
		       ((org-agenda-override-header "Current")))
	    (tags-todo "*/BLOCKED"
		       ((org-agenda-override-header "Blocked")))
	    (tags-todo "*/TODO"
		       ((org-agenda-override-header "Backlog")))))))
  
  ;; Inline images
  (setq org-startup-with-inline-images t
	org-image-actual-width nil)

  ;; Babel - Literate programming
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (C . t) (sqlite . t) (shell . t) (lisp . t) (java . t)))
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window) ;; Fixes weird SQLite indent issue in babel
  ;; Cosmetics
  (set-face-attribute 'org-block-begin-line nil
                      :overline "#AAAAAA" :underline "#AAAAAA"
                      :foreground "#5AABD1" :background "#F9F9F9" :extend t)  
  (set-face-attribute 'org-block nil
                      :background "#F9F9F9")
  (set-face-attribute 'org-block-end-line nil
                      :overline "#AAAAAA" :underline "#AAAAAA"
                      :foreground "#5AABD1" :background "#F9F9F9" :extend t)
  (set-face-attribute 'org-block-end-line nil
                      :overline "#AAAAAA" :underline "#AAAAAA"
                      :foreground "#5AABD1" :background "#F9F9F9" :extend t)
  (set-face-attribute 'org-code nil
                      :background "#FFFFFF" :extend t
                      :underline "#AAAAAA")

  ;; EPA - Used for encryption
  (require 'epa-file)
  (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
  ;; Org-crypt for encryption
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key nil
	    org-tags-exclude-from-inheritance (quote ("crypt")))
  :custom-face

  (org-block ((t (:background "#F9F9F9" :extend t))) )
)

(use-package org-modern
  :config
  (setq org-modern-star '("#" "##" "###" "####" "#####"))
  (setq org-ellipsis " ⇒ "))

(use-package plantuml-mode
  :config
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  (setq plantuml-default-exec-mode 'jar)
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

(use-package htmlize)

(use-package gnus
  :config
  (setq user-full-name "Zachary Dingels"
	user-mail-address "zacharydingels@gmail.com"
	gnus-select-method '(nnimap "imap.gmail.com")
	smtpmail-default-smtp-server "smtp.gmail.com"
	message-directory "~/.emacs.d/mail"
	nnml-directory "~/.emacs.d/mail"))

(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'fundamental-mode-hook       #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode            #'enable-paredit-mode)  
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

;; Project management
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-x p" . projectile-command-map))

(use-package python
  :defer t
  :bind (("C-c [" . python-indent-shift-left)
	 ("C-c ]" . python-indent-shift-right)
	 ("C-c /" . comment-line)
	 ("C-j" . python-eval-print-last-sexp))
  :init
  (setq python-indent-guess-indent-offset t  
	python-indent-guess-indent-offset-verbose nil)
  (load "~/.emacs.d/packages/pydocs.el")
  (load "~/.emacs.d/packages/pyfun.el"))

;; Notes 
(use-package remember
  :bind ("C-x C-r" . remember))

;; HTTP library 
(use-package request)

;; slime setup
(use-package slime
  :init
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'slime-mode-hook
          (lambda ()
            (unless (slime-connected-p)
              (save-excursion (slime))))
	  #'enable-paredit-mode)
  (show-paren-mode 1))

;; Connect to remote machines
(use-package tramp
  :config
  (customize-set-variable 'tramp-default-method "ssh"))

;; Good terminal
(use-package vterm  :ensure t)

;; Visual Undo Tree
(use-package vundo
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Snippet library
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; ---------------------------------- ELISP ------------------------------------
(use-package dash)

(use-package s)

;; -------------------------------- Random -------------------------------------
;; Don't show the splash screen - go straight to scratch
(setq inhibit-splash-screen t)
(setq initial-scratch-message ";;  ----------------------------------------------------------------------------
;; | Write tests before code
;; | Write equations before tests
;; | Test quantitatively with simulation data
;; | Test qualitatively with real data
;; | Automate tests
;; | Use a package instead of DIY
;; | Test the package
;; | Optimize code later
;; | Optimize code for readability before speed
;; | Functions should be short, less than 25 lines
;; | Files should be short, less than 500 lines
;; | Write programs that do one thing and do it well
;; | Write programs to work together
;; | Complexity is the enemy
;; | An implementation should be conservative in its sending behavior, and 
;;   liberal in its receiving behavior.
;;  ----------------------------------------------------------------------------

")
;; --------------------------------- Misc --------------------------------------
(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (org-defkey org-agenda-mode-map [(tab)] (lambda ()
						      (interactive)
						      (org-agenda-goto)
						      (org-narrow-to-subtree)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(package-selected-packages
   '(org-modern pdf-tools geiser-guile lsp-java vundo vterm use-package slime request projectile plantuml-mode paredit multiple-cursors magit iedit htmlize geiser exec-path-from-shell doom-modeline dante crux clues-theme auto-yasnippet ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(put 'narrow-to-page 'disabled nil)
