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

(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<up>") 'shrink-window)
(global-set-key (kbd "C-S-<down>") 'enlarge-window)

;; -----------------------------------Custom------------------------------------
(load "~/.emacs.d/utils.el")

(defun my-extend-return ()
  (local-set-key (kbd "<C-return>") #'extend-comment))
;; 

(add-hook 'emacs-lisp-mode-hook #'my-extend-return)
(add-hook 'lisp-mode-hook #'my-extend-return)
(add-hook 'lisp-interaction-mode-hook #'my-extend-return)
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

	  
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
(use-package avy :bind ("C-o" . avy-goto-char-timer)
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

;; Mode line package
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-encoding nil))


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
  :bind ("C-\\" . iedit-mode)
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

(use-package nano-theme
  :ensure nil
  :defer t)

;; Organization mode
(use-package org
  :bind ("C-c a" . org-agenda)
  :config 
  (setq org-log-done 'time)
  ;; Setup org mode for Kanban
  (setq org-todo-keywords 
	'((sequence "BACKLOG" "ON DECK" "SPECIFY" "IMPLEMENTING" "TESTING" "|" "DONE" "DELEGATED")))
  (setq org-agenda-files '("~/Google Drive/org"))
  (setq org-agenda-sorting-strategy '((agenda todo-state-down)))
  ;; Setup org mode babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (C . t) (sqlite . t) (shell . t) (ledger . t) (lisp . t)))
  ;; Org-crypt for encryption
  (require 'org-crypt)
  (require 'epa-file)
;;  (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
  (epa-file-enable)
  
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  (setq org-crypt-key nil)
  (add-hook 'org-mode-hook #'visual-line-mode)
  )

(use-package paredit
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

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
      ;; (require 'nano-layout)

      ;; ;; Theme
      ;; (require 'nano-faces)
      ;; (nano-faces)
      ;; (require 'nano-theme)
      ;; (require 'nano-theme-light)
      ;; (nano-theme)

      ;; ;; Nano header & mode lines (optional)
      ;; (require 'nano-modeline)
      
      ;; (provide 'nano)
      )
  (progn 
    (menu-bar-mode -1))
;;  (load-theme 'doom-dark+ t)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("ff4d091b20e9e6cb43954e4eeae1c3b334e28b5923747c7bd5d2720f2a67e272" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "361f5a2bc2a7d7387b442b2570b0ef35198442b38c2812bf3c70e1e091771d1a" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "1ed5c8b7478d505a358f578c00b58b430dde379b856fbcb60ed8d345fc95594e" "0ad7f1c71fd0289f7549f0454c9b12005eddf9b76b7ead32a24d9cb1d16cbcbd" "6231254e74298a1cf8a5fee7ca64352943de4b495e615c449e9bb27e2ccae709" "56911bd75304fdb19619c9cb4c7b0511214d93f18e566e5b954416756a20cc80" "845103fcb9b091b0958171653a4413ccfad35552bc39697d448941bcbe5a660d" "6bacece4cf10ea7dd5eae5bfc1019888f0cb62059ff905f37b33eec145a6a430" "24132f7b6699c6e0118d742351b74141bac3c4388937e40db9207554eaae78c9" "1526aeed166165811eefd9a6f9176061ec3d121ba39500af2048073bea80911e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "a83f05e5e2f2538376ea2bfdf9e3cd8b7f7593b16299238c1134c1529503fa88" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" default))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(sr-speedbar neotree planet-theme nord-theme zenburn-theme writeroom-mode vterm virtualenvwrapper virtualenv use-package undo-tree telega spinner sphinx-doc solidity-mode sml-mode smart-mode-line-atom-one-dark-theme slime selectrum-prescient restclient racket-mode pyvenv python-environment projectile posframe plantuml-mode pdf-tools parseclj parsec paredit org-bullets noxml-fold nano-theme multiple-cursors markdown-mode magit lpy log4j-mode latex-preview-pane hy-mode htmlize ht highlight-indentation geiser exec-path-from-shell epc ein dracula-theme doom-themes doom-modeline dockerfile-mode docker-compose-mode docker dired-toggle dired-subtree dante csv-mode crux auto-yasnippet auto-complete auctex atom-one-dark-theme async))
 '(tramp-default-method "ssh"))

(load-theme 'nord t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "#bcbcbc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(font-lock-string-face ((t (:foreground "color-84")))))

(set-face-attribute 'mode-line nil
                    :background "#111111"
                    :foreground "white"
                    :overline nil
                    :underline nil)
