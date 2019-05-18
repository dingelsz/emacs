(print "Loading ~/.emacs ...")
(setq start_time (float-time))

 ;;  _  __            ____  _           _ _                 
 ;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___ 
 ;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
 ;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
 ;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
 ;;           |___/                               |___/     
(print "Binding keys...")

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)

;;  _____                 _   _                 
;; |  ___|   _ _ __   ___| |_(_) ___  _ __  ___ 
;; | |_ | | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; |  _|| |_| | | | | (__| |_| | (_) | | | \__ \
;; |_|   \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
(print "Defining functions...")

 ;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

 ;;  ____            _                    
 ;; |  _ \ __ _  ___| | ____ _  __ _  ___ 
 ;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \
 ;; |  __/ (_| | (__|   < (_| | (_| |  __/
 ;; |_|   \__,_|\___|_|\_\__,_|\__, |\___|
 ;;                            |___/      
(print "Setting up packages...")

(require 'package)
(setq package-user-dir "~/.emacs.d/packages")

;; MELPA
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

; ---- REQUIRED PACKAGES

(require 'iedit)

(require 'conda)
(require 's) ;; String package needed by conda
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(conda-anaconda-home "/Users/zach/Projects/Programming/Python/miniconda3")
 '(package-selected-packages (quote (magit conda docker docker-tramp iedit))))

;;  _____ __  __    _    ____ ____  
;; | ____|  \/  |  / \  / ___/ ___| 
;; |  _| | |\/| | / _ \| |   \___ \ 
;; | |___| |  | |/ ___ \ |___ ___) |
;; |_____|_|  |_/_/   \_\____|____/ 
                                  

(print "Configuring emacs")

;; Look & Feel
(load-theme 'tsdh-light)
(set-default-font "monospace 14")
(scroll-bar-mode 0)
(column-number-mode 1)
(setq ring-bell-function 'ignore)

;; Functionality
(ido-mode 1)
(setq ido-separator "\n")

;;  _____ _       _     _     
;; |  ___(_)_ __ (_)___| |__  
;; | |_  | | '_ \| / __| '_ \ 
;; |  _| | | | | | \__ \ | | |
;; |_|   |_|_| |_|_|___/_| |_|
                            

(setq finish_time (float-time))

(print (format "Finished in %.2f seconds." (- (float-time) start_time)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
