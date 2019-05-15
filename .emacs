(print "Loading ~/.emacs ...")
(setq start_time (float-time))

 ;;  _  __            ____  _           _ _                 
 ;; | |/ /___ _   _  | __ )(_)_ __   __| (_)_ __   __ _ ___ 
 ;; | ' // _ \ | | | |  _ \| | '_ \ / _` | | '_ \ / _` / __|
 ;; | . \  __/ |_| | | |_) | | | | | (_| | | | | | (_| \__ \
 ;; |_|\_\___|\__, | |____/|_|_| |_|\__,_|_|_| |_|\__, |___/
 ;;           |___/                               |___/     

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)

 ;;  ____            _                    
 ;; |  _ \ __ _  ___| | ____ _  __ _  ___ 
 ;; | |_) / _` |/ __| |/ / _` |/ _` |/ _ \
 ;; |  __/ (_| | (__|   < (_| | (_| |  __/
 ;; |_|   \__,_|\___|_|\_\__,_|\__, |\___|
 ;;                            |___/      
(print "Setting up packages...")

(require 'package)
(setq package-user-dir "~/.emacs.d/packages")

 ;;  __  __ _____ _     ____   _    
 ;; |  \/  | ____| |   |  _ \ / \   
 ;; | |\/| |  _| | |   | |_) / _ \  
 ;; | |  | | |___| |___|  __/ ___ \ 
 ;; |_|  |_|_____|_____|_| /_/   \_\

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(require 'iedit)
 ;;  ____       _               
 ;; / ___|  ___| |_ _   _ _ __  
 ;; \___ \ / _ \ __| | | | '_ \ 
 ;;  ___) |  __/ |_| |_| | |_) |
 ;; |____/ \___|\__|\__,_| .__/ 
 ;;                      |_|    
(print "Running setup...")

;; Font Size
(set-default-font "monospace 14")

(scroll-bar-mode 0)

(ido-mode 1)
(setq ido-separator "\n")

(setq ring-bell-function 'ignore)

(load-theme 'tsdh-light)

(column-number-mode 1)

(setq finish_time (float-time))

(print (format "Finished in %.2f seconds." (- (float-time) start_time)))

 ;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(package-selected-packages (quote (docker docker-tramp iedit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
