;;; **Appearance and Themes**
;;; Load Wombat theme to customize Emacs appearance
(load-theme 'wombat t)

;;; **History and Record**
;;; Enable command history mode to save and recall previously executed commands
(savehist-mode 1)
;;; Set command history length to 50 entries
(setq history-length 50)

;;; **Files and Buffers**
;;; Enable recent files mode for easy access to recently opened files
(recentf-mode 1)
;;; Enable save place mode to remember cursor position in open files
(save-place-mode 1)
;;; Enable global auto-revert mode to automatically update buffers
;;; when underlying files change
(global-auto-revert-mode 1)
;;; Revert non-file buffers as well
(setq global-auto-revert-non-file-buffers t)

;;; **Configuration and Personalization**
;;; Set custom variables file and load it into Emacs
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;;; **Packages and Extensions**
;;; Load use-package library to efficiently manage Emacs packages
(require 'use-package)
;;; Install and configure Magit package for a more user-friendly Git interface
(use-package magit
  :ensure t)
;;; Install and configure Smartparens package to intelligently manage character pairs
(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add Smartparens mode to these modes
  :config
  ;; load default configuration
  (require 'smartparens-config))

;;; **Other Configurations**
;;; Enable visual bell instead of audible bell
(setq visible-bell t)
