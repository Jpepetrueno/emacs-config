;;; early-init.el --- Early initialization code for Emacs

;;; Commentary:
;; This file contains early initialization code for Emacs, setting up the
;; initial frame, garbage collection threshold, and other basic
;; settings.

;;; Code:

;; Set the initial frame to be maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set the initial value of gc-cons-threshold
(setq gc-cons-threshold 10000000) ; initial value, later overridden


;;; early-init.el ends here
(provide 'early-init)
