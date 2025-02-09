;;; early-init.el --- Early init Emacs -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file contains early initialization code for Emacs, setting up
;; the initial frame, garbage collection threshold, and other basic
;; settings.
;;
;;; Code:

;; Reduce garbage collection during startup to improve performance.
(setq gc-cons-threshold (* 2 800000))

;; Disable debugging mode by default (set to "1" to enable)
(setenv "DEBUG" nil)

;; Enable debug mode if DEBUG envvar is set
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; Set the initial frame to be maximized
(add-to-list #'initial-frame-alist '(fullscreen . maximized))


;;; early-init.el ends here
