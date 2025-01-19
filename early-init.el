;;; early-init.el --- Early initialization code for Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; This file contains early initialization code for Emacs, setting up the
;; initial frame, garbage collection threshold, and other basic
;; settings.

;;; Code:

;; Reduce garbage collection during startup to improve performance.
(if noninteractive  ; in CLI sessions
    ;; Adjust GC settings for CLI sessions to balance performance and memory usage.
    (setq gc-cons-threshold 536870912  ; 512 MB)
  ;; Disable GC during interactive startup to improve performance.
  ;; Note: This value is reset to its default value (0.76 MB) after `after-init-hook`.
	  (setq gc-cons-threshold most-positive-fixnum)))

;; Enable debugging mode for additional error information
(setenv "DEBUG" "1")

;; Enable debug mode if DEBUG envvar is set
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; Set the initial frame to be maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Disable elisp-flymake-byte-compile to avoid errors
(advice-add 'elisp-flymake-byte-compile :around #'ignore)

;;; early-init.el ends here
