(setq load-prefer-newer t)
;; Set command history length to 50 entries
(setq history-length 50)
;; Load use-package library to efficiently manage Emacs packages
;; Revert non-file buffers as well
(setq global-auto-revert-non-file-buffers t)
;; Set custom variables file and load it into Emacs
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
;; Load Wombat theme to customize Emacs appearance
(load-theme 'wombat t)
;; Disable tool bar
(tool-bar-mode -1)
;; Load use-package library for efficient package management,
;; providing a more declarative and flexible alternative to
;; the built-in package.el
(require 'use-package)

;; Customize default emacs
(use-package emacs
  :ensure nil
  :hook
  ((after-init . pending-delete-mode)
   (after-init . toggle-frame-maximized)
   (org-mode . auto-fill-mode)
   (text-mode . auto-fill-mode))
  :custom
  (visible-bell t)
  (savehist-mode 1)
  (recentf-mode 1)
  (save-place-mode 1)
  (global-auto-revert-mode 1)
  (use-short-answers t)
  (debugger-stack-frame-as-list t)
  (history-delete-duplicates t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (tab-always-indent 'complete)
  :config
  (setq-default c-basic-offset 4
                c-default-style "linux"
                indent-tabs-mode nil
                fill-column 80
                tab-width 4)
  (auto-fill-mode 1)
  (setq-default fill-column 80)
  (add-hook 'before-save-hook 'org-fill-paragraph)
  ;; :bind ("RET" . newline-and-indent)
  :init
  ;; For run emacs lisp's test with ert
  (defun my-eval-and-run-all-tests-in-buffer ()
    "Deletes all loaded tests from the runtime, evaluates the current
  buffer and runs all loaded tests with ert."
    (interactive)
    (if (fboundp 'ert-delete-all-tests)
        (ert-delete-all-tests))
    (eval-buffer)
    (ert 't)))

;; Install and configure Magit package for a more user-friendly Git interface
(use-package magit
  :ensure t)

;; define the def-pairs macro
(defmacro def-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(mapcar (lambda (pair)
                 `(defun ,(intern (format "wrap-with-%ss" (car pair)))
                      (&optional arg)
                    (interactive "p")
                    (sp-wrap-with-pair ,(cdr pair))))
               pairs)))

;; define the pairing functions
(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

;; smartparens configuration
(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)
              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-d" . delete-sexp)
              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)
              ("M-[" . sp-backward-unwrap-sexp)
              ("M-]" . sp-unwrap-sexp)
              ("C-x C-t" . sp-transpose-hybrid-sexp)
              ("C-c ("  . wrap-with-parens)
              ("C-c ["  . wrap-with-brackets)
              ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c `"  . wrap-with-back-quotes))
  :config
  ;; load default config
  (require 'smartparens-config)
  (smartparens-strict-mode t))