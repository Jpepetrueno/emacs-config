;; Configure Emacs core settings
(use-package emacs
  :bind (("M-o" . other-window)
	 ("C-x C-b" . ibuffer))
  :init
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Start the initial frame maximized
  (setq load-prefer-newer t
	global-auto-revert-non-file-buffers t
	custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file)

  :config
  (savehist-mode 1)
  (recentf-mode 1)
  (global-auto-revert-mode 1)
  (desktop-save-mode 1)
  (pending-delete-mode 1)
  (fido-mode t)
  (fido-vertical-mode t)
  (setq visible-bell t
	use-short-answers t
	debugger-stack-frame-as-list t
	history-length 50
	history-delete-duplicates t
	completion-ignore-case t
	read-buffer-completion-ignore-case t
	switch-to-buffer-obey-display-actions t
	tab-always-indent 'complete)
  (add-to-list 'savehist-additional-variables 'kill-ring) ; Save the kill ring between sessions
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

;; smartparens configuration
(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  ;; :bind (:map smartparens-mode-map
  ;; 	      ("C-M-a" . sp-beginning-of-sexp)
  ;; 	      ("C-M-e" . sp-end-of-sexp)
  ;; 	      ("C-<down>" . sp-down-sexp)
  ;; 	      ("C-<up>"   . sp-up-sexp)
  ;; 	      ("M-<down>" . sp-backward-down-sexp)
  ;; 	      ("M-<up>"   . sp-backward-up-sexp)
  ;; 	      ("C-M-f" . sp-forward-sexp)
  ;; 	      ("C-M-b" . sp-backward-sexp)
  ;; 	      ("C-M-n" . sp-next-sexp)
  ;; 	      ("C-M-p" . sp-previous-sexp)
  ;; 	      ("C-S-f" . sp-forward-symbol)
  ;; 	      ("C-S-b" . sp-backward-symbol)
  ;; 	      ("C-<right>" . sp-forward-slurp-sexp)
  ;; 	      ("M-<right>" . sp-forward-barf-sexp)
  ;; 	      ("C-<left>"  . sp-backward-slurp-sexp)
  ;; 	      ("M-<left>"  . sp-backward-barf-sexp)
  ;; 	      ("C-M-t" . sp-transpose-sexp)
  ;; 	      ("C-M-k" . sp-kill-sexp)
  ;; 	      ("C-k"   . sp-kill-hybrid-sexp)
  ;; 	      ("M-k"   . sp-backward-kill-sexp)
  ;; 	      ("C-M-w" . sp-copy-sexp)
  ;; 	      ("C-M-d" . delete-sexp)
  ;; 	      ("M-<backspace>" . backward-kill-word)
  ;; 	      ("C-<backspace>" . sp-backward-kill-word)
  ;; 	      ([remap sp-backward-kill-word] . backward-kill-word)
  ;; 	      ("M-[" . sp-backward-unwrap-sexp)
  ;; 	      ("M-]" . sp-unwrap-sexp)
  ;; 	      ("C-x C-t" . sp-transpose-hybrid-sexp)
  ;; 	      ("C-c ("  . wrap-with-parens)
  ;; 	      ("C-c ["  . wrap-with-brackets)
  ;;		("C-c {"  . wrap-with-braces)
  ;;		("C-c '"  . wrap-with-single-quotes)
  ;;		("C-c \"" . wrap-with-double-quotes)
  ;;		("C-c `"  . wrap-with-back-quotes))

  :config
  ;; load default config
  (require 'smartparens-config)
  ;; enable strict-mode
  (smartparens-strict-mode t)
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
	      (back-quote . "`"))))

;; Enable auto-fill mode to automatically wrap text
(use-package auto-fill
  :init
  (auto-fill-mode 1)
  :hook
  (prog-mode text-mode markdown-mode org-mode)
  :config
  (setq fill-column 80)
  :delight " AF")

;; Enable keycast mode to display key sequences
(use-package keycast
  :ensure t
  :init
  (keycast-tab-bar-mode 1))

;; Configure undo-fu-session to exclude certain files and enable global mode
(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

;; Enable global-display-line-numbers-mode
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

;; Enable Flyspell
(use-package flyspell
  :config
  (flyspell-prog-mode))
