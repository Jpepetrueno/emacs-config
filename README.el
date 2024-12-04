;; Configure Emacs core settings
(use-package emacs
  :bind (("M-o" . other-window)
	 ("C-x C-b" . ibuffer))
  :init
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
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
	require-final-newline t
	tab-always-indent 'complete)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; Install and configure Magit package for a more user-friendly Git interface
(use-package magit
  :ensure t)

;; smartparens configuration
(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; enable global strict-mode
  (smartparens-global-strict-mode)
  ;; enable the pres-set bindings
  (sp-use-smartparens-bindings))

;; Enable auto-fill mode to automatically wrap text
(use-package auto-fill
  :hook
  (prog-mode text-mode markdown-mode org-mode)
  :config
  (setq fill-column 80)
  (setq-default auto-fill-function 'do-auto-fill)
  (auto-fill-mode 1)
  :delight " AF")

;; Enable keycast mode to display key sequences
(use-package keycast
  :ensure t
  :init
  (keycast-tab-bar-mode 1))

;; Enable undo tree
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))

;; Enable global-display-line-numbers-mode
(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))

;; Enable Flyspell
(use-package flyspell
  :config
  (flyspell-prog-mode))

;; Enable which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Config Rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

;; Config Emacs Lisp
(use-package lisp-mode
  :config
  (defun my-elisp-eval-and-run-all-tests-in-buffer ()
    "Deletes all loaded tests from the runtime, evaluates the current
      buffer and runs all loaded tests with ert."
    (interactive)
    (ert-delete-all-tests)
    (eval-buffer)
    (ert 't))
  :bind (:map emacs-lisp-mode-map
	      ("C-c e b" . my-elisp-eval-and-run-all-tests-in-buffer))
  :hook
  (emacs-lisp-mode . flymake-mode))
