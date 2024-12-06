;;; -*- lexical-binding: t -*-
;; Configure Emacs core settings
(use-package emacs
  :bind
  ("C-x C-b" . ibuffer)
  :init
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (setq load-prefer-newer t
	custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)
  :config
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
	tab-always-indent 'complete))

;; Configure savehist to save minibuffer history
(use-package savehist
  :config
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;; Enable marginalia to add completion annotations to existing commands.
(use-package marginalia
  :ensure t
  :config (marginalia-mode))

;; Corfu enhances in-buffer completion with a small completion popup.
(use-package corfu
  :ensure t
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :after savehist
  :config
  (setq corfu-preview-current nil
	corfu-min-width 20
	corfu-popupinfo-delay '(1.25 . 0.5))
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Emacs nerd font icons library.
(use-package nerd-icons
  :ensure t)

;; Add icons to completion candidates.
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Introduces a margin formatter for Corfu which adds icons.
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Shows icons for each file in dired mode.
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Display nerd icons in ibuffer.
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Enable recentf to track recently opened files
(use-package recentf
  :config (recentf-mode 1))

;; Enable autorevert to revert buffers when files change on disk
(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t))

;; Save partial status of Emacs when killed
(use-package desktop
  :config (desktop-save-mode 1))

;; Enable to delete selection if you insert
(use-package delsel
  :config (delete-selection-mode 1))

;; A git porcelain inside Emacs
(use-package magit
  :ensure t)

;; Automatic insertion, wrapping and paredit-like
;; navigation with user defined pairs.
(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
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

;; Show current command and its binding
(use-package keycast
  :ensure t
  :config (keycast-tab-bar-mode 1))

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))

;; Interface for display-line-numbers
(use-package display-line-numbers
  :config (global-display-line-numbers-mode))

;; On-the-fly spell checker
(use-package flyspell
  :config (flyspell-prog-mode))

;; Display available keybindings in popup
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Highlight brackets according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :hook (emacs-lisp-mode . flymake-mode))

;; Directional window-selection routines
(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

;; Restore old window configurations
;; Use C-c right and C-c left for undo or redo window configurations
(use-package winner
  :config (winner-mode))

;; Minor mode to visualize TAB, (HARD) SPACE, NEWLINE
(use-package whitespace
  :config (global-whitespace-mode))

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :ensure t
  :config (eros-mode))
