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
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

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
  :bind (:map smartparens-mode-map
	      ("C-c s" . smartparens-command-map))
  :hook (prog-mode text-mode markdown-mode)
  :config
  ;; enable global strict-mode
  (smartparens-global-strict-mode)
  ;; enable the pres-set bindings
  (sp-use-smartparens-bindings)
  ;; disable autoclose for ' and ` in Emacs Lisp mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil))

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
  (defun my-elisp-ert-run-tests-in-buffer ()
    "Deletes all loaded tests from the runtime, saves the current
     buffer and the file being loaded, evaluates the current buffer
     and runs all loaded tests with ert."
    (interactive)
    (save-buffer)
    (let ((file-to-load (progn
			  (goto-char (point-min))
			  (re-search-forward "(load-file \"\\([^)]+\\)\"")
			  (match-string 1))))
      (with-current-buffer (find-file-noselect file-to-load)
	(save-buffer)))
    (ert-delete-all-tests)
    (eval-buffer)
    (ert 't))
  :bind (:map emacs-lisp-mode-map
	      ("C-c e b" . my-elisp-ert-run-tests-in-buffer))
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

;; Manage and navigate projects in Emacs easily.
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :init (projectile-mode +1))

;; Dired
(use-package dired
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;; Manage and navigate projects in Emacs easily.
(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; Viewing/editing system trash can.
(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; Dictionary client for accessing dictionary servers via RFC 2229 protocol
;; (Note: RFC 2229 is an informational document.
;;        RFC: Request for Comments, a system of Internet documents)
(use-package dictionary
  :bind (("C-c d" . dictionary-lookup-definition)
	 ("<f5>" . dictionary-lookup-definition))
  :config (setq dictionary-server "dict.org"))
