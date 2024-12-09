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

;; Get environment variables such as $PATH from the shell.
(use-package exec-path-from-shell
  :ensure t
  :init
  ;; Initialize exec-path-from-shell in various Emacs environments.
  (when (or (memq window-system '(mac ns x))
	    (eq window-system 'pgtk)
	    (daemonp))
    (exec-path-from-shell-initialize)))

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
  :hook (prog-mode text-mode markdown-mode)
  :config
  ;; enable global strict-mode
  (smartparens-global-strict-mode)
  ;; enable the pres-set bindings
  (sp-use-smartparens-bindings)
  ;; disable autoclose for ' and ` in Emacs Lisp mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil))

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
  (defun elisp/ert-run-tests-in-buffer ()
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
	      ("C-c o b" . elisp/ert-run-tests-in-buffer))
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

;; Enable auto-fill mode to automatically wrap text
(use-package auto-fill
  :hook
  (prog-mode text-mode markdown-mode)
  :config
  (auto-fill-mode)
  :delight "AF")

;; This package is a minor mode to visualize blanks
(use-package whitespace
  :hook (prog-mode text-mode markdown-mode))

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
   (dired-mode . hl-line-mode)
   (dired-mode . dired-omit-mode))
  :config
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	delete-by-moving-to-trash t
	dired-dwim-target t))

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
  :bind ("<f7>" . dictionary-lookup-definition)
  :config (setq dictionary-server "dict.org"))

;; Interaction mode for Emacs Lisp
(use-package ielm
  :bind (:map ielm-map
	      ("C-c C-q" . ielm/clear-repl)
	      ("<S-return>" . ielm/insert-newline))
  :config

  (defun ielm/clear-repl ()
    "Clear current REPL buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (ielm-send-input)))

  (defun ielm/insert-newline ()
    "Insert a newline without evaluating the sexp."
    (interactive)
    (let ((ielm-dynamic-return nil))
      (ielm-return))))

(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l d" . eldoc)
	      ("C-c l f" . eglot-format)
	      ("<f6>" . eglot-format)
	      ("C-c l r" . eglot-rename)
	      ("C-c l s" . eglot-shutdown)
	      ("C-c l S" . eglot-shutdown-all)
	      ("C-c l i" . eglot-inlay-hints-mode)
	      ("C-c l e" . eglot-events-buffer)
	      ("C-c l x" . eglot-stderr-buffer)
	      ("C-c l c" . eglot-clear-status)
	      ("C-c l u" . eglot-signal-didChangeConfiguration)
	      ("C-c l o" . eglot-code-action-organize-imports)
	      ("C-c l q" . eglot-code-action-quickfix)
	      ("C-c l X" . eglot-code-action-extract)
	      ("C-c l n" . eglot-code-action-inline)
	      ("C-c l w" . eglot-code-action-rewrite)
	      ("C-c l b" . eglot-format-buffer)
	      ("C-c l R" . eglot-reconnect)
	      ("C-c l B" . flymake-show-buffer-diagnostics)
	      ("C-c l P" . flymake-show-project-diagnostics)
	      ("C-c l g" . xref-find-definitions)
	      ("C-c l m" . imenu)
	      ("C-c l C" . completion-at-point)))

(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; set ellama-long-lines-length to fill-column
  (setq ellama-long-lines-length fill-column)
  :hook
  (ellama-session-mode . (lambda () (whitespace-mode -1))))

;; Python's flying circus support for Emacs
(use-package python
  :bind (:map python-ts-mode-map
	      ("<f5>" . recompile))
  :hook
  ((python-ts-mode . eglot-ensure))
  :mode
  (("\\.py\\'" . python-ts-mode)))

;; A better *help* buffer.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)))
