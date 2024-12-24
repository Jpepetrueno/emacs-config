;; Configure Emacs built-in variables and settings
(use-package emacs
  :bind
  ("C-x C-b" . ibuffer)
  ("C-c i" . dimagid/find-user-readme-org-file)
  ("C-c d" . eldoc)
  :init
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)
  :custom
  (visible-bell t)
  (tab-always-indent 'complete)
  (use-short-answers t)
  (require-final-newline t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (switch-to-buffer-obey-display-actions t)
  (Info-hide-note-references nil)
  (debugger-stack-frame-as-list t)
  (history-delete-duplicates t)
  (kill-do-not-save-duplicates t)
  (display-buffer-alist
   '(("\\*Occur\\*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected))))
  :config
  (fido-mode)
  (fido-vertical-mode)
  (column-number-mode)
  (tty-tip-mode)
  (defun dimagid/find-user-readme-org-file ()
    "Edit the README.org file in another window."
    (interactive)
    (find-file-other-window (concat "~/.config/emacs/README.org"))))

;; Get environment variables such as $PATH from the shell.
(use-package exec-path-from-shell
  :ensure t
  :init
  ;; Initialize exec-path-from-shell in various Emacs environments.
  (when (or (memq window-system '(mac ns x))
	    (eq window-system 'pgtk)
	    (daemonp))
    (exec-path-from-shell-initialize)))

;; Preview completion with inline overlay
(use-package completion-preview
  :bind (:map completion-preview-active-mode-map
	      ("M-n" . completion-preview-next-candidate)
	      ("M-p" . completion-preview-prev-candidate))
  :hook (prog-mode text-mode markdown-mode)
  :config
  (completion-preview-mode)
  (global-completion-preview-mode))

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
  :init
  (setq corfu-preview-current nil
	corfu-min-width 20
	corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode) ; shows documentation after `corfu-popupinfo-delay'
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
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Introduces a margin formatter for Corfu which adds icons.
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :custom
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Shows icons for each file in dired mode.
(use-package nerd-icons-dired
  :ensure t
  :hook dired-mode
  :delight " NID")

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
  :config
  (setq
   desktop-dirname "~/.config/emacs/desktop/"
   desktop-base-file-name "~/.config/emacs/desktop/.emacs.desktop"
   desktop-base-lock-name "~/.config/emacs/desktop/.emacs.desktop.lock")
  (desktop-save-mode))

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
  (set-face-attribute 'sp-pair-overlay-face nil :background "#444444")
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
  (global-undo-tree-mode 1)
  :delight " UTree")

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
  (defun dimagid/elisp-ert-run-tests-in-buffer ()
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
	      ("C-c o b" . dimagid/elisp-ert-run-tests-in-buffer))
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
  :hook (text-mode markdown-mode))

;; Evaluation Result OverlayS for Emacs Lisp.
(use-package eros
  :ensure t
  :config (eros-mode))

;; Manage and navigate projects in Emacs easily.
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :init (projectile-mode +1)
  :delight " PJILE")

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
	dired-dwim-target t)
  :delight "Dired")

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
	      ("C-c C-q" . dimagid/ielm-clear-repl)
	      ("<S-return>" . dimagid/ielm-insert-newline))
  :config
  (defun dimagid/ielm-clear-repl ()
    "Clear current REPL buffer."
    (interactive)
    (let ((inhibit-read-only t))
	(erase-buffer)
	(ielm-send-input)))
  (defun dimagid/ielm-insert-newline ()
    "Insert a newline without evaluating the sexp."
    (interactive)
    (let ((ielm-dynamic-return nil))
	(ielm-return))))

;; The Emacs Client for LSP servers
(use-package eglot
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l f" . eglot-format)
	      ("<f6>" . eglot-format)
	      ("C-c l r" . eglot-rename)
	      ("C-c l n" . flymake-goto-next-error)
	      ("C-c l p" . flymake-goto-prev-error)
	      ("C-c l s" . flymake-show-buffer-diagnostics)
	      ("C-c l S" . flymake-show-project-diagnostics)
	      ("C-c l i" . eglot-inlay-hints-mode)
	      ("C-c l e" . eglot-events-buffer)
	      ("C-c l x" . eglot-stderr-buffer)
	      ("C-c l c" . eglot-clear-status)
	      ("C-c l u" . eglot-signal-didChangeConfiguration)
	      ("C-c l o" . eglot-code-action-organize-imports)
	      ("C-c l q" . eglot-code-action-quickfix)
	      ("C-c l X" . eglot-code-action-extract)
	      ("C-c l I" . eglot-code-action-inline)
	      ("C-c l w" . eglot-code-action-rewrite)
	      ("C-c l b" . eglot-format-buffer)
	      ("C-c l R" . eglot-reconnect)))

;; Tool for interacting with LLMs.
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

;; Major mode for editing C and similar languages
(use-package cc-mode
  :bind (:map c-mode-map
	      ("<f5>" . recompile))
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure))
  :mode
  ("\\.c\\'" . c-mode)
  ("\\.cpp\\'" . c++-mode)
  ("\\.h\\'" . c-mode)
  ("\\.hpp\\'" . c++-mode))

;; A better *help* buffer.
(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function)))

;; A cornucopia of useful interactive commands to make your Emacs experience
;; more enjoyable.
(use-package crux
  :ensure t
  :bind
  ("M-o" . crux-smart-open-line)
  ("M-O" . crux-smart-open-line-above)
  (:map ctl-x-4-map
	("t" . crux-transpose-windows)))

;; Track command frequencies.
(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-excluded-commands
	'(self-insert-command
	  forward-char
	  backward-char
	  previous-line
	  next-line
	  org-self-insert-command
	  sp-backward-delete-char
	  mwheel-scroll))
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;; Pulse highlight on demand or after select functions.
(use-package pulsar
  :ensure t
  :custom
  (pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  :config
  (setq pulsar-face 'pulsar-green
	pulsar-iterations 5)
  (pulsar-global-mode))

;; Yet another snippet extension for Emacs
(use-package yasnippet
  :config
  (yas-global-mode))

;; Specialized comint.el for running the shell
(use-package shell
  :ensure nil
  :hook (shell-mode . my-shell-mode-hook-func)
  :config
  (defun my-shell-mode-hook-func ()
    (set-process-sentinel (get-buffer-process (current-buffer))
			  'my-shell-mode-kill-buffer-on-exit))
  (defun my-shell-mode-kill-buffer-on-exit (process state)
    (message "%s" state)
    (if (or
	 (string-match "exited abnormally with code.*" state)
	 (string-match "finished" state))
	(kill-buffer (current-buffer)))))
