;; Configure Emacs built-in variables and settings
(use-package emacs
  :bind
  ("C-x C-b" . ibuffer)
  ("C-c d" . eldoc)
  ("<down-mouse-8>" . kill-ring-save)
  ("<down-mouse-9>" . yank)
  ("C-c o i" . dimagid/find-user-config-org-file)
  ("C-c o r" . restart-emacs)
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)
  :custom
  (visible-bell t) ; Enable visual (flash screen) instead of audible
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
  (enable-recursive-minibuffers t)
  (transient-align-variable-pitch t)
  (set-mark-command-repeat-pop t)
  (global-auto-revert-non-file-buffers t)
  (recentf-max-saved-items 50)
  (shr-width 70) ; Set HTML width to 70
  (display-buffer-alist
   '(("\\*Occur\\*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected))))
  :config
  (fido-vertical-mode)
  (tty-tip-mode)
  (repeat-mode)
  (global-prettify-symbols-mode)
  (save-place-mode) ; Enable saving and restoring cursor positions
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (recentf-mode) ; Enable tracking recently opened files
  (delete-selection-mode)
  (winner-mode) ; Undo/redo window configs with C-c <left>/<right>
  (size-indication-mode)
  (windmove-default-keybindings) ; Move between windows with Shift+arrows
  (add-hook 'after-save-hook 'check-parens)
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold 800000)
              (message "gc-cons-threshold restored to %.2f MB."
                       (/ gc-cons-threshold 1048576.0))))
  (defun dimagid/find-user-config-org-file ()
    "Find Emacs config user README.org file in another window."
    (interactive)
    (find-file-other-window (concat "~/.config/emacs/README.org")))
  (defun modi/multi-pop-to-mark (orig-fun &rest args)
    "Call ORIG-FUN until the cursor moves.
  Try the repeated popping up to 10 times."
    (let ((p (point)))
      (dotimes (i 10)
	(when (= p (point))
          (apply orig-fun args)))))
  (advice-add 'pop-to-mark-command :around #'modi/multi-pop-to-mark))

;; Colorful and legible themes
(use-package ef-themes
  :ensure t
  :init
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (ef-themes-select 'ef-owl)
  (setq ef-themes-mixed-fonts t ; allow spacing-sensitive constructs
        ef-themes-variable-pitch-ui t))

;; Macro-aware go-to-definition for elisp.
(use-package elisp-def
  :ensure t
  :defer t)

;; Elisp API Demos.
(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'describe-function-1
	      :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update
	      :after #'elisp-demos-advice-helpful-update))

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
			  (match-string 1)))) ; the ( is a kludge in README.org
      (with-current-buffer (find-file-noselect file-to-load)
	(save-buffer)))
    (ert-delete-all-tests)
    (eval-buffer)
    (ert 't))
  (defun dimagid/elisp-eval-and-comment ()
    "Evaluate a Lisp expression and insert its value
 as a comment at the end of the line.
 Useful for documenting values or checking values."
    (interactive)
    (save-excursion
      (backward-sexp)
      (-let [result
             (thread-last (thing-at-point 'sexp)
			  read-from-string
			  car
			  eval
			  (format " ;; ⇒ %s"))]
	(forward-sexp)
	(end-of-line)
	(insert result))))
  :bind (:map emacs-lisp-mode-map
	      ("C-c b" . dimagid/elisp-ert-run-tests-in-buffer)
	      ("C-c ;" . dimagid/elisp-eval-and-comment)
	      ("M-." . elisp-def))
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

;; the Emacs command shell
(use-package eshell
  :defer t
  :config
  (setq eshell-hist-ignoredups 'erase))

;; Peruse file or buffer without editing. Built-in package.
(use-package view
  :hook (Info-mode . (lambda ()
                       (define-key Info-mode-map (kbd "{")
				   'View-scroll-half-page-backward)
                       (define-key Info-mode-map (kbd "}")
				   'View-scroll-half-page-forward))))

;; Syntax highlighting of known Elisp symbols.
(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; Preview completion with inline overlay
(use-package completion-preview
  :bind (:map completion-preview-active-mode-map
	      ("M-n" . completion-preview-next-candidate)
	      ("M-p" . completion-preview-prev-candidate))
  :hook (prog-mode text-mode markdown-mode)
  :config
  (completion-preview-mode)
  (global-completion-preview-mode))

;; Transient user interfaces for various modes.
(use-package casual
  :ensure t)

;; Configure savehist to save minibuffer history. Built-in package.
(use-package savehist
  :config
  (setq savehist-additional-variables '(projectile-project-command-history
					corfu-history
					register-alist
					kill-ring))
  (savehist-mode))

;; Enable marginalia to add completion annotations to existing
;;commands.
(use-package marginalia
  :ensure t
  :config (marginalia-mode))

;; Consulting completing-read
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or
	 ;; recently opened file.
         ("M-s M-b" . consult-buffer)))

;; Conveniently act on minibuffer completions
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export))
  :config
  (define-key icomplete-minibuffer-map (kbd "C-.") nil))

;; Consult integration for Embark
(use-package embark-consult
  :ensure t)

;; Writable grep buffer
(use-package wgrep
  :ensure t)

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

;; Save partial status of Emacs when killed. Built-in package.
(use-package desktop
  :config
  (setq
   desktop-dirname "~/.config/emacs/desktop/"
   desktop-base-file-name "~/.config/emacs/desktop/.emacs.desktop"
   desktop-base-lock-name "~/.config/emacs/desktop/.emacs.desktop.lock")
  (desktop-save-mode))

;; A git porcelain inside Emacs
(use-package magit
  :ensure t
  :defer t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure t
  :defer t
  :config
  (global-diff-hl-mode))

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

;; On-the-fly spell checker. Built-in package.
(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

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

;; Display available keybindings in popup
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Highlight brackets according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Enable auto-fill mode to automatically wrap text
(use-package auto-fill
  :hook
  (prog-mode text-mode markdown-mode)
  :config
  (auto-fill-mode)
  :delight "AF")

;; This package is a minor mode to visualize blanks. Built-in package.
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

;; Dired, the Directory Editor
(use-package dired
  :commands (dired)
  :bind (:map dired-mode-map
	      ("C-o" . casual-dired-tmenu) ; casual-dired transient menu
	      ("s" . casual-dired-sort-by-tmenu)
	      ("/" . casual-dired-search-replace-tmenu)
	      ("<tab>" . dired-subtree-toggle)
	      ("TAB" . dired-subtree-toggle)
	      ("<backtab>" . dired-subtree-remove)
	      ("S-TAB" . dired-subtree-remove))
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   (dired-mode . diff-hl-dired-mode)
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
  :config
  (setq dired-subtree-use-backgrounds nil))

;; Operate on buffers like dired. Built-in package.
(use-package ibuffer
  :bind (:map ibuffer-mode-map
	      ("C-o" . casual-ibuffer-tmenu)))

;; Info package for Emacs. Built-in package.
(use-package info
  :bind
  (:map Info-mode-map
	("C-o" . casual-info-tmenu)))

;; The GNU Emacs calculator. Built-in package.
(use-package calc
  :bind
  (:map calc-mode-map
	("C-o" . casual-calc-tmenu))
  :hook
  (calc-mode . (lambda () (display-line-numbers-mode -1))))

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
;; (Note: RFC 2229 is an informational document. RFC: Request for Comments, a
;; system of Internet documents). Built-in package.
(use-package dictionary
  :bind ("<f7>" . dictionary-lookup-definition)
  :config (setq dictionary-server "dict.org"))

;; Interaction mode for Emacs Lisp. Built-in package.
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

;; The Emacs Client for LSP servers. Built-in package.
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

;; Python's flying circus support for Emacs. Built-in package.
(use-package python
  :bind (:map python-ts-mode-map
	      ("<f5>" . recompile))
  :hook
  ((python-ts-mode . eglot-ensure))
  :mode
  (("\\.py\\'" . python-ts-mode)))

;; Major mode for editing C and similar languages. Built-in package.
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

;; Open project dependencies in jar archives
(use-package jarchive
  :ensure t
  :config
  (jarchive-mode))

;; Major mode for editing Clojure code.
(use-package clojure-mode
  :ensure t
  :hook
  ((clojure-mode . eglot-ensure))
  :config
  (setq cider-eldoc-display-for-symbol-at-point nil))

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

;; Collection of yasnippet snippets
(use-package yasnippet-snippets
  :ensure t
  :defer t
  :config
  (yas-global-mode))

;; Specialized comint.el for running the shell
(use-package shell
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

;; Suggest elisp functions that give the output requested.
(use-package suggest
  :ensure t
  :defer t)

;; Support library for PDF documents
(use-package pdf-tools
  :ensure t
  :defer t
  :config (pdf-tools-install))

;; Insert dummy pseudo Latin text
(use-package lorem-ipsum
  :ensure t)

;; Increase selected region by semantic units.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; An Emacs Atom/RSS feed reader.
(use-package elfeed
  :bind ("C-c w" . elfeed)
  :config
  (define-advice elfeed-search--header (:around (oldfun &rest args))
    "Check if Elfeed database is loaded before searching"
    (if elfeed-db
        (apply oldfun args)
      "No database loaded yet"))
  (setq
   elfeed-db-directory "~/.config/emacs/elfeed"
   elfeed-feeds
   '(("https://planet.emacslife.com/atom.xml" blog emacs)
     ("https://nullprogram.com/feed/" blog emacs)
     ("https://news.ycombinator.com/rss" news)
     ("https://clojure.org/feed.xml" news clojure))))
