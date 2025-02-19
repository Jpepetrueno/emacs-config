;;; init.el --- My Emacs init file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 David Dimagid
;;
;; Author: David Dimagid
;; Created: 29 Nov 2024
;; URL: https://github.com/Jpepetrueno/emacs-config/blob/main/init.el
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;;<http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; See my Emacs configuration in:
;; https://github.com/Jpepetrueno/emacs-config
;;
;;; Code:

;; Initialize the package system
(package-initialize)

;; Add MELPA repository to Emacs package archives
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

;; Customizations that don't require loading a package
(use-package emacs
  :bind
  ("C-x C-b" . ibuffer)
  ("<down-mouse-8>" . kill-ring-save)
  ("<down-mouse-9>" . yank)
  ("M-n" . scroll-up-line)
  ("M-p" . scroll-down-line)
  ("C-c o e" . dimagid/eshell-other-window)
  ("C-c o f" . dimagid/flyspell-enable-by-mode)
  ("C-c o i" . dimagid/find-user-init-file)
  ("C-c o m" . dimagid/display-buffer-modes)
  ("C-c o p" . use-package-report)
  ("C-c o r" . restart-emacs)
  ("<f9>" . browse-url-chromium)
  (:map emacs-lisp-mode-map
        ("C-c b" . dimagid/elisp-ert-run-tests-in-buffer)
	("C-c j" . dimagid/elisp-eval-and-comment))
  (:map lisp-interaction-mode-map
	("C-c j" . dimagid/elisp-eval-and-comment))
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error-if-file-is-missing)
  :custom
  (load-prefer-newer t)
  (package-install-upgrade-built-in t)
  (visible-bell t) ; Enable visual (flash screen) instead of audible
  (tab-always-indent 'complete)
  (use-short-answers t)
  (require-final-newline t)
  (switch-to-buffer-obey-display-actions t)
  (debugger-stack-frame-as-list t)
  (history-delete-duplicates t)
  (kill-do-not-save-duplicates t)
  (enable-recursive-minibuffers t)
  (set-mark-command-repeat-pop t)
  (global-auto-revert-non-file-buffers t)
  (scroll-preserve-screen-position 1)
  (recentf-max-saved-items 100)
  (shr-width 70) ; Set HTML width to 70
  (default-input-method 'spanish-prefix) ; 'A -> Á, ~N -> Ñ, ~? -> ¿
  (completions-max-height 14)
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (display-buffer-alist
   '(("\\*Occur\\*"
      (display-buffer-reuse-mode-window
       display-buffer-below-selected))))
  :hook
  (after-init . dimagid/restore-gc-cons-threshold)
  (after-save . check-parens)
  :config
  (fido-vertical-mode)
  (repeat-mode)
  (global-prettify-symbols-mode)
  (save-place-mode) ; Enable saving and restoring cursor positions
  (minibuffer-depth-indicate-mode)
  (global-auto-revert-mode)
  (recentf-mode) ; Enable tracking recently opened files
  (delete-selection-mode)
  (size-indication-mode)
  (winner-mode) ; Undo/redo window configs with C-c <left>/<right>
  (windmove-default-keybindings) ; windmove with Shift+arrows
  (defun dimagid/restore-gc-cons-threshold ()
    "Resets to 800000 bytes."
    (setq gc-cons-threshold 800000)
    (message "gc-cons-threshold restored to %d bytes."
	     gc-cons-threshold))
  (defun dimagid/find-user-init-file ()
    "Find Emacs init file in another window."
    (interactive)
    (find-file-other-window (expand-file-name
			     "init.el" user-emacs-directory)))
  (defun dimagid/eshell-other-window ()
    "Open eshell in other window."
    (interactive)
    (let ((buf (eshell)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf)))
  (defun dimagid/count-available-packages ()
    "Show the number of available packages in current Emacs session.
  Available packages include built-in packages and installed
  packages with their dependencies."
    (interactive)
    (let ((pkg-builtins (length package--builtins))
          (pkg-installed (length package-alist)))
      (message "Pkgs: %d (available), %d (built-in), %d (installed & deps)."
	       (+ pkg-builtins pkg-installed)
	       pkg-builtins
	       pkg-installed)))
  (defun dimagid/count-loaded-features ()
    "Show the number of features loaded in the current Emacs session."
    (interactive)
    (message "%d features loaded in the current Emacs session."
	     (length features)))
  (defun dimagid/get-default-browser ()
    "Get the default browser."
    (interactive)
    (let ((default-browser (shell-command-to-string
			    "xdg-settings get default-web-browser")))
      (message (string-trim default-browser))))
  (defun dimagid/flyspell-enable-by-mode ()
    "Enable `flyspell-prog-mode` in prog modes, else `flyspell-mode`."
    (interactive)
    (if (derived-mode-p 'prog-mode)
	(flyspell-prog-mode)
      (flyspell-mode)))
  (defun dimagid/elisp-ert-run-tests-in-buffer ()
    "Delete all loaded tests, evaluate the buffer, and run tests.
  This function deletes all loaded ERT tests, saves the current buffer
  and the file being loaded, evaluates the buffer, and runs all tests."
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
  (defun dimagid/elisp-eval-and-comment ()
    "Evaluate the sexp and insert its value as a ';; ⇒' comment."
    (interactive)
    (save-excursion
      (backward-sexp)
      (let* ((result
              (thread-last (thing-at-point 'sexp)
                           read-from-string
                           car
                           eval
                           (format " ;; ⇒ %s"))))
	(forward-sexp)
	(end-of-line)
	(insert result))))
  (defun dimagid/display-buffer-modes ()
    "Display major and minor modes in the current buffer."
    (interactive)
    (let* ((minor-modes-list
	    (seq-filter (lambda (mode)
                          (and (boundp mode) (symbol-value mode)))
			minor-mode-list))
           (minor-modes-alist
	    (seq-filter (lambda (mode) (symbol-value (car mode))) minor-mode-alist))
           (minor-mode-count-list
	    (length minor-modes-list))
           (minor-mode-count-alist
	    (length minor-modes-alist))
           (modes-list
	    (if minor-modes-list
		(mapconcat #'symbol-name minor-modes-list ", ")
              "None"))
           (modes-alist
	    (if minor-modes-alist
		(mapconcat (lambda (mode) (symbol-name (car mode))) minor-modes-alist ", ")
              "None")))
      (message "Major mode: %s\nMinor modes (%d): %s\nMinor modes in mode line (%d): %s"
               major-mode
               minor-mode-count-list
               modes-list
               minor-mode-count-alist
               modes-alist)))
  (defun modi/multi-pop-to-mark (orig-fun &rest args)
    "Call ORIG-FUN until the cursor moves, up to 10 times."
    (let ((p (point)))
      (dotimes (_ 10)
        (when (= p (point))
          (apply orig-fun args)))))
  (advice-add 'list-packages
	      :before #'package-refresh-contents)
  (advice-add 'pop-to-mark-command
	      :around #'modi/multi-pop-to-mark))

;; Colorful and legible themes
(use-package ef-themes
  :ensure t
  :init
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (ef-themes-select 'ef-owl)
  (setopt ef-themes-to-toggle '(ef-owl ef-eagle)
	  ef-themes-mixed-fonts t ; allow spacing-sensitive constructs
	  ef-themes-variable-pitch-ui t))

;; A dimmer switch for your lighter text
(use-package delight
  :ensure t)

;; SOAP library to access debbugs servers
(use-package debbugs
  :ensure t
  :defer t)

;; Macro-aware go-to-definition for elisp
(use-package elisp-def
  :bind (:map emacs-lisp-mode-map
	      ("M-." . elisp-def))
  :ensure t)

;; Elisp API Demos
(use-package elisp-demos
  :ensure t
  :config
  (advice-add 'describe-function-1
	      :after 'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update
	      :after 'elisp-demos-advice-helpful-update))

;; Suggest elisp functions that give the output requested
(use-package suggest
  :ensure t
  :defer t)

;; A package-lint Flymake backend
(use-package package-lint-flymake
  :ensure t
  :hook (emacs-lisp-mode . package-lint-flymake-setup))

;; Increase selected region by semantic units
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Highlight escape sequences
(use-package highlight-escape-sequences
  :ensure t
  :config
  (add-to-list 'hes-mode-alist
	       '(lisp-interaction-mode . "\\(\\\\\\(u[[:xdigit:]]\\{4\\}\\|U00[[:xdigit:]]\\{6\\}\\|x[[:xdigit:]]+\\|[0-7]+\\|.\\)\\)"))
  (hes-mode))

;; Enchanted Spell Checker
(use-package jinx
  :ensure t
  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages)
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US es_ES")
  :delight)

;; The Emacs command shell. Built-in package.
(use-package eshell
  :defer t
  :custom
  (eshell-hist-ignoredups 'erase)
  (eshell-cmpl-ignore-case t))

;; Emulate A Terminal, in a region, in a buffer and in Eshell
(use-package eat
  :ensure t
  :after eshell
  :config (eat-eshell-mode)
  :custom (eshell-visual-commands '()))

;; Info package for Emacs. Built-in package.
(use-package info
  :bind (:map Info-mode-map
	      ("C-o" . casual-info-tmenu))
  :config
  (setopt Info-hide-note-references nil))

;; Peruse file or buffer without editing. Built-in package.
(use-package view
  :bind (:map Info-mode-map
              ("{" . View-scroll-half-page-backward)
              ("}" . View-scroll-half-page-forward)))

;; Syntax highlighting of known Elisp symbols
(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; Opinionated Transient-based keyboard interfaces for Emacs modes
(use-package casual
  :ensure t
  :defer t)

;; Save minibuffer history. Built-in package.
(use-package savehist
  :config
  (setopt savehist-additional-variables '(register-alist
					  kill-ring))
  (savehist-mode))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :ensure t
  :config (marginalia-mode))

;; Consulting "completing-read"
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
	 ("C-c i c" . consult-info-completion)
	 ("C-c i e" . consult-info-emacs)
	 ("C-c i g" . consult-info-magit)
         ("C-c i i" . consult-info)
         ("C-c i o" . consult-info-org)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'            ;; previous bindings
	 ("C-x M-:" . consult-complex-command)     ;; repeat-complex-command
	 ("C-x b" . consult-buffer)                ;; switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; switch-to-buffer-other-frame
	 ("C-x t b" . consult-buffer-other-tab)    ;; switch-to-buffer-other-tab
	 ("C-x r b" . consult-bookmark)            ;; bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)      ;; abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)            ;; yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)           ;; Alternative: consult-flycheck
	 ("M-g M-n" . flymake-goto-next-error)
	 ("M-g M-p" . flymake-goto-prev-error)
         ("M-g g" . consult-goto-line)         ;; goto-line
         ("M-g M-g" . consult-goto-line)       ;; goto-line
         ("M-g o" . consult-outline)           ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)             ;; imenu
         ("M-g I" . consult-imenu-multi)       ;; imenu
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)              ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s i" . consult-recent-file)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)           ;; Enable consult-line with isearch
         ("M-s L" . consult-line-multi)     ;; Enable consult-line with isearch
         ;; Minibuffer history
         ;; :map minibuffer-local-map
         ;; ("M-s" . consult-history)       ;; next-matching-history-element
	 )
  ;; Enable automatic preview at point in the *Completions*
  ;; buffer. This is relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This
  ;; improves the register formatting, adds thin separator lines,
  ;; register sorting and hides the window mode line.
  (advice-add #'register-preview :override 'consult-register-window)
  (setopt register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function 'consult-xref
          xref-show-definitions-function 'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize'
  ;; macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setopt consult-narrow-key "<") ;; "C-+"
  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key
  ;; instead.
  (keymap-set consult-narrow-map
	      (concat consult-narrow-key " ?")
	      'embark-prefix-help-command)
  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl"))
  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))
  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "embark" "ido"))
  (defun consult-info-magit ()
    "Search through completion info pages."
    (interactive)
    (consult-info "magit" "magit-section")))

;; Conveniently act on minibuffer completions
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export)
   :map icomplete-minibuffer-map
   ("C-." . embark-act))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setopt prefix-help-command 'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult integration for Embark
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Writable grep buffer
(use-package wgrep
  :ensure t
  :defer t)

;; Manage how Nerd Fonts formats icons
(use-package nerd-icons
  :ensure t)

;; Add icons to completion candidates
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook 'nerd-icons-completion-marginalia-setup))

;; Shows icons for each file in dired mode
(use-package nerd-icons-dired
  :ensure t
  :hook dired-mode
  :delight)

;; Display nerd icons in ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;; Save partial status of Emacs when killed. Built-in package.
(use-package desktop
  :config
  (setopt
   desktop-dirname
   (expand-file-name "desktop" user-emacs-directory)
   desktop-base-file-name
   (expand-file-name ".emacs.desktop" desktop-dirname)
   desktop-base-lock-name
   (expand-file-name ".emacs.desktop.lock" desktop-dirname))
  (desktop-save-mode))

;; A git porcelain inside Emacs
(use-package magit
  :ensure t
  :defer t
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons))

;; Highlight uncommitted changes on the side of a window using VC
(use-package diff-hl
  :ensure t
  :defer t
  :config
  (global-diff-hl-mode))

;; Auto-insertion, wrapping, and navigation for custom pairs.
(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode)
  :custom
  (sp-highlight-pair-overlay nil)
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
  (setopt undo-tree-auto-save-history t)
  (global-undo-tree-mode 1)
  :delight)

;; Display available keybindings in popup
(use-package which-key
  :ensure t
  :config (which-key-mode)
  :delight)

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Enable auto-fill mode to automatically wrap text
(use-package auto-fill
  :hook
  (prog-mode text-mode markdown-mode)
  :config
  (auto-fill-mode)
  :delight)

;; Visualize blanck (TAB, (HARD) SPACE and NEWLINE). Built-in package.
(use-package whitespace
  :hook (text-mode markdown-mode)
  :delight)

;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros
  :ensure t
  :config (eros-mode))

;; Manage files and directories. Built-in package.
(use-package dired
  :bind
  (:map dired-mode-map
	("C-o" . casual-dired-tmenu) ; casual-dired transient menu
	("/" . casual-dired-search-replace-tmenu)
	("<tab>" . dired-subtree-toggle)
	("TAB" . dired-subtree-toggle)
	("<backtab>" . dired-subtree-remove)
	("S-TAB" . dired-subtree-remove)
	("J" . dired-preview-mode)
        ("r" . wdired-change-to-wdired-mode)
	("s" . casual-dired-sort-by-tmenu))
  :hook
  (dired-mode . (lambda ()
                  (dired-hide-details-mode)
                  (hl-line-mode)
                  (diff-hl-dired-mode)
                  (dired-omit-mode)))
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  (wdired-allow-to-change-permissions t))

;; Manage and navigate projects in Emacs easily
(use-package dired-subtree
  :ensure t
  :defer t
  :after dired
  :config
  (setopt dired-subtree-use-backgrounds nil))

;; Automatically preview file at point in Dired
(use-package dired-preview
  :ensure t
  :defer t
  :after dired)

;; Operate on buffers like dired. Built-in package.
(use-package ibuffer
  :bind (:map ibuffer-mode-map
	      ("C-o" . casual-ibuffer-tmenu)))

;; Incremental search. Built-in package.
(use-package isearch
  :bind (:map isearch-mode-map
	      ("C-o" . casual-isearch-tmenu)))

;; The GNU Emacs calculator. Built-in package.
(use-package calc
  :bind (:map calc-mode-map
	      ("C-o" . casual-calc-tmenu))
  :hook
  (calc-mode . (lambda () (display-line-numbers-mode -1))))

;; Viewing/editing system trash can.
(use-package trashed
  :ensure t
  :defer t
  :config
  (setq trashed-action-confirmer #'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; Client for accessing dictionary servers.  Built-in package.
(use-package dictionary
  :bind ("<f7>" . dictionary-lookup-definition)
  :config (setopt dictionary-server "dict.org"))

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
  :bind
  (:map eglot-mode-map
	("C-c d" . eldoc)
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

;; Tool for interacting with LLMs
(use-package ellama
  :bind ("C-c e" . ellama-transient-main-menu)
  :init
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function
	  #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function
	  #'display-buffer-at-bottom)
  (setopt ellama-language "Spanish")
  :config
  ;; set ellama-long-lines-length to fill-column
  (setopt ellama-long-lines-length fill-column)
  :hook
  (ellama-session-mode . (lambda () (whitespace-mode -1))))

;; Major mode for editing shell scripts. Built-in package.
(use-package sh
  :hook
  (bash-ts-mode . flymake-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode)))

;; Python's flying circus support for Emacs. Built-in package.
(use-package python
  :bind (:map python-ts-mode-map
	      ("<f5>" . recompile)
	      ("C-c p" . live-py-mode))
  :hook
  (python-ts-mode . eglot-ensure)
  :mode
  ("\\.py\\'" . python-ts-mode))

;; Live Coding in Python
(use-package live-py-mode
  :ensure t
  :defer t)

;; Major mode for editing C and similar languages. Built-in package.
(use-package cc-mode
  :bind (:map c-mode-map
	      ("<f5>" . recompile))
  :hook
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :mode
  ("\\.c\\'" . c-mode)
  ("\\.cpp\\'" . c++-mode)
  ("\\.h\\'" . c-mode)
  ("\\.hpp\\'" . c++-mode))

;; Open project dependencies in jar archives
(use-package jarchive
  :ensure t
  :defer t
  :config (jarchive-mode))

;; Major mode for editing Clojure code
(use-package clojure-mode
  :ensure t
  :hook
  (clojure-mode . eglot-ensure)
  :config
  (setopt cider-eldoc-display-for-symbol-at-point nil))

;; A better *help* buffer that provides more contextual information
(use-package helpful
  :ensure t
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-c C-d" . helpful-at-point)
  ("C-h F" . helpful-function))

;; A cornucopia of useful interactive commands
(use-package crux
  :ensure t
  :bind
  ("M-o" . crux-smart-open-line)
  ("M-O" . crux-smart-open-line-above)
  (:map ctl-x-4-map
	("t" . crux-transpose-windows)))

;; Track command frequencies
(use-package keyfreq
  :ensure t
  :config
  (setopt keyfreq-excluded-commands
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

;; Pulse highlight on demand or after select functions
(use-package pulsar
  :ensure t
  :defer 1
  :custom
  (pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  :config
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
  (defun my-shell-mode-kill-buffer-on-exit (process state)
    "Kill the shell buffer when the process exits.
  This function is intended to be used as a hook for `shell-mode'.
  It checks if the PROCESS has exited (either normally or abnormally)
  based on the STATE string, and kills the buffer if so."
    (message "%s" state)
    (if (or
	 (string-match "exited abnormally with code.*" state)
	 (string-match "finished" state))
	(kill-buffer (current-buffer))))
  (defun my-shell-mode-hook-func ()
    "Set up `my-shell-mode-kill-buffer-on-exit' as the sentinel.
  This function is intended to be used as a hook for `shell-mode'.
  It configures the current shell buffer to automatically kill itself
  when the shell process exits."
    (set-process-sentinel (get-buffer-process (current-buffer))
			  'my-shell-mode-kill-buffer-on-exit)))

;; Support library for PDF documents
(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config (pdf-loader-install))

;; An Emacs Atom/RSS feed reader.
(use-package elfeed
  :ensure t
  :bind ("C-c w e" . elfeed)
  :custom
  (elfeed-show-truncate-long-urls nil)
  :config
  (define-advice elfeed-search--header (:around (oldfun &rest args))
    "Check if Elfeed database is loaded before searching."
    (if elfeed-db
        (apply oldfun args)
      "No database loaded yet"))
  (setopt elfeed-db-directory
	  (expand-file-name "elfeed" user-emacs-directory)
	  elfeed-show-entry-switch #'display-buffer
	  elfeed-feeds
	  '(("https://planet.emacslife.com/atom.xml" blog emacs)
	    ("https://nullprogram.com/feed/" blog emacs)
	    ("https://news.ycombinator.com/rss" news)
	    ("https://clojure.org/feed.xml" news clojure)
	    ("https://lucidmanager.org/tags/emacs/index.xml" blog emacs)
	    ("https://www.reddit.com/r/emacs/.rss" reddit emacs))))

;; Access Google Translate without tracking via lingva.ml.
(use-package lingva
  :ensure t
  :defer t
  :bind ("<f8>" . lingva-translate)
  :hook
  (lingva-mode . (lambda () (fill-region (point-min) (point-max))))
  :config
  (setq lingva-source "auto"
        lingva-target "es"))

;; Displays Emacs startup stats.
(message "Emacs initialized in %s with %d garbage collections."
	 (emacs-init-time)
	 gcs-done)

;; Show the number of built-in, installed, and dependency of available
;; packages.
(dimagid/count-available-packages)

;; Show the number of loaded features in the current Emacs session.
(dimagid/count-loaded-features)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)


;;; init.el ends here
