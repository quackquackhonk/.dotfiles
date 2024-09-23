;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq default-frame-alist '((undecorated-round . t)))
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(setq native-comp-async-report-warnings-errors nil)
(set-fringe-mode 10)        ; Give some breathing room
(xterm-mouse-mode)          ; enable mouse control in terminal
(global-hl-line-mode)       ; cursor line
(electric-pair-mode)        ; auto pairs
(electric-indent-mode)      ; auto pairs
(setq vc-follow-symlinks t) ; auto follow VC links
(setq indicate-empty-lines t)
(setq inhibit-startup-message t)

;; column numbers
(setq display-line-numbers 'relative)
(global-display-line-numbers-mode 'relative)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; only "y or n" prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable file backups
(setq backup-inhibited t)
(setq auto-save-default nil)

;; expand tabs into spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; toggles relative column numbers
(defun qqh/toggle-relative-line ()
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
      (setq display-line-numbers 'relative)))

(defun qqh/emacs-reload ()
  (interactive)
  (load-file user-init-file))
(defun qqh/open-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))

(defun qqh/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1.0))

;; general is used for custom key bindings
(use-package general
  :config
  (general-evil-setup))

(use-package vertico
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; set find args
  (setq consult-fd-args '((if (executable-find "fdfind" 'remote)
                              "fdfind" "fd")
                          "--hidden --full-path --color=never"))

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key 'any)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;; Use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Use `swiper' for interactive buffer search.
(use-package swiper
  :commands (swiper)
  :config
  (setq swiper-goto-start-of-match t))

;; required for the searches
(use-package ripgrep)

;; company for text auto completion
(use-package company
  :commands (company-complete-common company-dabbrev)
  :config
  (global-company-mode)

  ;; Increase maximum number of items to show in auto-completion. Why?
  ;; .. seeing more at once gives you a better overview of your options.
  (setq company-tooltip-limit 40)

  ;; Don't make abbreviations lowercase or ignore case. Why?
  ;; .. many languages are case sensitive, so changing case isn't helpful.
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  ;; Key-map: hold Control for Vim motion. Why?
  ;; .. we're already holding Control, allow navigation at the same time.
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-e") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)

  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous))


(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-shift-width 2)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; define movements to be accessed by Meta + key on colemak
  (general-def
    :states '(normal visual insert)
    "M-m" 'evil-backward-char
    "M-n" 'evil-next-visual-line
    "M-e" 'evil-previous-visual-line
    "M-i" 'evil-forward-char
    ;; Window movement
    "<C-left>" 'evil-window-left
    "<C-right>" 'evil-window-right
    "<C-up>" 'evil-window-up
    "<C-down>" 'evil-window-down
    "C-q" 'evil-window-delete)

  ;; Universal argument: C-u -> C-l
  (global-unset-key (kbd "C-l"))
  (general-define-key
   "C-l" 'universal-argument)
  (general-define-key
   :keymaps 'universal-argument-map
   "C-l" 'universal-argument-more)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "<C-a>") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "<C-x>") 'evil-numbers/dec-at-pt))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :after evil
  :config
  (evil-define-key '(normal) evil-snipe-local-mode-map
    "s" 'evil-snipe-s
    "S" 'evil-snipe-S)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; vim-easymotion movements
(use-package avy
  :config
  (setq avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o)
        avy-background t))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

(use-package magit
  :ensure t)

(require 'org)

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org/"))
  (org-roam-db-autosync-mode))

(setq org-directory "~/org")

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "KILLED(k)")))
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Nice bullets
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

(setq org-hide-emphasis-markers t
      org-pretty-entities t
      org-startup-indented t)

(general-define-key
 :prefix "SPC"
 :keymaps 'org-mode-map
 :states '(normal visual)
 "/is" 'org-insert-structure-template)

;; vterm as a terminal
(use-package vterm
  :ensure t
  :config
  (setq vterm-timer-delay 0.01))
(use-package multi-vterm
  :ensure t
  :after vterm)

(use-package docker
  :ensure t)

(use-package docker-compose-mode)
(use-package dockerfile-mode)

;; i forget what this does
(use-package command-log-mode)

;; formatting for most lanugages
(use-package format-all)

(use-package hydra)
(defhydra hydra-windows (:hint nil :rows 1)
  "Window Navigation..."
  ;; resizing windows
  ("<left>" evil-window-decrease-width)
  ("<up>" evil-window-increase-height)
  ("<down>" evil-window-decrease-height)
  ("<right>" evil-window-increase-width)
  ;; make windows  
  ("v" evil-window-vsplit)
  ("s" evil-window-split)
  ("q" evil-window-delete))

;; keybindings
(general-create-definer qqh/leader-definer
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

;; defines leader key bindings
(qqh/leader-definer
  ;; top level bindings
  "SPC" 'consult-buffer
  "TAB" 'other-window
  "x" 'execute-extended-command
  "," 'switch-to-prev-buffer
  "." 'switch-to-next-buffer
  "q" 'qqh/kill-current-buffer
  ;; misc (;)
  ";r" 'qqh/emacs-reload
  ";c" 'qqh/open-emacs-config
  ;; global org bindings
  ";oa" 'org-agenda
  ";oc" 'org-roam-capture
  ";ol" 'org-roam-node-insert
  ";on" 'org-roam-node-find
  ;; toggles (t)
  "tr" 'qqh/toggle-relative-line
  ;; projectile (p)
  "p" 'projectile-command-map
  ;; LSP
  "ld" 'lsp-find-definition
  "lr" 'lsp-ui-peek-find-references
  "lR" 'lsp-rename
  "lI" 'lsp-ui-imenu
  "l SPC" 'lsp-execute-code-action
  ;; code
  "cc" 'compile
  "cC" 'compile-interactive
  "ce" 'eval-defun
  "ch" 'man
  "cd" 'docker
  ;; windows
  "w" 'hydra-windows/body
  ;; git bindings
  "g" 'magit
  ;; files
  "ff" 'find-file
  "fp" 'counsel-rg
  "fs" 'swiper
  "fq" 'kill-buffer
  ;; Open (o)
  "of" 'dired
  "ot" 'multi-vterm
  ;; Terminal
  "tt" 'multi-vterm-dedicated-toggle)

(general-def
  :states '(normal visual)
  "RET" 'avy-goto-char-2)

(general-def
  :states '(normal visual insert)
  "<f8>" 'multi-vterm-dedicated-toggle)

(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook
            #'tree-sitter-hl-mode))

;; syntax highlighting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq
   lsp-warn-no-matched-clients nil
   lsp-auto-execute-action nil)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]spack_env\\'"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq
    ;; sideline congfig
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-sideline-show-diagnostics t
    lsp-ui-sideline-delay 0.2
    ;; documentation settings
    lsp-ui-doc-enable t
    lsp-ui-doc-show-with-cursor nil
    lsp-ui-doc-show-with-mouse nil
    ;; Themeing
    lsp-lens-enable nil
    lsp-headerline-breadcrumb-enable nil
    lsp-modeline-diagnostics-enable t
    lsp-modeline-code-actions-enable t))


(use-package dap-mode
  :ensure t
  :defer t
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; C/C++
(use-package ccls
  :after projectile
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package haskell-mode)
(use-package lsp-haskell)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(general-define-key
 :prefix "SPC"
 :keymaps 'haskell-mode-map
 :states '(normal visual)
 "/f" 'format-all-buffer
 "/l" 'haskell-process-load-file)

;; GLSL
(use-package glsl-mode)

(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune
  :ensure t)

;; Merlin configuration
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

;; Built-in Python utilities
(use-package python
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  (dap-python-executable "python3")
  (python-shell-interpreter "python3")
  :config
  (require 'dap-python)
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

;; Required to easily switch virtual envs 
;; via the menu bar or with `pyvenv-workon` 
;; Setting the `WORKON_HOME` environment variable points 
;; at where the envs are located. I use miniconda. 
(use-package pyvenv
  :ensure t
  :defer t
  :config
  ;; Setting work on to easily switch between environments
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs/"))
  ;; Display virtual envs in the menu bar
  (setq pyvenv-menu t)
  ;; Restart the python process when switching environments
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
            (pyvenv-restart-python)))
  :hook (python-mode . pyvenv-mode))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp-deferred

;; SML
(use-package sml-mode
  :config
  (setq sml-indent-level 2))

(use-package racket-mode
    :hook ((racket-mode . format-all-mode)
           (racket-mode . racket-xp-mode)))
(general-define-key
 :prefix "SPC"
 :keymaps 'racket-mode-map
 :states '(normal visual)
 ;; language bindings
 "ld" 'xref-find-definitions
 "lr" 'xref-find-references
 "lR" 'racket-xp-rename
 ;; racket bindings
 "/f" 'format-all-buffer
 "/r" 'racket-run-and-switch-to-repl
 "/R" 'racket-run-module-at-point)

(general-nmap
 :keymaps 'racket-mode-map
 "K" 'racket-xp-describe)

;; RUST
(use-package rust-mode
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . flycheck-mode))
  :config
  ;; rustfmt
  (setq rust-format-show-buffer nil)
  (setq rust-format-on-save t))

(use-package cargo
  :after rust-mode)

;; Cmake
(use-package cmake-mode)
;; editing justfiles
(use-package just-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

(use-package catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(catppuccin-reload)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode)       ; highlight TODO
  (setq hl-todo-keyword-faces
    '(("TODO"   . "#fabd2f")
      ("FIXME"  . "#fb4934")
      ("DEBUG"  . "#8ec07c"))))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

; (use-package fancy-compilation
;   :commands (fancy-compilation-mode))

; (with-eval-after-load 'compile
;   (fancy-compilation-mode))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))
