;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
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

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1.0))

;; general is used for custom key bindings
(use-package general
  :config
  (general-evil-setup))

;; vim-easymotion movements
(use-package avy
  :config
  (setq avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o)
        avy-background nil))

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-shift-width 4)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

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
  :custom
  (evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

  (define-key evil-visual-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-visual-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match t)      
  :init
  (global-corfu-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '(("~/code/" . 2))
        projectile-switch-project-action 'consult-fd))

(use-package perspective
  :after consult
  :init
  (persp-mode)

  ;; Add perspective mode source to buffer switcher
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)

  :custom
  (persp-mode-prefix-key (kbd "C-p")))

(use-package persp-projectile
  :after '(projectile perspective))

(use-package magit
  :ensure t
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

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
  :config
  (setq vterm-timer-delay 0.01))
(use-package multi-vterm
  :after vterm
  :config
  (setq multi-vterm-dedicated-window-height nil
        multi-vterm-dedicated-window-height-percent 50))

(use-package docker
  :ensure t
  :init
  (setenv "DOCKER_DEFAULT_PLATFORM" "linux/amd64"))

(use-package docker-compose-mode)
(use-package dockerfile-mode)

(use-package hydra)
(defhydra hydra-windows (:hint nil :rows 1)
  "Window Navigation..."
  ;; resizing windows
  ("<left>" evil-window-decrease-width)
  ("<up>" evil-window-increase-height)
  ("<down>" evil-window-decrease-height)
  ("<right>" evil-window-increase-width)

  ;; movement on a laptop
  ("h" evil-window-left)
  ("j" evil-window-down)
  ("k" evil-window-up)
  ("l" evil-window-right)

  ;; make windows  
  ("v" evil-window-vsplit)
  ("s" evil-window-split)
  ("q" evil-window-delete))

(use-package vertico
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.2
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)

  ;; set find args
  (setq consult-fd-args '((if (executable-find "fdfind" 'remote)
                              "fdfind" "fd")
                          "--hidden --full-path --color=never"
                          ;; ignores
                          "--exclude .git"
                          "--exclude .spack_env"
                          "--exclude .cache"))


  ;; Disable automatic preview for these commands
  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; LSP integration for consult
(use-package consult-lsp)

;; Use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init (marginalia-mode))

;; required for the searches
(use-package ripgrep)

;; Minibuffer actions
(use-package embark

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Consult todos
(use-package consult-todo)

(use-package markdown-mode
  :init
  (setq markdown-list-indent-width 2))

(use-package grip-mode
  :hook (markdown-mode . grip-mode)
  :config
  (setq grip-use-mdopen t
        grip-mdopen-path "/Users/i34866/.cargo/bin/mdopen"
        grip-preview-use-webkit nil
        grip-update-after-change nil))

;; i forget what this does
(use-package command-log-mode)

(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (setq treesit-font-lock-level 4)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook
            #'tree-sitter-hl-mode))

(use-package eglot)
;; formatting for most lanugages
(use-package format-all
  :hook (prog-mode . format-all-mode))

;; Cmake
(use-package cmake-mode)
;; editing justfiles
(use-package just-mode)

;; C/C++
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls")))

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

(use-package go-mode)

(use-package lua-mode)

(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package dune
  :ensure t)

;; Merlin configuration
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :hook ((tuareg-mode) . merlin-eldoc-setup))

;; This uses Merlin internally
(use-package flycheck-ocaml
  :config
  (flycheck-ocaml-setup))

;; Built-in Python utilities
(use-package python
  :hook ((python-mode . format-all-mode))
  :custom
  (python-shell-interpreter "python3")
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (pyvenv-mode 1))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp-deferred

(use-package protobuf-mode)

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

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Corfu settings
  (tab-always-indent 'complete)

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

(setq native-comp-async-report-warnings-errors nil)
(set-fringe-mode 10)        ; Give some breathing room
(xterm-mouse-mode)          ; enable mouse control in terminal
(global-hl-line-mode)       ; cursor line
(electric-pair-mode)        ; auto pairs
(electric-indent-mode)      ; auto indent
(setq vc-follow-symlinks t) ; auto follow VC links
(setq indicate-empty-lines t)
(setq inhibit-startup-message t)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq split-height-threshold 80
      split-width-threshold 160)
(setq compilation-scroll-output t)

;; column numbers
(setq-default display-line-numbers 'relative
              display-line-numbers-mode 'relative
              global-display-line-numbers-mode 'relative)

;; disable line numbers in certain modes
(dolist (mode '(org-mode-hook
                markdown-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;; only "y or n" prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable file backups
(setq backup-inhibited t)
(setq auto-save-default nil)

;; expand tabs into spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; use the faster programs
(setq find-program "fd"
      grep-program "rg")

;; keybindings
(general-create-definer qqh/leader-definer
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(defun qqh/unset-leader (key)
  (general-unbind
    :keymaps '(normal visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"
    key))


;; remove the help binding
(global-unset-key (kbd "C-h"))

;; defines leader key bindings
(qqh/leader-definer
  ;; top level bindings
  "SPC" 'consult-buffer
  "TAB" 'other-window
  "RET" 'avy-goto-char-2
  "q" 'qqh/kill-current-buffer
  "g" 'magit
  "x" 'execute-extended-command
  "," 'evil-switch-to-windows-last-buffer
  ":" 'eval-expression

  ;; search (s)
  "s RET" 'avy-goto-line
  "ss" 'consult-line
  "sS" 'consult-line-multi

  ;; files
  "ff" 'consult-fd
  "fo" 'find-file

  ;; Open (o)
  "of" 'dired
  "oi" 'consult-imenu
  "ot" 'multi-vterm-project
  "oT" 'multi-vterm
  "od" 'consult-lsp-diagnostics

  ;; projects (p)
  "pp" 'projectile-persp-switch-project
  "pa" 'projectile-find-other-file
  "pc" 'projectile-commander

  ;; code
  "cc" 'compile
  "cC" 'compile-interactive
  "ch" 'man
  "cd" 'docker

  ;; toggles (t)
  "tr" 'qqh/toggle-relative-line

  ;; windows
  "w" 'hydra-windows/body

  ;; emacs (;)
  ";r" 'qqh/emacs-reload
  ";c" 'qqh/open-emacs-config
  ;; lisp eval
  ";l SPC" 'eval-last-sexp
  ";ll" 'eval-region
  ;; global org bindings (;o)
  ";oa" 'org-agenda
  ";oc" 'org-roam-capture
  ";ol" 'org-roam-node-insert
  ";on" 'org-roam-node-find
  )

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

(general-def
  :states '(normal)
  ;; evil LSP keybindings
  ;; "gd" 'evil-goto-definition <-- built in
  "gr" 'lsp-ui-peek-find-references
  "gR" 'lsp-rename
  "g SPC" 'lsp-execute-code-action)

(general-def
  :states '(normal visual)
  "K" 'lsp-ui-doc-glance
  ;; move through diagnostics
  "]d" 'flycheck-next-error
  "[d" 'flycheck-previous-error
  ;; git conflicts
  "]x" 'smerge-vc-next-conflict)

(general-def
  :states '(normal visual insert emacs)
  "M-[" 'persp-prev
  "M-]" 'persp-next)

(general-def
  :states '(normal visual insert)
  "<f8>" 'multi-vterm-dedicated-toggle)

;; unbind keys
(general-unbind
  :states '(normal visual emacs insert)
  "C-p" ;; used for the persp-mode map
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

(use-package catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(catppuccin-reload)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; nicer compilation window
(use-package fancy-compilation
  :commands (fancy-compilation-mode))

(with-eval-after-load 'compile
  (fancy-compilation-mode))

;; icons
(use-package all-the-icons)
