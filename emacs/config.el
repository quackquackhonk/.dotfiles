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

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(xterm-mouse-mode)          ; enable mouse control in terminal
(global-hl-line-mode)       ; cursor line
(menu-bar-mode -1)          ; Disable the menu b
(electric-pair-mode)        ; auto pairs
(electric-indent-mode)      ; auto pairs
(setq visible-bell t)       ; Set up the visible bell
(setq vc-follow-symlinks t) ; auto follow VC links
(setq indicate-empty-lines t)

;; column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

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
(defun my/toggle-relative-line ()
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
      (setq display-line-numbers 'relative)))

(defun my/emacs-reload ()
  (interactive)
  (load-file user-init-file))
(defun my/open-emacs-config ()
  (interactive)
  (find-file "~/.dotfiles/emacs/config.org"))

(defun my/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1.5))

;; general is used for custom key bindings
(use-package general
  :config
  (general-evil-setup))

;; keybindings
(general-create-definer my/leader-definer
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

;; defines leader key bindings
(my/leader-definer
  ;; top level bindings
  "SPC" 'helm-buffers-list
  "x" 'helm-M-x
  "," 'switch-to-prev-buffer
  "." 'switch-to-next-buffer
  "q" 'my/kill-current-buffer
  ;; misc (;)
  ";r" 'my/emacs-reload
  ";c" 'my/open-emacs-config
  ;; toggles (t)
  "tr" 'my/toggle-relative-line
  ;; projectile (p)
  "p" 'projectile-command-map
  ;; LSP
  "ld" 'lsp-find-definition
  "lr" 'lsp-ui-peek-find-references
  "lR" 'lsp-rename
  "lI" 'lsp-ui-imenu
  "le" 'helm-lsp-diagnostics
  "l SPC" 'helm-lsp-code-actions
  ;; windows
  "w" 'hydra-windows/body
  ;; git bindings
  "gg" 'magit
  "gp" 'magit-pull
  ;; files
  "ff" 'find-file
  "fp" 'counsel-rg
  "fs" 'swiper
  "fq" 'kill-buffer)

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "g +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "g -") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package hydra)
(defhydra hydra-windows (:hint nil :rows 1)
  "Window Navigation..."
  ;; navigating windows
  ("<left>" evil-window-left)
  ("<up>" evil-window-up)
  ("<down>" evil-window-down)
  ("<right>" evil-window-right)
  ;; make windows  
  ("v" evil-window-vsplit)
  ("s" evil-window-split)
  ("q" evil-quit))

;; IVY COMPLETION
(use-package ivy
  :demand t
  :config
  (ivy-mode)
  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-done)
  ;; so we can switch away
  (define-key ivy-minibuffer-map (kbd "C-w") 'evil-window-map))
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  ;; completion window settings
  (setq helm-display-header-line nil)
  (helm-autoresize-mode 1)
  (setq helm-autoresize-max-height 25)
  (setq helm-autoresize-min-height 25))

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
  (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-h") 'company-abort)
  (define-key company-active-map (kbd "<C-return>") 'company-complete-selection)

  (define-key company-search-map (kbd "C-j") 'company-select-next)
  (define-key company-search-map (kbd "C-k") 'company-select-previous))

;; Use `swiper' for interactive buffer search.
(use-package swiper
  :commands (swiper)
  :config
  (setq swiper-goto-start-of-match t))

;; Use counsel for project wide searches. Why?
;; .. interactive project wide search is incredibly useful.
(use-package counsel
  :commands (counsel-git-grep counsel-switch-buffer))

;; required for the searches
(use-package ripgrep)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'helm))
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code"))))

(use-package helm-projectile
  :config (helm-projectile-on))

(use-package persp-mode
  :config
    (with-eval-after-load "persp-mode"
      (setq wg-morph-on nil)
      (setq persp-autokill-buffer-on-remove 'kill-weak)
      (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))
    (require 'persp-mode))

(use-package magit
  :ensure t)

;; i forget what this does
(use-package command-log-mode)

;; vterm as a terminal
(use-package vterm
  :ensure t)

;; formatting for most lanugages
(use-package format-all)

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
  (setq lsp-auto-execute-action nil))

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

(use-package helm-lsp)
(use-package helm-xref)

;; C/C++
(use-package ccls
  :after projectile
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp-deferred)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

;; GLSL
(use-package glsl-mode)

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  (setq doom-gruvbox-dark-variant nil)
  (doom-themes-visual-bell-config))

(use-package melancholy-theme)
(use-package gruvbox-theme)

(load-theme 'doom-gruvbox t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package autothemer
  :config
  (setq autothemer--theme 'doom-gruvbox))
