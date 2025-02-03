;;; init.el --- Entry point for my emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Guardrail
(when (< emacs-major-version 30)
  (error "[qqh] config requires Emacs version 30+, currently running %s!" emacs-major-version))

;;; Top Level Definitions
(defgroup qqh nil
  "User options for my Emacs configuration."
  :group 'file)

(defvar qqh/modules-dir (expand-file-name "qqh" user-emacs-directory)
  "The directory containing my module files.")

(defun qqh/macos-p ()
  "Check if the current frame is an OSX gui frame."
  (eq system-type 'darwin))

;;; Straight initialization

;;;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
       (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;; Setup use-package for straight
(use-package straight
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))
  (straight-use-package-by-default t))

;; always load the newest bytecode
(setq load-prefer-newer t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))
;;; Some initial packages
;; Load diminish for :diminish constructs in use-package
(use-package diminish
  :init
  ;; diminish built-in minor modes
  (diminish 'abbrev-mode)
  (diminish 'visual-line-mode)
  (diminish 'smerge-mode))

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Catppuccin for using colors in other packages
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha
	    catppuccin-italic-comments t
	    catppuccin-highlight-matches t)

  (add-hook 'server-after-make-frame-hook #'catppuccin-reload)

  (load-theme 'catppuccin :no-confirm t)
  (catppuccin-reload))

;;; Basic settings
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)             ;; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil)            ;; this information is useless for most
(setopt auto-revert-avoid-polling t)                      ;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-interval 2)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
(setq vc-follow-symlinks t)                               ;; auto follow VC links
(setq indicate-empty-lines t)
(setq inhibit-startup-message t)
(require 'windmove)
(windmove-default-keybindings 'control)                   ;; Move through windows with Control-<arrow keys>
(setopt sentence-end-double-space nil)                    ;; Fix archaic defaults
(defalias 'yes-or-no-p 'y-or-n-p)                         ;; only "y or n" prompts
(global-set-key (kbd "<escape>") 'keyboard-quit)          ;; Make ESC quit prompts
(setq find-program "fd"                                   ;; use the faster programs
      grep-program "rg")
(savehist-mode)                                           ;; Save history of minibuffer
(setopt enable-recursive-minibuffers t)                   ;; Use the minibuffer whilst in the minibuffer
(setopt indent-tabs-mode nil)                             ;; Tabs BTFO
(setopt tab-width 4)
(setq make-backup-files nil)                              ;; Get rid of backup files
(setq backup-inhibited nil)                               ;; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)
;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ;; Emacs 28 with native compilation
  (setq native-compile-prune-cache t))                    ;; Emacs 29
(keymap-set minibuffer-mode-map "TAB"
            'minibuffer-complete)                         ;; TAB acts more like how it does in the shell
(scroll-bar-mode -1)                                      ;; no scrollbars
(blink-cursor-mode -1)                                    ;; Steady cursor
(pixel-scroll-precision-mode)                             ;; Smooth scrolling
(setopt ring-bell-function 'ignore)                       ;; disable the bell
(setopt compilation-scroll-output t)                      ;; follow compilation output by default
(setq frame-resize-pixelwise t)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Don't show trailing whitespace, and delete when saving
(setopt show-trailing-whitespace nil)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;;; Built-Ins.
;;;; Repeat Mode
(use-package repeat
  :config (repeat-mode))

;;;; Dired
(use-package dired
  :straight nil
  :bind (:map dired-mode-map
              ("-" . dired-create-empty-file))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

;;;; Outline-mode
(use-package outline
  :straight nil
  :diminish outline-minor-mode
  :config
  (define-key outline-minor-mode-map (kbd "C-c C-c")
              (lookup-key outline-minor-mode-map (kbd "C-c @")))

  (setq outline-minor-mode-highlight 'append)
  (setq outline-minor-mode-cycle t)

  :bind (:map outline-minor-mode-map
              ("<backtab>" . outline-cycle-buffer)
              ("C-c <tab>" . outline-cycle)
              ("C-c C-n" . outline-next-visible-heading)
              ("C-c C-p" . outline-previous-visible-heading)
              ("C-c C-f" . outline-forward-same-level)
              ("C-c C-b" . outline-backward-same-level)
              ("C-c C-u" . outline-up-heading)
              ("C-c C-a" . outline-show-all)
              ("C-c C-c C-a" . outline-show-all)
              ("<f1>" . outline-toggle-children))
  :hook (emacs-lisp-mode . outline-minor-mode))


;;;; Eshell
(use-package eshell
  :straight nil
  :init
  (defun qqh/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . qqh/setup-eshell)))


;;; Misc. editing enhancements
(use-package avy
  :demand t
  :config
  (setq avy-keys '(?a ?r ?s ?t ?p ?l ?n ?e ?i ?o)
        avy-background nil))

;; faster searching
(use-package ripgrep)

;; Modify search results en masse
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))


;;; Minibuffer & Completion
(use-package embark-consult)

;;;; Embark and Consult
(use-package consult
  :after embark-consult
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
                          "--exclude .cache"
                          "--exclude .mypy_cache"
                          "--exclude devdocs"
                          "--exclude build"))


  :bind (
         ;; Drop-in replacements
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("C-s" . consult-line)         ; Alternative: rebind C-s to use
         ("M-s s" . isearch)            ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi)
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )

  :config
  (consult-customize consult--source-bookmark :hidden t :default :nil)
  (setq-default consult-buffer-sources
                '(consult--source-hidden-buffer
                  consult--source-modified-buffer
                  consult--source-buffer
                  consult--source-recent-file
                  consult--source-file-register
                  ;; Don't show bookmarks in consult-buffer
                  ;; consult--source-bookmark
                  consult--source-project-buffer-hidden
                  consult--source-project-recent-file-hidden))
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :demand t
  :after '(avy embark-consult)
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Add the option to run embark when using avy
  (defun qqh/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'qqh/avy-action-embark))

;; Vertico: better vertical completion for minibuffer commands
;;;; Vertico
(use-package vertico
  :custom
  (vertico-count 30)
  (vertico-resize t)
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;;;; Marginalia: annotations for minibuffer
(use-package marginalia
  :config
  (marginalia-mode))

;;;; Orderless: powerful completion style
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless)))))

;;;; Corfu: Popup completion-at-point
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; auto completion
  (corfu-quit-no-match t)        ;; Quit when no matches
  :init
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  :config
  )

(use-package corfu
  ;; TAB-and-Go customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt

  :bind (:map corfu-map
              ;; Use TAB for cycling, default is `corfu-complete'.
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

;;;;; Corfu popupinfo
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;;;;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;;;;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; Cape: Fancy completion-at-point functions
;; there's too much in the cape package to configure here; dive in when you're comfortable!
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;;; yasnippet: Snippets!
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; for completion-at-point-functions integration
(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet-snippets)

;;;; A few more useful configurations...
(use-package emacs
  :straight nil
  :custom
  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Corfu settings
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

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

;;; Dev

;;;; Built-in dev config
(use-package emacs
  :straight nil
  :config
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package track-changes
  :config
  ;; FIXME: This shouldn't be necessary
  (setq track-changes-record-errors nil))

;;;; Eat: Terminal Emulation
(use-package eat
  :custom
  (eat-term-name "xterm")
  :config
  (unbind-key (kbd "M-'") 'eat-semi-char-mode-map)
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat


;;;; Magit: best Git client to ever exist
(use-package magit)

(use-package forge
  :config
  ;; Configure auth source
  (setq auth-sources '("~/.authinfo")))

;;;; Project Management
;;;;; direnv integration
;; (use-package envrc
;;   :hook (after-init . envrc-global-mode))
(use-package direnv
 :config
 (direnv-mode))
;;;;; Projectile
(use-package project)
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)

  (when (qqh/macos-p)
    (setq projectile-fd-executable "/opt/homebrew/bin/fd"))

  (setq projectile-enable-caching t
        projectile-auto-discover nil
        projectile-project-search-path '(("~/code/" . 2)
					                     "~/sources/")
        projectile-switch-project-action 'qqh/fuzzy-find-file))

(defun qqh/open-project-org-file ()
  "Open the project.org file at the root of the current project. If no project.org file is found, create a new one from a template."
  (interactive)
  (let ((file     (projectile-expand-root "project.org"))
        (template (expand-file-name "templates/project-template.org"
                                    qqh/modules-dir)))
    (unless (file-exists-p file)
      (copy-file template file))
    (find-file file)))

;;;;; Perspective
(use-package perspective
  :after consult
  :custom
  (persp-mode-prefix-key (kbd "C-M-p"))
  :init
  (require 'consult)
  (persp-mode)

  ;; Add perspective mode source to buffer switcher
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;;;;; make them play nice
(use-package persp-projectile
  :config
  (define-key projectile-command-map (kbd "P") 'projectile-persp-switch-project))

;;;;; Consult-todo: Search project todos
(use-package consult-todo)

;;;; Miscellaneous file types

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode))
  :config
  (setq markdown-list-indent-width 2))

(use-package grip-mode
  :after markdown-mode
  :config
  (setq grip-use-mdopen t
        grip-mdopen-path "/Users/i34866/.cargo/bin/mdopen"
        grip-preview-use-webkit nil
        grip-update-after-change nil))

(use-package yaml-mode)
(use-package json-mode)
(use-package protobuf-mode)

(use-package dts-mode
  :mode "\\.keymap\\'")

(use-package just-mode)
(use-package cmake-mode)

;;;; Documentation and Diagnostics

;;;;; Eldoc
(use-package eldoc
  :diminish eldoc-mode
  :config
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose))

;;;;; documentation comment generation
(use-package docstr
  :diminish docstr-mode
  :config (global-docstr-mode 1))

;;;;; Devdocs.io integration
(use-package devdocs
  :bind (("C-h D" . devdocs-lookup))
  :hook ((tuareg-mode . (lambda () (setq-local devdocs-current-docs '("ocaml~5.0"))))
         ((python-mode python-ts-mode) . (lambda () (setq-local devdocs-current-docs '("python~11"))))
         (c-mode . (lambda () (setq-local devdocs-current-docs '("c"))))
         (c++-mode . (lambda () (setq-local devdocs-current-docs '("cpp"))))))

;;;;; Flymake
(use-package flymake
  :straight nil
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-counters flymake-mode-line-exception))
  :custom-face
  (flymake-note ((t :underline ,(catppuccin-color 'green))))
  (flymake-warning ((t :underline ,(catppuccin-color 'yellow))))
  (flymake-error ((t :underline ,(catppuccin-color 'red))))
  :hook (prog-mode . flymake-mode))

;;;; Eglot
(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files
  :hook ((tuareg-mode python-mode python-ts-mode c-mode c++-mode) . eglot-ensure)
  :config
  ;; Disable inlay hints globally
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

  ;; extra server binaries
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  ;; PERF: dont log every event
  (fset #'jsonrpc--log-event #'ignore)

  ;; Remove the eglot indicator from the mode-line-misc-info variable
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info))

  ;; server configurations
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:plugins (:pycodestyle (:enabled :json-false)
                                                    :mccabe (:enabled :json-false)
                                                    :pyflakes (:enabled :json-false)
                                                    :flake8 (:enabled :json-false
                                                                      :maxLineLength 100)
                                                    :pylsp_mypy (:enabled t)
                                                    :ruff (:enabled t
                                                                    :lineLength 100)
                                                    :pydocstyle (:enabled :json-false)
                                                    :yapf (:enabled :json-false)
                                                    :autopep8 (:enabled :json-false)))))))

(use-package eglot-booster
  :init
  (unless (executable-find "emacs-lsp-booster")
    (add-to-list 'exec-path "/home/sahana/.local/bin"))
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;;;; Code formatting...
(use-package format-all)
;;;; Dape: DAP support for eglot
(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  :hook
  ((kill-emacs . dape-breakpoint-save)  ;; Save breakpoints on quit
   (after-init . dape-breakpoint-load)) ;; Load breakpoints on startup

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root))

;;;; Language specific configuration

;;;;; C / C++
(setq-default c-basic-offset 4)

;;;;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")
;;;;; OCaml
;; Major-Mode mode for OCaml programming
(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune)

(use-package utop
  :hook ((tuareg-mode . utop-minor-mode)
         (tuareg-mode . (lambda ()
                          (let* ((p-root (projectile-project-root))
                                 (p-root-str (if p-root p-root ""))
                                 (fname (format "%s." p-root-str))
                                 (rel-dir (file-relative-name fname default-directory))
                                 (cmd (format "dune utop %s -- -emacs" rel-dir)))
                            (setq utop-command cmd))))))

;;;;; PYTHON
;; Built-in Python utilities
(use-package python
  :custom
  (python-shell-interpreter "python3")

  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package blacken
  :defer t
  :hook ((python-mode python-ts-mode) . blacken-mode)
  :custom
  (blacken-allow-py36 t))

(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (pyvenv-mode 1))

;;;;; RUST
(use-package rust-mode
  :config
  ;; rustfmt
  (setq rust-format-show-buffer nil)
  (setq rust-format-on-save t)

  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package cargo
  :after rust-mode)
;;; Org Mode

;;;; Settings
;; Agenda variables
(setq org-directory "~/org/")         ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("inbox.org"))

;; Default tags
(setq org-tag-alist '(;; locale
                      (:startgroup)
                      ("personal" . ?p)
                      ("work" . ?w)
                      (:endgroup)
                      (:newline)
                      ;; scale
                      (:startgroup)
                      ("one-shot" . ?o)
                      ("project" . ?j)
                      ("tiny" . ?t)
                      (:endgroup)
                      ;; misc
                      ("meta")
                      ("resource")
                      ("review")
                      ("reading")))

;; TODO: Org-refile: where should org-refile look?
;; (setq org-refile-targets 'FIXME)

;;;; Org package
(use-package org
  :hook ((org-mode . visual-line-mode))  ; wrap lines at word breaks

  :config

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (setq org-export-with-smart-quotes t                ; Make exporting quotes better
        org-return-follows-link t)                    ; RET follows links

  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in. Run
  ;;     M-x describe-variable RET org-todo-keywords RET
  ;; for documentation on how these keywords work.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  ;; An agenda view lets you see your TODO items filtered and
  ;; formatted in different ways. You can have multiple agenda views;
  ;; please see the org-mode documentation for more information.
  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work" agenda ""
           ((org-agenda-files '("work.org"))))))


  ;; Babel Language activation
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

;;;; Org-roam
(use-package org-roam
  :init
  ;; Org-roam variables
  (setq org-roam-directory "~/org/roam/")
  (setq org-roam-index-file "~/org/roam/index.org")

  (setq org-roam-completion-everywhere nil)
  (setq org-roam-capture-templates
        '(("n" "note" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("m" "meeting" plain "%?"
           :if-new
           (file+head "work/meeting/${title}.org" "#+title: ${title}\n#+filetags: :work-meeting:\n")
           :immediate-finish t
           :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

;;; Themes / UI customization

;;;; Face customizations
(if (qqh/macos-p)
    (set-face-attribute 'default nil
                        :family "Iosevka"
                        :height 130)
  (set-face-attribute 'default nil
                      :family "Iosevka Nerd Font"
                      :height 100))

(set-face-attribute 'window-divider nil
                    :background (catppuccin-color 'base)
                    :foreground (catppuccin-color 'base))
(set-face-attribute 'fringe nil
                    :background (catppuccin-color 'mantle))

;;;; Packages
(use-package colorful-mode
  :diminish colorful-mode)

(use-package nerd-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(use-package hl-todo
  :config
  (defface qqh/hl-todo/todo-face
    `((t . (:bold t :background ,(catppuccin-color 'sky) :foreground ,(catppuccin-color 'base))))
    "The face highlighting TODOs in projects."
    :group 'qqh)

  (setq hl-todo-keyword-faces
        `(("TODO" . ,(catppuccin-color 'sky))
          ("HACK" . ,(catppuccin-color 'peach))
          ("FIXME" . ,(catppuccin-color 'red))
          ("NOTE" . ,(catppuccin-color 'mauve))
          ("PERF" . ,(catppuccin-color 'lavender))))

  (global-hl-todo-mode))

;; dim inactive buffers
(use-package auto-dim-other-buffers
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil
                      :background (catppuccin-color 'mantle))
  (set-face-attribute 'auto-dim-other-buffers-hide-face nil
                      :background (catppuccin-color 'mantle)
                      :foreground (catppuccin-color 'mantle))
  (auto-dim-other-buffers-mode))

(use-package fancy-compilation
  :config
  (set-face-attribute 'fancy-compilation-default-face nil
                      :background (catppuccin-color 'base))
  (fancy-compilation-mode))

;;;; Modeline configurtaion

;;;;; Common
(defgroup qqh/modeline nil
  "User options for my modeline configuration."
  :group 'qqh)

(defgroup qqh/modeline/faces nil
  "Faces for my modeline configuration."
  :group 'qqh/modeline)

(defcustom qqh/modeline/truncation-length 12
  "Length to truncate mode-line strings to in small windows."
  :type 'natnum
  :group 'qqh/modeline)

;;;;; Buffer ID

(defun qqh/modeline/major-mode-name ()
  "Return the major mode of the current buffer with a colon after it."
  (concat (symbol-name major-mode) ":"))

(defface qqh/modeline/faces/bold-yellow
  `((t . (:bold t :foreground ,(catppuccin-color 'yellow))))
  "The face to use for the names of modified buffers on the mode line."
  :group 'qqh/modeline/faces)

(defun qqh/modeline/buffer-name ()
  "Return the name of the current buffer."
  (let ((text " %b")
        (face (if (and (buffer-modified-p)
                       (mode-line-window-selected-p))
                  'qqh/modeline/faces/bold-yellow
                nil)))
    (if face
        (propertize text 'face face)
      text)))

(defvar-local qqh/modeline/flymake
    `(:eval (when (mode-line-window-selected-p)
              flymake-mode-line-format)))

;;;;; Eglot
(defvar-local qqh/modeline/eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode (" [" eglot--mode-line-format "]"))))
  "Mode line construct for reporting eglot status of the current buffer.")

(setq-default mode-line-format
              '("%e%n"
                "  "
                (:eval (meow-indicator))
                " "
                (:eval (qqh/modeline/major-mode-name))
                (:eval (qqh/modeline/buffer-name))
                " "
                mode-line-process

                ;; emacs 30: right align the rest of the modeline
                mode-line-format-right-align         ;; emacs 30
                " "
                (vc-mode vc-mode)
                ;; flymake-mode-line-format
                qqh/modeline/flymake
                qqh/modeline/eglot
                " "
                (:eval (when (mode-line-window-selected-p)
                         mode-line-misc-info))
                "  "))

(dolist (construct '(qqh/modeline/eglot
                     qqh/modeline/flymake))
  (put construct 'risky-local-variable t))

;;;; Buffer display configuration
(use-package popper
  :straight t
  :bind (("C-'"   . popper-toggle)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq
   popper-reference-buffers '("\\*Messages\\*"
                              "\\*eldoc\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              "^\\*eat.*\\*$"
                              help-mode
                              compilation-mode)
   popper-group-function #'popper-group-by-perspective
   popper-echo-dispatch-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))

  (setq popper-window-height (lambda (win)
                               (fit-window-to-buffer
                                win
                                (floor (frame-height) 3)
                                (floor (frame-height) 3))))

  (popper-mode +1)
  ;; echo area hints
  (popper-echo-mode +1))

(add-to-list 'display-buffer-alist
	     '("\\*\\(Compile-Log\\|Async-native-compile-log\\|Warnings\\)\\*"
	       (display-buffer-no-window)
	       (allow-no-window t)))

(add-to-list 'display-buffer-alist
	     '("\\*\\(Ibuffer\\|vc-dir\\|vc-diff\\|vc-change-log\\|Async Shell Command\\)\\*"
	       (display-buffer-full-frame)))

;; Show magit in a full window
(add-to-list 'display-buffer-alist
	     '("\\(magit: .+\\|magit-log.+\\|magit-revision.+\\)"
	       (display-buffer-full-frame)))
;;; Keybindings

;;;; Definitions
(defun qqh/kill-buffer ()
  "Kill the current buffer using the `persp-kill-buffer*' command."
  (interactive)
  (persp-kill-buffer* (current-buffer)))

(defun qqh/fuzzy-find-file ()
  "Fuzzy find a file in the current directory."
  (interactive)
  (consult-fd))

(defun qqh/emacs/reload ()
  "Load my Emacs configuration."
  (interactive)
  (load-file user-init-file))

(defun qqh/emacs/open-config ()
  "Load my Emacs configuration."
  (interactive)
  (find-file user-init-file))

(defun qqh/config/open-nix-flake ()
  "Open my nix flake."
  (interactive)
  (find-file "~/dotfiles/flake.nix"))

(defun qqh/config/open-nix-home ()
  "Open my nix home-manager home.nix."
  (interactive)
  (find-file "~/dotfiles/home-manager/home.nix"))


;;;; surround: surround selections with custom delimiters
(use-package surround)

;;;; transient: so many leader keys
(use-package transient)

;; Set up some transient maps for additional leaders
(transient-define-prefix qqh/transient/leader ()
  "Transient map for my leader bindings.

These bindings are preferred over `meow-leader-define-key', since I have less restrictions here!"
  [["leader bindings..."
    ("<escape>" "quit" transient-quit-one)
    ("SPC" "buffers" consult-buffer)
    ("," "last buffer" meow-last-buffer)
    (":" "eval expression" eval-expression)]
   ["(c)ode..."
    ("cc" "compile" compile)
    ("c SPC" "code action" eglot-code-actions :if (lambda () eglot--managed-mode))
    ("cr" "rename symbol" eglot-rename :if (lambda () eglot--managed-mode))
    ("cf" "format" format-all-region-or-buffer)]
   ["(g)it..."
    ("gg" "git status" magit)
    ("gb" "git branch" magit-branch)
    ("gB" "git blame" magit-blame)]
   ["(n)otes..."
    ("nc" "capture note" org-roam-capture)
    ("ni" "insert note" org-roam-node-insert)
    ("nls" "store link" org-store-link)
    ("nli" "insert link" org-insert-link-global)]
   ["(o)pen..."
    ("od" "open diagnostics panel" consult-flymake)
    ("ot" "open terminal" eat)]
   ["(p)rojects..."
    ("pp" "switch to project" projectile-persp-switch-project)
    ("pd" "project dired" projectile-dired)
    ("pt" "open project terminal" eat-project)]
   ["(s)earch..."
    ("ss" "search files" qqh/fuzzy-find-file)
    ("sn" "search notes" org-roam-node-find)
    ("so" "search outline" consult-outline)
    ("si" "search imenu" consult-imenu)
    ("sl" "search all lines" consult-line-multi)
    ("sp" "search perspectives" persp-switch)]
   ["(;) configuration files.."
    (";r" "reload config" qqh/emacs/reload)
    (";f" "open flake.nix" qqh/config/open-nix-flake)
    (";n" "open home.nix" qqh/config/open-nix-home)
    (";c" "edit config" qqh/emacs/open-config)]])

(transient-define-prefix qqh/transient/g ()
  "Transient map for emulating vim's g- leader keybinding."
  [["Edit"
    ("c" "comment" comment-dwim)]
   ["Find"
    ("d" "definition" xref-find-definitions)
    ("r" "references" xref-find-references)]
   ["Move"
    ("g" "top" beginning-of-buffer)
    ("G" "bottom" end-of-buffer)
    ("l" "line" meow-goto-line)
    ("RET" "2 chars" avy-goto-char-2)]])


(transient-define-prefix qqh/transient/next ()
  "Transient map for going to the next thing."
  [["Next"
    ("d" "todo" hl-todo-next)
    ("e" "error" flymake-goto-next-error)
    ("t" "tab" tab-next)
    ("p" "perspective" persp-next)]])

(transient-define-prefix qqh/transient/prev ()
  "Transient map for going to the previous thing."
  [["Previous"
    ("d" "todo" hl-todo-previous)
    ("e" "error" flymake-goto-prev-error)
    ("t" "tab" tab-previous)
    ("p" "perspective" persp-prev)]])


;;;; Global bindings
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;;;; Meow

(use-package meow
  :straight t
  :demand t
  :custom-face
  (meow-position-highlight-number-1 ((t (:background ,(catppuccin-lighten (catppuccin-color 'mauve) 25)))))
  (meow-position-highlight-number-2 ((t (:background ,(catppuccin-color 'mauve)))))
  (meow-position-highlight-number-3 ((t (:background ,(catppuccin-darken (catppuccin-color 'mauve) 25)))))

  (meow-position-highlight-reverse-number-1 ((t (:background ,(catppuccin-lighten (catppuccin-color 'pink) 25)))))
  (meow-position-highlight-reverse-number-2 ((t (:background ,(catppuccin-color 'pink)))))
  (meow-position-highlight-reverse-number-3 ((t (:background ,(catppuccin-darken (catppuccin-color 'pink) 25)))))
  ;; mode line faces
  (meow-normal-indicator ((t (:bold t :foreground ,(catppuccin-color 'base) :background ,(catppuccin-color 'mauve)))))
  (meow-motion-indicator ((t (:bold t :foreground ,(catppuccin-color 'base) :background ,(catppuccin-color 'red)))))
  (meow-keypad-indicator ((t (:bold t :foreground ,(catppuccin-color 'base) :background ,(catppuccin-color 'peach)))))
  (meow-insert-indicator ((t (:bold t :foreground ,(catppuccin-color 'base) :background ,(catppuccin-color 'green)))))
  (meow-beacon-indicator ((t (:bold t :foreground ,(catppuccin-color 'base) :background ,(catppuccin-color 'sapphire))))))


(defun meow-setup ()
  "Function for setting up meow keybindings."
  ;; colemak-dh cheatsheet
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  ;; register some things
  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair ("<") (">")))

  ;; sets the thing table characters to use ([{ for grouping punctuations
  ;; test ' asrtarstar '
  (setq meow-char-thing-table
        '((?\( . round)
          (?\) . round)
          (?\[ . square)
          (?\] . square)
          (?\{ . curly)
          (?\} . curly)
          (?\< . angle)
          (?\> . angle)
          (?\" . string)
          (?s . symbol)
          (?w . window)
          (?b . buffer)
          (?b . paragraph)
          (?l . line)
          (?v . visual-line)
          (?f . defun)
          (?\. . sentence)))

  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '(":" . meow-keypad)
   '("[" . qqh/transient/prev)
   '("]" . qqh/transient/next)
   '("SPC" . qqh/transient/leader)
   '("<escape>" . ignore))

  ;; default meow leader bindings
  (meow-leader-define-key
   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("(" . meow-beginning-of-thing)
   '(")" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . ignore)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . ignore)
   '("d" . meow-kill)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("G" . meow-grab)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . ignore)
   '("K" . eldoc)
   '("l" . meow-line)
   '("L" . avy-goto-line)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   (cons "S" surround-keymap)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . ignore)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<" . meow-pop-to-global-mark)
   '(">" . meow-unpop-to-mark)
   '("<escape>" . keyboard-quit)
   '("RET" . ignore)
   '("SPC" . qqh/transient/leader)

   ;; Some vim-like bindings
   '("g" . qqh/transient/g)
   '(":" . meow-keypad)
   '("="   . meow-indent)
   '("C-q" . delete-window)
   '("M-q" . qqh/kill-buffer)

   ;; Bracketed movement
   '("[" . qqh/transient/prev)
   '("]" . qqh/transient/next)

   '("C-." . embark-act)         ;; pick some comfortable binding
   '("C-;" . embark-dwim)        ;; good alternative: M-.
   '("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
   )

(require 'meow)
(meow-setup)
(add-to-list 'meow-mode-state-list '(eat-mode . normal))
(meow-global-mode 1)
(meow-esc-mode 1)               ;; enable esc mode for terminal use

;;; Customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Cleanup
(catppuccin-reload)
(setq gc-cons-threshold (or qqh/initial-gc-threshold 800000))


;;; init.el ends here
(put 'downcase-region 'disabled nil)
