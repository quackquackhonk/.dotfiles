;;; init.el --- Entry point for my emacs config -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Guardrail
(when (< emacs-major-version 30)
  (error "[qqh] config requires Emacs version 30+, currently running %s!" emacs-major-version))

;;; Definitions
(defgroup qqh-emacs nil
  "User options for my Emacs configuration."
  :group 'file)

(defvar qqh/modules-dir (expand-file-name "qqh" user-emacs-directory)
  "The directory containing my module files.")

(defun qqh/macos-p ()
  "Check if the current frame is an OSX gui frame."
  (eq window-system 'darwin))

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

;;; Basic settings
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most


;; Enable terminal mouse mode in WSL
(when (qqh/in-terminal-p)
  (xterm-mouse-mode t))

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 2)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Move through windows with Control-<arrow keys>
(require 'windmove)
(windmove-default-keybindings 'control)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; only "y or n" prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; use the faster programs
(setq find-program "fd"
      grep-program "rg")

;; Save history of minibuffer
(savehist-mode)

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Tabs BTFO
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(scroll-bar-mode -1)                                  ; no scrollbars
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(setopt ring-bell-function 'ignore)                   ; disable the bell
(setopt compilation-scroll-output t)                  ; follow compilation output by default
(setq frame-resize-pixelwise t)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(setq vc-follow-symlinks t)                             ; auto follow VC links
(setq indicate-empty-lines t)
(setq inhibit-startup-message t)


;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backups
(defun qqh/backup-file-name (fpath)
  "Return a new file path of a given file path (FPATH).
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "backups/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'qqh/backup-file-name)

;;; Built-Ins.
;;;; Repeat Mode
(use-package repeat
  :config (repeat-mode))
;;;; Dired
(use-package dired
  :straight nil
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
;;;; Tempel: Templates (snippets) for emacs
;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

;;;; Corfu: Popup completion-at-point
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match t)        ;; Quit when no matches

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
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
  (when (eq window-system 'x)
    (setq eat-shell (executable-find "nu")))
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat


;;;; Magit: best Git client to ever exist
(use-package magit)

(use-package forge
  :config
  ;; Configure auth source
  (setq auth-sources '("~/.authinfo")))

;;;; Project Management
;;;;; Projectile
(use-package project)
(use-package projectile
  :diminish projectile-mode
  :init
  (projectile-mode +1)

  (setq projectile-auto-discover nil
        projectile-project-search-path '(("~/code/" . 2)
					                     "~/sources/")
        projectile-switch-project-action 'consult-fd))

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
  (persp-mode-prefix-key (kbd "M-p"))
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

(use-package dts-mode)

(use-package just-mode)
(use-package cmake-mode)

;;;; Documentation and Diagnostics

;;;;; Eldoc
(use-package eldoc
  :diminish eldoc-mode)

;;;;; documentation comment generation
(use-package docstr
  :diminish docstr-mode
  :config (global-docstr-mode 1))

;;;;; Flycheck diagnostics
(use-package flycheck
  :diminish flycheck-mode
  :init
  (global-flycheck-mode)
  :config
  (set-face-attribute 'flycheck-info nil
                      :background (catppuccin-darken (catppuccin-color 'teal) 70))
  :custom-face
  (flycheck-error ((t (:underline  ,(catppuccin-darken (catppuccin-color 'red) 60)))))
  (flycheck-fringe-warning ((t (:foreground ,(catppuccin-color 'red)))))
  (flycheck-error-list-err ((t (:foreground ,(catppuccin-color 'red)))))

  (flycheck-warning ((t (:underline ,(catppuccin-darken (catppuccin-color 'peach) 60)))))
  (flycheck-fringe-warning ((t (:foreground ,(catppuccin-color 'peach)))))
  (flycheck-error-list-err ((t (:foreground ,(catppuccin-color 'peach)))))

  (flycheck-fringe-info ((t (:foreground ,(catppuccin-color 'teal)))))
  (flycheck-error-list-info ((t (:foreground ,(catppuccin-color 'teal))))))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-popup-tip
  :after flycheck
  :config
  (setq popup-tip-max-width 120)
  :commands (flycheck-popup-tip-mode flycheck-popup-tip)
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package consult-flycheck)

;;;;; Devdocs.io integration
(use-package devdocs
  :bind (("C-h D" . devdocs-lookup))
  :config
  (add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~11"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~11"))))
  (add-hook 'c-mode-hook (lambda () (setq-local devdocs-current-docs '("c"))))
  (add-hook 'c++-mode-hook (lambda () (setq-local devdocs-current-docs-hook '("cpp")))))

;;;; Eglot
(use-package eglot
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files
  :hook ((python-mode python-ts-mode c-mode c++-mode) . eglot-ensure)
  :config
  ;; Disable inlay hints globally
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

  ;; clangd for c/c++
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

  ;; PERF: dont log every event
  (fset #'jsonrpc--log-event #'ignore)

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
                                                    :autopep8 (:enabled :json-false)
                                                    :black (:enabled t
                                                                     :cache_config t)))))))

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

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

;;;;; PYTHON
;; Built-in Python utilities
(use-package python
  :custom
  (python-shell-interpreter "python3")

  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

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

;; Agenda variables
(setq org-directory "~/org/")         ; Non-absolute paths for agenda and
                                      ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "work.org"))

;; Default tags
(setq org-tag-alist '(;; locale
                      (:startgroup)
                      ("home" . ?h)
                      ("work" . ?w)
                      ("school" . ?s)
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
                      ("review")
                      ("reading")))

;; TODO: Org-refile: where should org-refile look?
;; (setq org-refile-targets 'FIXME)

;; Org-roam variables
(setq org-roam-directory "~/org/roam/")
(setq org-roam-index-file "~/org/roam/index.org")

(use-package org
  :hook ((org-mode . visual-line-mode))  ; wrap lines at word breaks

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)


  (setq org-export-with-smart-quotes t               ; Make exporting quotes better
        org-return-follows-link t                    ; RET follows links
        )

  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in. Run
  ;;     M-x describe-variable RET org-todo-keywords RET
  ;; for documentation on how these keywords work.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

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

;;; Themes / UI customization

;;;; Settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Don't show trailing whitespace, and delete when saving
(setopt show-trailing-whitespace nil)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;;;; Color Schemes
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha
	    catppuccin-italic-comments t
	    catppuccin-highlight-matches t)

  (add-hook 'server-after-make-frame-hook #'catppuccin-reload)

  (load-theme 'catppuccin :no-confirm t)
  (catppuccin-reload))

;;;; Face customizations
(set-face-attribute 'window-divider nil
                    :background (catppuccin-color 'mantle)
                    :foreground (catppuccin-color 'mantle))
(set-face-attribute 'fringe nil
                    :background (catppuccin-color 'mantle))

;;;; Misc. Theming Packages
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :diminish rainbow-mode)

(use-package nerd-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(use-package hl-todo
  :config
  (defface qqh/hl-todo/todo-face
    `((t . (:bold t :background ,(catppuccin-color 'sky) :foreground ,(catppuccin-color 'base))))
    "The face highlighting TODOs in projects."
    :group 'qqh-emacs)

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

;;;;; Major mode

(defun qqh/modeline/major-mode-name ()
  "Return the major mode of the current buffer with a colon after it."
  (concat (symbol-name major-mode) ":"))

(defface qqh/modeline/modified-buffer-face
  `((t . (:bold t :foreground ,(catppuccin-color 'yellow))))
  "The face to use for the names of modified buffers on the mode line."
  :group 'qqh-emacs)

(defun qqh/modeline/buffer-name ()
  "Return the name of the current buffer."
  (let ((text " %b")
        (face (if (and (buffer-modified-p)
                      (mode-line-window-selected-p))
                  'qqh/modeline/modified-buffer-face
                nil)))
    (if face
        (propertize text 'face face)
      text)))

(defun qqh/modeline/project-and-vc ()
  "Report project name and VC information in the modeline."
  (let ((project-name (projectile-project-name)))
    (format "%s%s"
            (or project-name "")
            (if vc-mode (format " on%s" vc-mode) ""))))

(defun qqh/modeline/flycheck-status-text ()
  "Get a text describing STATUS for use in the mode line.

This has been adapted from `flycheck-mode-line-status-text'"
  (let* ((current-status flycheck-last-status-change)
         (errors (let-alist (flycheck-count-errors flycheck-current-errors)
                   (propertize (format "%se" (or .error 0))
                               'face 'flycheck-fringe-error)))
         (warnings (let-alist (flycheck-count-errors flycheck-current-errors)
                     (propertize (format "%sw" (or .warning 0))
                                 'face 'flycheck-fringe-warning)))
         (infos (let-alist (flycheck-count-errors flycheck-current-errors)
                  (propertize (format "%si" (or .info 0))
                              'face 'flycheck-fringe-info)))
         (indicator (pcase current-status
                      (`not-checked "")
                      (`no-checker "missing checker")
                      (`running "...")
                      (`errored "!!!")
                      (`finished
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (if (or .error .warning .info)
                             (format "%s %s %s" errors warnings infos)
                           "yippie!")))
                      (`interrupted "xxx")
                      (`suspicious "???")))
         (face (pcase current-status
                 (`not-checked nil)
                 (`no-checker 'error)
                 (`running 'flycheck-fringe-warning)
                 (`errored 'error)
                 (`finished
                  (let-alist (flycheck-count-errors flycheck-current-errors)
                    (if (or .error .warning .info) nil 'flycheck-fringe-info)))
                 (`interrupted 'error)
                 (`suspicious 'warning)) ))
    (when (not (string= "" indicator))
      (format " [%s]" (if face
                          (propertize indicator 'face face)
                        indicator)))))

(defvar-local qqh/modeline/flycheck
    `(:eval (when (and (featurep 'flycheck)
                       (mode-line-window-selected-p))
              (qqh/modeline/flycheck-status-text)))
  "Mode line construct for reporting flycheck status of the current buffer.")

(defvar-local qqh/modeline/eglot
  `(:eval
    (when (and (featurep 'eglot) (mode-line-window-selected-p))
      '(eglot--managed-mode ("[" eglot--mode-line-format "]"))))
  "Mode line construct for reporting eglot status of the current buffer.")

(setq-default mode-line-format
              '("%e%n"
                "  "
                (:eval (meow-indicator))
                " "
                (:eval (qqh/modeline/major-mode-name))
                (:eval (qqh/modeline/buffer-name))
                mode-line-process

                ;; emacs 30: right align the rest of the modeline
                mode-line-format-right-align         ;; emacs 30
                (:eval (qqh/modeline/project-and-vc))
                qqh/modeline/flycheck
                qqh/modeline/eglot
                " "
                (:eval (when (mode-line-window-selected-p)
                         mode-line-misc-info))
                "  "))

(dolist (construct '(qqh/modeline/flycheck
                     qqh/modeline/eglot))
  (put construct 'risky-local-variable t))

;;; Keybindings
(defun qqh/kill-buffer ()
  "Kill the current buffer using the persp-kill-buffer* command."
  (interactive)
  (persp-kill-buffer* (current-buffer)))

;; Install surround package
(use-package surround)

;; Make some transient- keymaps
(use-package transient
  :config
  (transient-bind-q-to-quit))

;; Set up some transient maps for additional leaders
(transient-define-prefix qqh/g-prefix-menu ()
  "Transient map for emulating vim's g- leader keybinding."
  [["Edit"
    ("c" "comment" comment-dwim)
    ("R" "rename" eglot-rename)]
   ["Find"
    ("d" "definition" xref-find-definitions)
    ("r" "references" xref-find-references)]
   ["Move"
    ("g" "top" beginning-of-buffer)
    ("G" "bottom" end-of-buffer)]])

(transient-define-prefix qqh/next-prefix-menu ()
  "Transient map for going to the next thing"
  [["Next"
    ("d" "todo" hl-todo-next)
    ("e" "error" flycheck-next-error)
    ("t" "tab" tab-next)
    ("p" "perspective" persp-next)]])

(transient-define-prefix qqh/prev-prefix-menu ()
  "Transient map for going to the previous thing"
  [["Previous"
    ("d" "todo" hl-todo-previous)
    ("e" "error" flycheck-previous-error)
    ("t" "tab" tab-previous)
    ("p" "perspective" persp-prev)]])

;;;; Global bindings
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;;;; Meow

(use-package meow
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

  ;; Change the keys used by keypad mode
  (setq meow-keypad-ctrl-meta-prefix ?\r          ;; Use RET for C-M-
        meow-keypad-meta-prefix ?z                ;; Use z for M-
        meow-keypad-literal-prefix 32)            ;; Use SPC for literal keys

  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))

  (defvar qqh/meow/window-keymap
    (let ((keymap (make-keymap)))
      (define-key keymap (kbd "w") #'other-window)
      (define-key keymap (kbd "h") #'split-window-below)
      (define-key keymap (kbd "v") #'split-window-right)
      (define-key keymap (kbd "q") #'delete-window)
      keymap))

  ;; define an alias for your keymap
  (defalias 'window-keymap qqh/meow/window-keymap)

  ;; default meow leader bindings
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("SPC" . consult-buffer)
   '("," . meow-last-buffer)
   '(":" . eval-expression)
   '("g" . magit)
   '("f" . consult-fd)
   '("p" . projectile-command-map)
   '("RET" . avy-goto-char-2)
   '("w" . window-keymap)

   ;; Open (o)
   '("od" . consult-flycheck)
   '("oi" . consult-imenu)
   '("oe". eshell)
   '("ot" . eat-project)

   ;; Emacs bindings
   '(";r" . (lambda ()
              (interactive)
              (load-file user-init-file)))
   '(";c" . (lambda ()
              (interactive)
              (find-file user-init-file)))
   '(";p" . qqh/open-project-org-file)


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
   '("L" . meow-goto-line)
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
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . meow-cancel-selection)
   '("RET" . avy-goto-line)

   ;; Some vim-like bindings
   '("g" . qqh/g-prefix-menu)
   '(":" . meow-M-x)
   '("="   . meow-indent)
   '("C-q" . delete-window)
   '("M-q" . qqh/kill-buffer)

   ;; Bracketed movement
   '("[" . qqh/prev-prefix-menu)
   '("]" . qqh/next-prefix-menu)

   '("C-." . embark-act)        ;; pick some comfortable binding
   '("C-;" . embark-dwim)       ;; good alternative: M-.
   '("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   ))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
;; enable esc mode for terminal use
(meow-esc-mode 1)

;;; Customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Cleanup
(setq gc-cons-threshold (or qqh/initial-gc-threshold 800000))


;;; init.el ends here
