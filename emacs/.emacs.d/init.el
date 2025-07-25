;;; init.el --- My emacs configurtion -*- lexical-binding: t -*-

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

(defvar qqh/trunc-len 24
  "The length to truncate strings to.")

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
  (straight-vc-git-default-protocol 'ssh)
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
  :custom
  (which-key-show-transient-maps t)
  :config
  (which-key-mode))

;; Catppuccin for using colors in other packages
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha
	    catppuccin-italic-comments nil
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
(setq-default line-spacing (if (qqh/macos-p) nil 2))

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
(recentf-mode t)

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


;;; Misc. editing enhancements
(use-package avy
  :demand t
  :config
  (setq avy-keys '(?a ?s ?d ?f ?r ?u ?j ?k ?l ?\;)
        avy-background nil))

;; faster searching
(use-package ripgrep)

;; Modify search results en masse
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;;; Minibuffer & Completion
;;;; Embark and Consult
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
                  consult--source-project-buffer-hidden
                  consult--source-project-recent-file-hidden))


  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :demand t
  :after '(avy)
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

(use-package embark-consult
  :after (embark consult)
  :config
  (bind-key (kbd "C-.") 'embark-act 'minibuffer-mode-map))


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
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)            ;; auto completion
  (corfu-quit-no-match t)   ;; Quit when no matches
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  :bind (:map corfu-map
              ;; Use TAB for cycling, default is `corfu-complete'.
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :init
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
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

;;;; Vterm: Terminal Emulation
(use-package vterm
  :hook (vterm-mode . goto-address-mode)
  :bind (:map vterm-mode-map
              ("C-c C-x" . vterm--self-insert))
  :config
  (unbind-key (kbd "M-'") 'vterm-mode-map)
  (unbind-key (kbd "M-]") 'vterm-mode-map))

(use-package multi-vterm
  :custom
  (multi-vterm-buffer-name "vterm")
  :config
  (defun multi-vterm-format-buffer-name (name)
    "Format vterm buffer NAME."
    (let* ((dirs (file-name-split name))
           (dirs (cl-remove-if-not (lambda (s) (not (string= s ""))) dirs))
           (name (car (last dirs))))
      (format "*%s: %s*" multi-vterm-buffer-name (file-name-nondirectory name)))))

(defun multi-vterm ()
  "Create new vterm buffer, using `display-buffer' instaed of `switch-to-buffer'."
  (interactive)
  (let* ((vterm-buffer (multi-vterm-get-buffer)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (multi-vterm-internal)
    (display-buffer vterm-buffer)))

(defun multi-vterm-project ()
  "Create new vterm buffer, using `display-buffer' instead of `switch-to-buffer'."
  (interactive)
  (if (multi-vterm-project-root)
      (if (buffer-live-p (get-buffer (multi-vterm-project-get-buffer-name)))
          (if (string-equal (buffer-name (current-buffer)) (multi-vterm-project-get-buffer-name))
              (delete-window (selected-window))
            (display-buffer (multi-vterm-project-get-buffer-name)))
        (let* ((vterm-buffer (multi-vterm-get-buffer 'project))
               (multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer))))
          (set-buffer vterm-buffer)
          (multi-vterm-internal)
          (display-buffer vterm-buffer)))
    (message "This file is not in a project")))



;;;; Magit: best Git client to ever exist
(use-package magit
  :custom
  (magit-commit-show-diff nil)
  (magit-commit-diff-inhibit-same-window nil)
  :config
  ;; Show magit in a full window
  (add-to-list 'display-buffer-alist
	           '("\\(magit: .+\\|magit-log.+\\|magit-revision.+\\)"
	             (display-buffer-full-frame)))
  ;; except for certain buffers
  (add-to-list 'display-buffer-alist
	           '("\\(magit-diff:.*\\)"
	             (display-buffer-at-bottom))))

(use-package forge
  :config
  ;; Configure auth source
  (setq auth-sources '("~/.authinfo"))
  (push '("gitlab.veriskweather.net"               ; GITHOST
          "gitlab.veriskweather.net/api/v4"        ; APIHOST
          "gitlab.veriskweather.net"               ; WEBHOST and INSTANCE-ID
          forge-gitlab-repository)    ; CLASS
        forge-alist))

;;;; Project Management
;;;;; direnv integration
(use-package envrc
  :hook (after-init . envrc-global-mode))
;;;;; Projectile
(use-package project)

(defun qqh/project/open-org-file ()
  "Open the project.org file at the root of the current project. If no project.org file is found, create a new one from a template."
  (interactive)
  (let ((file     (projectile-expand-root "project.org"))
        (template (expand-file-name "templates/project-template.org"
                                    qqh/modules-dir)))
    (unless (file-exists-p file)
      (copy-file template file))
    (find-file file)))

(use-package projectile
  :diminish projectile-mode
  :bind (:map projectile-mode-map
              ("<f8>" . projectile-command-map)
              ("<f7>" . multi-vterm-project)
         :map projectile-command-map
              (";" . qqh/project/open-org-file))
  :init
  (projectile-mode +1)

  ;; integration with project.el, some packages work better with it
  (add-hook 'project-find-functions #'project-projectile)

  (when (qqh/macos-p)
    (setq projectile-fd-executable "/opt/homebrew/bin/fd"))

  (setq projectile-enable-caching t
        projectile-auto-discover t
        projectile-project-search-path '(("~/code/" . 3)
					                     "~/sources/"))
  :config
  (projectile-register-project-type 'gleam '("gleam.toml")
                                    :project-file "gleam.toml"
				                    :compile "gleam build"
				                    :test "gleam test"
				                    :run "gleam run"
                                    :src-dir "src/"
                                    :test-dir "test/"
				                    :test-suffix "_test"))

;;;;; Perspectives
(use-package persp-mode
  :after consult
  :custom
  (persp-auto-resume-time -1.0)
  (persp-auto-save-opt 1)
  (persp-add-buffer-on-after-change-major-mode 'free)
  (persp-kill-foreign-buffer-behaviour 'kill)
  (persp-keymap-prefix (kbd "<f5>"))
  :bind (:map persp-mode-map
              ("<f6>" . persp-switch))
  :config
  (persp-mode 1)

  ;; Override persp-switch to make it exclude the nil perspective
  (cl-defun persp-frame-switch (name &optional (frame (selected-frame)))
    (interactive "i")
    (unless name
      (setq name (persp-read-persp "to switch(in frame)" nil nil nil t t)))
    (unless (memq frame persp-inhibit-switch-for)
      (run-hook-with-args 'persp-before-switch-functions name frame)
      (let ((persp-inhibit-switch-for (cons frame persp-inhibit-switch-for)))
        (persp-activate (persp-add-new name) frame)))
    name)
  (cl-defun persp-window-switch (name &optional (window (selected-window)))
    (interactive "i")
    (unless name
      (setq name (persp-read-persp "to switch(in window)" nil nil nil t t)))
    (unless (memq window persp-inhibit-switch-for)
      (run-hook-with-args 'persp-before-switch-functions name window)
      (let ((persp-inhibit-switch-for (cons window persp-inhibit-switch-for)))
        (persp-activate (persp-add-new name) window)))
    name)

  ;; Perspective-exclusive tabs, ala tmux windows
  (add-hook 'persp-before-deactivate-functions
            (defun qqh/persp/save-tab-bar-data (_)
              (when (get-current-persp)
                (set-persp-parameter
                 'tab-bar-tabs (tab-bar-tabs)))))

  (add-hook 'persp-activated-functions
            (defun qqh/persp/load-tab-bar-data (_)
              (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
              (tab-bar--update-tab-bar-lines t)))

  (defvar persp-consult-source
    (list :name     "Persp"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items
          #'(lambda ()
              (mapcar #'buffer-name
                      (persp-filter-out-bad-buffers)))))
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

(use-package persp-mode-projectile-bridge
  :hook (after-init-hook . persp-mode-projectile-bridge-mode)
  :custom
  (persp-mode-projectile-bridge-persp-name-prefix "")

  :config
  (add-hook 'persp-mode-projectile-bridge-mode-hook
            #'(lambda ()
                (if persp-mode-projectile-bridge-mode
                    (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                  (persp-mode-projectile-bridge-kill-perspectives)))))

;;;;; Consult-todo: Search project todos
(use-package consult-todo)

;;;; Miscellaneous file types

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode))
  :config
  (setq markdown-list-indent-width 2))

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
  :straight (:type built-in)
  :diminish eldoc-mode)

;;;;; documentation comment generation
(use-package docstr
  :diminish docstr-mode
  :hook (((python-mode python-ts-mode) . docstr-mode))
  :custom
  (docstr-key-enable t)
  (docstr-python-style 'google)
  (docstr-python-modes '(python-mode python-ts-mode))
  :config
  (add-to-list 'docstr-writers-alist '(python-ts-mode . docstr-writers-python)))

;;;;; Devdocs.io integration
(use-package devdocs
  :bind (("C-h D" . devdocs-lookup))
  :hook ((tuareg-mode . (lambda () (setq-local devdocs-current-docs '("ocaml~5.0"))))
         ((python-mode python-ts-mode) . (lambda () (setq-local devdocs-current-docs '("python~11"))))
         (c-mode . (lambda () (setq-local devdocs-current-docs '("c"))))
         (c++-mode . (lambda () (setq-local devdocs-current-docs '("cpp"))))))

;;;;; Flymake
(use-package flymake
  :straight (:type built-in)
  :custom
  (flymake-mode-line-format '(" " flymake-mode-line-counters flymake-mode-line-exception))
  (flymake-margin-indicators-string '((error "X" compilation-error)
                                      (warning "!" compilation-warning)
                                      (note "?" compilation-info)))
  :custom-face
  (flymake-note ((t :underline ,(catppuccin-color 'green))))
  (flymake-warning ((t :underline ,(catppuccin-color 'yellow))))
  (flymake-error ((t :underline ,(catppuccin-color 'red))))
  :hook (prog-mode . flymake-mode))

;;;; Eglot
(use-package eglot
  :straight (:type built-in)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files
  (eglot-autoshutdown t)
  :hook ((tuareg-mode python-mode python-ts-mode c-mode c++-mode nix-mode go-mode) . eglot-ensure)
  :config
  ;; Disable inlay hints globally
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

  ;; extra server binaries
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs '(gleam-ts-mode . ("gleam" "lsp")))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  ;; PERF: dont log every event
  (fset #'jsonrpc--log-event #'ignore)

  ;; Remove the eglot indicator from the mode-line-misc-info variable
  ;; (setq mode-line-misc-info
  ;;       (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info))

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

;;;; Code formatting
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

;;;;; Gleam
(use-package gleam-ts-mode
  :mode (("\\.gleam\\'" . gleam-ts-mode)))

;;;;; Go
(use-package go-mode
  :hook ((go-mode . (lambda ()
                      "gofmt on save in go buffers"
                      (add-hook 'before-save-hook 'gofmt-before-save)))))

;;;;; Nix
(use-package nix-mode
  :mode "\\.nix\\'")

;;;;; OCaml
;; Major-Mode mode for OCaml programming
(use-package tuareg
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune)

;;;;; Python
;; Built-in Python utilities
(use-package python
  :custom
  (python-shell-interpreter "python3")

  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(defun qqh/spack-python ()
  "Activate the spack environment in the current project, if there is one."
  (interactive)
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (let* ((root-dir (if (projectile-project-root)
                       (projectile-project-root)
                     default-directory))
         (env-dir (expand-file-name "spack_env/.spack-env/view" root-dir)))
    (if (file-exists-p env-dir)
        (pyvenv-activate env-dir)
      (ignore))))

(use-package pyvenv
  :hook (((python-mode python-ts-mode) . qqh/spack-python))
  :config
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (pyvenv-mode 1))

;;;;; Rust
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
  (unbind-key (kbd "C-'") 'org-mode-map)
  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  (setq org-export-with-smart-quotes t                ; Make exporting quotes better
        org-hide-emphasis-markers t                   ; hide formatting stuff
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

;;; Keybindings

;;;; Definitions
(defun qqh/kill-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun qqh/project/open-flake ()
  "Open the project flake file, if it exists."
  (interactive)
  (let ((f (projectile-expand-root "flake.nix")))
    (if (file-exists-p f)
        (find-file f)
      (message (format "%s does not exist!" f)))))

(defun qqh/search-files ()
  "Fuzzy find files with `projectile-find-file', falling
back to `consult-fd' if we're not in a project."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (consult-fd)))

(defun qqh/emacs/reload ()
  "Load my Emacs configuration."
  (interactive)
  (load-file user-init-file))

(defun qqh/emacs/open-config ()
  "Open my Emacs configuration."
  (interactive)
  (find-file user-init-file))

(defun qqh/config/open-nix-flake ()
  "Open my nix flake."
  (interactive)
  (find-file "~/dotfiles/flake.nix"))

(defun qqh/config/open-nix-home ()
  "Open my nix home-manager home.nix."
  (interactive)
  (find-file "~/dotfiles/home/home.nix"))

(defun qqh/config/hyprland ()
  "Open my hyprland config."
  (interactive)
  (find-file "~/dotfiles/home/hyprland.nix"))

;;;; Undo-fu
(use-package undo-fu
  :straight (undo-fu :type git :host github :repo "emacsmirror/undo-fu"))

;;;; transient: so many leader keys
(use-package transient
  :config
  (transient-bind-q-to-quit)
  :bind
  (:map transient-base-map
        ("C-g" . transient-quit-all)
        :map transient-sticky-map
        ("C-g" . transient-quit-all)))

(use-package transient-showcase
  :straight '(transient-showcase
              :type git :host github
              :repo "positron-solutions/transient-showcase"))

;; Set up some transient maps for additional leaders
(transient-define-prefix qqh/transient/code ()
  ["code..."
   ("c" "compile" projectile-compile-project)
   ("f" "format" format-all-region-or-buffer)
   ("v" "activate environment" pyvenv-workon)])

(transient-define-prefix qqh/transient/git ()
  [:class transient-row "git..."
          ("g" "status" magit)
          ("b" "branch" magit-branch)
          ("B" "blame" magit-blame)])

(transient-define-prefix qqh/transient/open ()
  [:class transient-row "open..."
          ("d" "diagnostics panel" consult-flymake)
          ("t" "terminal" multi-vterm)])

(transient-define-prefix qqh/transient/notes ()
  [:class transient-row "notes..."
          ("c" "capture" org-roam-capture)
          ("s" "search" org-roam-node-find)])

(transient-define-prefix qqh/transient/search ()
  [:class transient-row "search..."
          ("l" "all lines" consult-line-multi)
          ("i" "imenu" consult-imenu)
          ("n" "notes" org-roam-node-find)
          ("o" "outline" consult-outline)
          ("s" "files" qqh/search-files)])

(transient-define-prefix qqh/transient/config ()
  [("r" "reload emacs config" qqh/emacs/reload)]
  [:class transient-row "edit config for..."
          ("c" "emacs" qqh/emacs/open-config)
          ("f" "flake" qqh/config/open-nix-flake :if (lambda () (not (qqh/macos-p))))
          ("h" "hyprland" qqh/config/hyprland :if (lambda () (not (qqh/macos-p))))
          ("n" "home-manager" qqh/config/open-nix-home :if (lambda () (not (qqh/macos-p))))])

(transient-define-prefix qqh/transient/leader ()
  "Transient map for my leader bindings."
  ["leader bindings..."
   ("SPC" "buffers" consult-buffer)
   ("," "prev buffer" previous-buffer)
   ("." "next buffer" next-buffer)
   (":" "eval expression" eval-expression)]
  [:class transient-row
          ("c" "+code" qqh/transient/code)
          ("g" "+git" qqh/transient/git)
          ("o" "+open" qqh/transient/open)
          ("n" "+notes" qqh/transient/notes)
          ("s" "+search" qqh/transient/search)
          (";" "+config" qqh/transient/config)])

(transient-define-prefix qqh/transient/next ()
  "Transient map for going to the next thing."
  [["Next"
    ("d" "todo" hl-todo-next)
    ("e" "error" flymake-goto-next-error)
    ("p" "perspective" persp-next)
    ("x" "conflict" smerge-vc-next-conflict)]])

(transient-define-prefix qqh/transient/prev ()
  "Transient map for going to the previous thing."
  [["Previous"
    ("d" "todo" hl-todo-previous)
    ("e" "error" flymake-goto-prev-error)
    ("p" "perspective" persp-prev)]])

;;;; Global bindings
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "M-[") 'tab-previous)
(global-set-key (kbd "M-]") 'tab-next)

(global-set-key (kbd "M-<mouse-1>") 'goto-address-at-mouse)

;;;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t)
  ;; settings
  (setq evil-want-keybinding nil
        evil-want-unimpaired-p nil
        evil-want-integration t
        evil-want-C-i-jump t
        evil-shift-width 2)

  ;; Set the cursor colors per mode
  (setq evil-normal-state-cursor `(box ,(catppuccin-color 'mauve))
        evil-insert-state-cursor `(box ,(catppuccin-color 'green))
        evil-visual-state-cursor `(box ,(catppuccin-color 'sapphire))
        evil-replace-state-cursor `(box ,(catppuccin-color 'red))
        evil-motion-state-cursor `(box ,(catppuccin-color 'peach))
        evil-operator-state-cursor `(box ,(catppuccin-color 'yellow))
        evil-emacs-state-cursor `(box ,(catppuccin-color 'rosewater)))

  ;; Configuring initial major mode for some modes
  ;; start in emacs mode
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'inferior-python-mode 'emacs)

  (evil-set-initial-state 'messages-buffer-mode 'normal)

  (add-hook 'git-commit-setup-hook 'evil-insert-state)
  :config
  ;; enable the stuff
  (evil-set-undo-system 'undo-fu)
  (evil-mode 1))

;;;;; Evil plugins
(use-package evil-collection
  :after evil
  :custom
  (evil-collection-want-unimpaired-p nil)
  (evil-collection-magit-setup t)
  (evil-collection-forge-setup t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package goto-chg
  :after evil)

;;;; Evil Bindings
(global-set-key (kbd "M-q") 'qqh/kill-buffer)
(global-set-key (kbd "C-q") 'evil-window-delete)

(evil-define-key '(normal insert emacs) 'global (kbd "<prior>") 'evil-scroll-up)
(evil-define-key '(normal insert emacs) 'global (kbd "<next>") 'evil-scroll-down)

;; Setup my transients and maps
(evil-define-key 'normal 'global (kbd "SPC") 'qqh/transient/leader)
(evil-define-key 'normal 'global (kbd "M-q") 'qqh/kill-buffer)

;; Avy bindings
(evil-define-key '(normal visual) 'global (kbd "L") 'avy-goto-line)
(evil-define-key '(normal visual) 'global (kbd "g RET") 'avy-goto-char-2)

;; Add my own bracketed movement options
(evil-define-key 'motion 'global (kbd "[ e") 'flymake-goto-prev-error)
(evil-define-key 'motion 'global (kbd "] e") 'flymake-goto-next-error)
(evil-define-key 'motion 'global (kbd "] x") 'smerge-vc-next-conflict)

;; C-g quits normal mode
(evil-define-key 'insert 'global (kbd "C-g") 'evil-normal-state)

;; Eglot bindings
(evil-define-key 'normal 'eglot-mode-map (kbd "g R") 'eglot-rename)
(evil-define-key 'normal 'eglot-mode-map (kbd "g SPC") 'eglot-code-actions)
;; vterm
(evil-define-key nil vterm-mode-map (kbd "C-SPC") 'qqh/transient/leader)

;;; Themes / UI customization

;;;; Face customizations
(set-face-attribute 'default nil
                    :family "Comic Code"
                    :weight 'regular
                    :height 120)
(set-face-attribute 'window-divider nil
                    :background (catppuccin-color 'mantle)
                    :foreground (catppuccin-color 'mantle))
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

;; dim inactive buffrs
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

(use-package vim-tab-bar
  :init
  (vim-tab-bar-mode 1))

(set-face-attribute 'tab-bar nil :box nil :background (catppuccin-color 'mantle))
(set-face-attribute 'tab-bar-tab nil :foreground (catppuccin-color 'mauve) :background (catppuccin-color 'base))

;;;; Modeline configurtaion
(use-package mood-line
  :custom-face
  (mood-line-unimportant ((t (:inherit shadow))))
  (mood-line-buffer-name ((t (:weight normal))))
  (mood-line-major-mode ((t (:weight normal))))
  :config
  (mood-line-mode)

  (defun mood-line-segment-cursor-position ()
    "Return the position of the cursor in the current buffer."
    (propertize (format-mode-line "(%l:%c)") 'face 'mood-line-unimportant))

  (defun mood-line-segment-separator ()
    (propertize "|" 'face 'mood-line-unimportant))

  (defun mood-line-segment-vc ()
    "Return color-coded version control information."
    (if (> (length mood-line-segment-vc--text) qqh/trunc-len)
        (concat (substring mood-line-segment-vc--text 0 qqh/trunc-len) "...")
      mood-line-segment-vc--text))

  (defun mood-line-segment-persp ()
    "Return the name of the cerrent persp"
    (let ((p (get-current-persp)))
      (if p
          (propertize (persp-name p) 'face 'mood-line-unimportant))))

  (defface qqh/evil/normal-face
    `((t . (:bold t :background ,(catppuccin-color 'mauve) :foreground ,(catppuccin-color 'base))))
    "evil normal mode mood line face"
    :group 'qqh)
  (defface qqh/evil/insert-face
    `((t . (:bold t :background ,(catppuccin-color 'green) :foreground ,(catppuccin-color 'base))))
    "evil insert mode mood line face"
    :group 'qqh)
  (defface qqh/evil/visual-face
    `((t . (:bold t :background ,(catppuccin-color 'sapphire) :foreground ,(catppuccin-color 'base))))
    "evil visual mode mood line face"
    :group 'qqh)
  (defface qqh/evil/replace-face
    `((t . (:bold t :background ,(catppuccin-color 'red) :foreground ,(catppuccin-color 'base))))
    "evil replace mode mood line face"
    :group 'qqh)
  (defface qqh/evil/motion-face
    `((t . (:bold t :background ,(catppuccin-color 'peach) :foreground ,(catppuccin-color 'base))))
    "evil motion mode mood line face"
    :group 'qqh)
  (defface qqh/evil/operator-face
    `((t . (:bold t :background ,(catppuccin-color 'yellow) :foreground ,(catppuccin-color 'base))))
    "evil operator mode mood line face"
    :group 'qqh)
  (defface qqh/evil/emacs-face
    `((t . (:bold t :background ,(catppuccin-color 'rosewater) :foreground ,(catppuccin-color 'base))))
    "evil emacs mode mood line face"
    :group 'qqh)

  (setq
   mood-line-glyph-alist mood-line-glyphs-fira-code

   mood-line-segment-modal-evil-state-alist '((normal   . (" N " . qqh/evil/normal-face))
                                              (insert   . (" I " . qqh/evil/insert-face))
                                              (visual   . (" V " . qqh/evil/visual-face))
                                              (replace  . (" R " . qqh/evil/replace-face))
                                              (motion   . (" M " . qqh/evil/motion-face))
                                              (operator . (" O " . qqh/evil/operator-face))
                                              (emacs    . (" E " . qqh/evil/emacs-face)))

   mood-line-format (mood-line-defformat
                     :left
                     (((mood-line-segment-modal)                                          . " ")
                      ((mood-line-segment-buffer-status)                                  . " ")
                      ((mood-line-segment-buffer-name)                                    . " ")
                      ((mood-line-segment-cursor-position)                                . " ")
                      ((mood-line-segment-separator)                                      . " ")
                      (mood-line-segment-major-mode))
                     :right
                     (((mood-line-segment-misc-info)                                      . " ")
                      ((when (mood-line-segment-misc-info) (mood-line-segment-separator)) . " ")
                      ((mood-line-segment-checker)                                        . " ")
                      ((when (mood-line-segment-checker) (mood-line-segment-separator))   . " ")
                      ((mood-line-segment-project)                                        . " ")
                      ((when (mood-line-segment-vc) "on")                                 . " ")
                      ((mood-line-segment-vc)                                             . " ")
                      ((when (mood-line-segment-persp) (mood-line-segment-separator))     . " ")
                      ((mood-line-segment-persp)                                          . " ")))))

;;;; Buffer display configuration
;;;;; display-buffer-alist customization
;; reuse as much as possible
(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-same-window)
        (reusable-frames . t)))

(setq even-window-sizes nil)     ; avoid resizing

(add-to-list 'display-buffer-alist
	         '("\\*\\(Compile-Log\\|Async-native-compile-log\\|Warnings\\)\\*"
	           (display-buffer-no-window)
	           (allow-no-window t)))

(add-to-list 'display-buffer-alist
	         '("\\*\\(Ibuffer\\|vc-dir\\|vc-diff\\|vc-change-log\\|Async Shell Command\\)\\*"
	           (display-buffer-full-frame)))

(add-to-list 'display-buffer-alist
             '("\\*vterminal.*\\*"
               (display-buffer-reuse-window)))

;; pop up management
(use-package popper
  :straight t
  :bind (("C-'"   . popper-toggle)
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers '("\\*Messages\\*"
                                   "\\*eldoc\\*"
                                   "Output\\*$"
                                   "\\*Async Shell Command\\*"
                                   "\\*OCaml\\*"
                                   "magit.*"
                                   "\\*vterm:.*\\*"
                                   vterm-mode
                                   magit-mode
                                   help-mode
                                   compilation-mode
                                   comint-mode)
        popper-group-function #'popper-group-by-projectile
        popper-echo-dispatch-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))

  (setq
   popper-mode-line nil
   popper-window-height (lambda (win)
                          (fit-window-to-buffer
                           win
                           (floor (frame-height) 2.5)
                           (floor (frame-height) 3))))

  (popper-mode +1)
  ;; echo area hints
  (popper-echo-mode +1))

;;; Customization file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Cleanup
(catppuccin-reload)
(setq gc-cons-threshold (or qqh/initial-gc-threshold 800000))


(put 'downcase-region 'disabled nil)
;;; init.el ends here
