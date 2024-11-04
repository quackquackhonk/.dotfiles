;;; init.el --- Entry point for my emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;;;  - Straight Initialization
;;;  - Basic settings
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;; Code:

;;;; Guardrail

(when (< emacs-major-version 29)
  (error "[qqh] config assumes Emacs version 29+, currently running %s!" emacs-major-version))

;;;; Straight initialization

;; bootstrap straight
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

;; Setup use-package for straight
(use-package straight
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))
  (straight-use-package-by-default t))

;; always load the newest bytecode
(setq load-prefer-newer t)

;; Load diminish for :diminish constructs in use-package
(use-package diminish
  :config
  ;; diminish built-in minor modes
  (diminish 'visual-line-mode)
  (diminish 'eldoc-mode))

(use-package exec-path-from-shell
  :init (setq-default explicit-shell-file-name "/usr/bin/nu")
  :config
  ;; only run exec-path-from-shell when not running in terminal (GUI or daemon)
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;;;; Variable definitions

(defvar qqh/modules-dir (expand-file-name "qqh" user-emacs-directory)
  "The directory containing my module files.")

;;;; Basic settings
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most


;; Enable terminal mouse mode in WSL
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
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
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; use the faster programs
(setq find-program "fd"
      grep-program "rg")

;;; Minibuffer/completion settings
;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

;; Save history of minibuffer
(savehist-mode)

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'lazy)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;; Dired
(use-package dired
  :straight nil
  :config
  (setq dired-kill-when-opening-new-dired-buffer t))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup
(defun qqh/backup-file-name (fpath)
  "Return a new file path of a given file path (FPATH).
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "backups/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'qqh/backup-file-name)

;;; Interface enhancements/defaults
;; Mode line information
(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

;; Don't show trailing whitespace, and delete when saving
(setopt show-trailing-whitespace nil)
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

;; Tabs BTFO
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(scroll-bar-mode -1)
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(setopt ring-bell-function 'ignore)                   ; disable the bell
(setopt compilation-scroll-output t)                  ; follow compilation output by default

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(setq frame-resize-pixelwise t)

(setq vc-follow-symlinks t)                             ; auto follow VC links
(setq indicate-empty-lines t)
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :demand t
  :bind (("C-c n" . avy-goto-line))
  :config
  (setq avy-keys '(?a ?r ?s ?t ?p ?l ?n ?e ?i ?o)
        avy-background nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package embark-consult)

;; Consult: Misc. enhanced commands
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
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

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match t)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :straight nil
  :init
  (defun qqh/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . qqh/setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; Orderless: powerful completion style
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. editing enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A better auto indentation mode
(use-package ripgrep)

;; Modify search results en masse
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; A few more useful configurations...
(use-package emacs
  :straight nil
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

;; Vim-bindings in Emacs (evil-mode configuration)
;; (load-file (expand-file-name "bindings-evil.el" qqh/modules-dir))
;;; Some declarations for Keybindings
(defun qqh/kill-buffer ()
  (interactive)
  (persp-kill-buffer* (current-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Meow for modal editing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install meow
(use-package meow :demand t)

(defun meow-setup ()
  ;; colemak-dh cheatsheet
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  ;; sets the thing table characters to use ([{ for grouping
  ;; punctuations
  (setq meow-char-thing-table
        '((?\( . round)
          (?\) . round)
          (?\[ . square)
          (?\] . square)
          (?\{ . curly)
          (?\} . curly)
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
        meow-keypad-literal-prefix ?\')           ;; Use ' for literal keys

  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))

  ;; default meow leader bindings
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("SPC" . consult-buffer)
   '("TAB" . other-window)
   '("," . meow-last-buffer)
   '(":" . eval-expression)
   '("g" . magit)
   '("f" . consult-fd)
   '("p" . projectile-command-map)
   '("RET" . avy-goto-line)

   '(";r" . (lambda ()
              (interactive)
              (load-file user-init-file)))
   '(";c" . (lambda ()
              (interactive)
              (find-file user-init-file)))

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
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("(" . meow-block)
   '(")" . meow-to-block)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . embark-act)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   ;; TODO:
   '("g" . ignore)
   '("G" . meow-grab)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
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
   '("S" . 'surround-keymap)
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
   '(":" . meow-M-x)
   '("="   . meow-indent)
   '("C-q" . delete-window)
   '("M-q" . qqh/kill-buffer)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
;; enable esc mode for terminal use
(meow-esc-mode 1)

(use-package surround
  :bind-keymap (("M-'" . surround-keymap)))

;; Packages for software development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package transient)
(use-package magit)

(use-package forge
  :config
  ;; Configure auth source
  (setq auth-sources '("~/.authinfo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qqh/open-project-org-file ()
  (interactive)
  (require 'projectile)
  (let ((file     (projectile-expand-root "project.org"))
        (template (expand-file-name "templates/project-template.org"
                                    qqh/modules-dir)))
    (unless (file-exists-p file)
      (copy-file template file))
    (find-file file)))

(use-package project)
(use-package projectile
  :init
  (projectile-mode +1)

  (setq projectile-project-search-path '(("~/code/" . 2)
					                     "~/.sources/")
	    projectile-mode-line-prefix " In "
        projectile-switch-project-action 'qqh/open-project-org-file))

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

;; make them play nice
(use-package persp-projectile
  :config
  (define-key projectile-command-map (kbd "P") 'projectile-persp-switch-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Documentation and Diagnostics
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show eldoc in a popup box
(use-package eldoc-box
  :config
  ;; Don't show on hover, only on keypress
  (eldoc-box-hover-mode -1)
  (setq eldoc-echo-area-use-multiline-p nil)
  :bind (:map meow-normal-state-keymap
              ("K" . eldoc-box-help-at-point)))

(use-package consult-todo :demand t)

;; Devdocs integration
(use-package devdocs
  :bind (("C-h D" . devdocs-lookup))
  :hook (('python-mode-hook . (lambda () (setq-local devdocs-current-docs '("python~3.11"))))
         ('python-ts-mode-hook . (lambda () (setq-local devdocs-current-docs '("python~3.11"))))
         ('c-mode-hook . (lamdba () (setq-local devdocs-current-docs '("c"))))
         ('c++-mode-hook . (lamdba () (setq-local devdocs-current-docs '("cpp"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  :defer t
  :hook (;; Python
         (python-ts-mode . eglot-ensure)
         ;; C / C++
         ((c-mode c++-mode) . eglot-ensure)
         )
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
  :config
  ;; Disable inlay hints globally
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)

  ;; clangd for c/c++
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

  ;; format on save
  (add-hook 'before-save-hook
            (lambda () (if (eglot-managed-p)
                           (eglot-format-buffer))))

  ;; PERF: dont log every event
  (fset #'jsonrpc--log-event #'ignore)

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

;; Diagnostic
(use-package eldoc)
(use-package flymake
  :after eglot
  :hook (('emacs-lisp-mode-hook . flymake-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language specific configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; C / C++
(setq-default c-basic-offset 4)

;;; PYTHON
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

;;; RUST
(use-package rust-mode
  :config
  ;; rustfmt
  (setq rust-format-show-buffer nil)
  (setq rust-format-on-save t)

  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package cargo
  :after rust-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Org-refile: where should org-refile look?
;; (setq org-refile-targets 'FIXME)


;; Org-roam variables
(setq org-roam-directory "~/org/roam/")
(setq org-roam-index-file "~/org/roam/index.org")

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
;; (setq org-link-abbrev-alist
;;       '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

(use-package org
  :hook ((org-mode . visual-line-mode))  ; wrap lines at word breaks

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)


  (setq org-export-with-smart-quotes t               ; Make exporting quotes better
        org-return-follows-link t                    ; RET follows links
        )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Phase 3: extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Babel Language activation

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  )

;; Extra UI / Themeing

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha
	    catppuccin-italic-comments t
	    catppuccin-highlight-matches t)


  (add-hook 'server-after-make-frame-hook #'catppuccin-reload)

  (load-theme 'catppuccin :no-confirm t)
  (catppuccin-reload))

(use-package nerd-icons)

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package fancy-compilation
  :config
  (fancy-compilation-mode))

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modeline configurtaion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq-default mode-line-format
;;               '("%e" mode-line-front-space
;;                 (:propertize
;;                  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;;                  display
;;                  (min-width
;;                   (5.0)))
;;                 mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
;;                 (vc-mode vc-mode)
;;                 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

;; (setq-default mode-line-format
;;               '("%e" mode-line-buffer-identification mode-line-modified mode-line-remote
;;                 " @ " mode-line-position evil-mode-line-tag
;;                 (vc-mode vc-mode)
;;                 "  " mode-line-modes mode-line-misc-info))

;; (force-mode-line-update)

;; Load the custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Cleanup
(setq gc-cons-threshold (or qqh/initial-gc-threshold 800000))


;;; init.el ends here
