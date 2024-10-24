;;; init.el --- Entry point for my emacs config -*- lexical-binding: t; -*-

;;; Commentary:
;;;  - Basic settings
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional extras
;;;  - Built-in customization framework

;;; Code:

;;; Guardrail

(when (< emacs-major-version 29)
  (error "[qqh] config assumes Emacs version 29+, currently running %s!" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Package initialization
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; always load the newest bytecode
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Variable definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar qqh/modules-dir (expand-file-name "qqh" user-emacs-directory)
  "The directory containing my module files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Move through windows with Control-<arrow keys>
(require 'windmove)
(windmove-default-keybindings 'shift)

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

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;;; Dired
(use-package dired
  :ensure nil
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
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Tabs BTFO
(setopt indent-tabs-mode nil)
(setopt tab-width 4)

;; Misc. UI tweaks
(scroll-bar-mode -1)
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(setopt ring-bell-function 'ignore)                   ; disable the bell
(setopt compilation-scroll-output t)

;; Use common keystrokes by default
(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width
(setopt display-line-numbers-type 'relative)

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
  :config
  (which-key-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Load the rest of my configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 ;; Minibuffer / Completion packages
(load-file (expand-file-name "base.el" qqh/modules-dir))

;; Packages for software development
(load-file (expand-file-name "dev.el" qqh/modules-dir))

;; Vim-bindings in Emacs (evil-mode configuration)
(load-file (expand-file-name "bindings.el" qqh/modules-dir))

;; Org-mode configuration
(load-file (expand-file-name "org.el" qqh/modules-dir))

;; Extra UI / Themeing
(load-file (expand-file-name "ui.el" qqh/modules-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eat-shell "/bin/zsh")
 '(ignored-local-variable-values
   '((eval progn
           (defun qqh/venv-on nil
             (pyvenv-activate
              "/Users/i34866/code/amps/amps-surface-roughness/spack_env/.spack-env/view")))))
 '(package-selected-packages
   '(avy blacken cape cargo catppuccin-theme cmake-mode corfu-terminal
         devdocs diminish dts-mode eat eglot-booster embark-consult
         evil-collection evil-commentary evil-surround
         fancy-compilation forge general grip-mode hl-todo json-mode
         just-mode keycast kind-icon ligature magit-todos marginalia
         nerd-icons orderless perspective projectile protobuf-mode
         pyvenv rainbow-delimiters rainbow-mode ripgrep rust-mode
         solaire-mode tree-sitter-langs undo-fu vertico wgrep
         yaml-mode))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url
                    "https://github.com/jdtsmith/eglot-booster")))
 '(safe-local-variable-values
   '((eval progn
           (defun qqh/venv-on nil
             (interactive)
             (pyvenv-activate
              "/Users/i34866/code/amps/amps-surface-roughness/spack_env/.spack-env/view"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq gc-cons-threshold (or qqh/initial-gc-threshold 800000))
(put 'narrow-to-region 'disabled nil)

;;; init.el ends here
