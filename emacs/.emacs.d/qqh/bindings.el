;;; bindings.el --- Global bindings -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;;  - Evil
;;;  - General
;;;  - Global Keybindings

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evil Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: Move to meow

(use-package undo-fu)

(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t)
  ;; collection settings
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-shift-width 2)
  :config
  ;;  enable
  (evil-set-undo-system 'undo-fu)
  (evil-mode 1)

  ;; Universal argument: C-u -> C-l
  (global-unset-key (kbd "C-l"))
  (global-set-key (kbd "C-l") 'universal-argument)
  (define-key universal-argument-map
	      "C-l" 'universal-argument-more)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'eat-mode 'emacs)
  (add-hook 'git-commit-setup-hook 'evil-insert-state)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)


  ;; C-g quits normal mode
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state))

(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keybindings
;; general is used for custom key bindings
(use-package general
  :config
  (general-evil-setup)

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

  (defun qqh/kill-buffer ()
    (interactive)
    (persp-kill-buffer* (current-buffer)))


  ;; defines leader key bindings
  (qqh/leader-definer
    ;; top level bindings
    "SPC" 'consult-buffer
    "TAB" 'other-window
    "RET" 'avy-goto-char-2
    "q" 'qqh/kill-buffer
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

    ;; Open (o)
    "of" 'find-file
    "oi" 'consult-imenu
    "ot" 'eat-project
    "od" 'flymake-diagnostics

    ;; projects (p)
    "p" 'projectile-command-map

    ;; code
    "cc" 'compile
    "cC" 'compile-interactive
    "cd" 'docker

    ;; emacs (;)
    ";r" (lambda ()
	   (interactive)
	   (load-file user-init-file))
    ";c" (lambda ()
	   (interactive)
	   (find-file user-init-file))

    ;; global org bindings (;o)
    ";oa" 'org-agenda
    ";oc" 'org-roam-capture
    ";ol" 'org-roam-node-insert
    ";on" 'org-roam-node-find)

  ;; define movements to be accessed by Meta + key on colemak
  (general-def
    :states '(normal)
    ;; quit window
    "C-q" 'evil-window-delete
    
    ;; evil LSP keybindings
    ;; "gd" 'evil-goto-definition <-- built in
    "gr" 'lsp-ui-peek-find-references
    "gR" 'lsp-rename
    "g SPC" 'lsp-execute-code-action)

  (general-def
    :states '(normal visual)
    ;; move through diagnostics
    "]d" 'flymake-goto-next-error
    "[d" 'flymake-goto-prev-error
    ;; git conflicts
    "]x" 'smerge-vc-next-conflict)

  (general-def
    :states '(normal visual insert emacs)
    "M-[" 'persp-prev
    "M-]" 'persp-next)

  ;; unbind keys
  (general-unbind
    :states '(normal visual emacs insert)
    ;; disable for persp-mode
    "C-p"))

