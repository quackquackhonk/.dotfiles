;;; Emacs Bedrock
;;;
;;; Extra config: Vim emulation

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evil Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: Move to meow

(use-package undo-fu)

;; general is used for custom key bindings
(use-package general
  :config
  (general-evil-setup))

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  ;; collection settings
  (setq evil-want-keybinding nil
        evil-want-integration t
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-shift-width 4)
  :config
  ;;  enable
  (evil-set-undo-system 'undo-fu)
  (evil-mode 1)

  ;; If you use Magit, start editing in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)


  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Universal argument: C-u -> C-l
  (global-unset-key (kbd "C-l"))
  (general-define-key
   "C-l" 'universal-argument)
  (general-define-key
   :keymaps 'universal-argument-map
   "C-l" 'universal-argument-more))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


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
  ";on" 'org-roam-node-find)
