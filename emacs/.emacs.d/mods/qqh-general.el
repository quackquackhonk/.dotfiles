;;; qqh-general.el --- A listing of modules to load on startup
;;; Commentary:
;;; Code:

(qqh-require-packages '(general))

(require 'general)

(general-evil-setup)

;; Create a leader key definer
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

(general-def
  :states '(normal visual)
  ;"K" 'lsp-ui-doc-glance
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

(provide 'qqh-general)
