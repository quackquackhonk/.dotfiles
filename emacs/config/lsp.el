
(require 'package)
(require 'use-package)

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

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)


;; RUST
(use-package rustic
  :config
  (setq rustic-format-on-save t))

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

;; RACKET
(use-package racket-mdoe)

(provide 'lsp)
