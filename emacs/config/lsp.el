
(require 'package)
(require 'use-package)

;; syntax highlighting
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "SPC l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (rust-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(provide 'lsp)
