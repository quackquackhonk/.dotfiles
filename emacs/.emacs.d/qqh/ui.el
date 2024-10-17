;;; ui.el --- Packages for ui enhancements / themeing -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha
	catppuccin-italic-comments t
	catppuccin-highlight-matches t))

(use-package diminish
  :after (which-key tree-sitter)
  :config
  (require 'which-key)
  (diminish 'which-key-mode)

  (require 'tree-sitter)
  (diminish 'tree-sitter-mode)

  ;; diminish built-in minor modes
  (diminish 'eldoc-mode))

(use-package nerd-icons)

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (require 'diminish)
  (diminish 'rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (require 'diminish)
  (diminish 'rainbow-delimiters-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package fancy-compilation
  :config
  (fancy-compilation-mode))



(load-theme 'catppuccin :no-confirm t)
(catppuccin-reload)

(add-hook 'server-after-make-frame-hook #'catppuccin-reload)

;;; ui.el ends here.
