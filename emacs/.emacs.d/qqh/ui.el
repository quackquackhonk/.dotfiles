;;; ui.el --- Packages for ui enhancements / themeing

;;; Commentary:

;;; Code:

(use-package catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(catppuccin-reload)

(use-package diminish
  :config
  (require 'which-key)
  (diminish 'which-key-mode)
  (require 'undo-tree)
  (diminish 'undo-tree-mode)

  ;; diminish built-in minor modes
  (diminish 'eldoc-mode))

(use-package nerd-icons)

(use-package rainbow-delimiters
  :after diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode)
  :config
  (require 'diminish)
  (diminish 'rainbow-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))
