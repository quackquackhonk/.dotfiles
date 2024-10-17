;;; ui.el --- Packages for ui enhancements / themeing

;;; Commentary:

;;; Code:

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha))

(use-package diminish
  :config
  (require 'which-key)
  (diminish 'which-key-mode)

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

(load-theme 'catppuccin :no-confirm t)
(add-hook 'server-after-make-frame-hook #'catppuccin-reload)
