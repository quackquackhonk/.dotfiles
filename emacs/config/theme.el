(require 'package)
(require 'use-package)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  (setq doom-gruvbox-dark-variant nil)
  (doom-themes-visual-bell-config))

(use-package melancholy-theme)
(use-package gruvbox-theme)

(load-theme 'doom-gruvbox t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(use-package autothemer
  :config
  (setq autothemer--theme 'doom-gruvbox))
