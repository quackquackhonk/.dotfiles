(require 'package)
(require 'use-package)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config))

(use-package melancholy-theme)
(use-package gruvbox-theme)

(load-theme 'doom-gruvbox t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

(provide 'my/theme)
