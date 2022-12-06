(require 'package)
(require 'use-package)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :ensure t
  :config
  (doom-themes-visual-bell-config)

  (load-theme 'doom-gruvbox 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))
