(require 'package)
(require 'use-package)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-gruvbox 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
