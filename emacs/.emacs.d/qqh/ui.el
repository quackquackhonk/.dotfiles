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

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Modeline configurtaion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                 display
                 (min-width
                  (5.0)))
                mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
                (vc-mode vc-mode)
                "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(setq-default mode-line-format
              '("%e" mode-line-buffer-identification mode-line-modified mode-line-remote
                " @ " mode-line-position evil-mode-line-tag
                (vc-mode vc-mode)
                "  " mode-line-modes mode-line-misc-info))

(force-mode-line-update)

;;; Cleanup settings
(add-hook 'server-after-make-frame-hook #'catppuccin-reload)

(load-theme 'catppuccin :no-confirm t)
(catppuccin-reload)

;;; ui.el ends here.
