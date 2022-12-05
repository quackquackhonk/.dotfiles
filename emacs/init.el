
(add-to-list 'load-path "~/.emacs.d/config/")
(load-library "packages")
(load-library "settings")
(load-library "keys")
(load-library "settings")
(load-library "lsp")
(load-library "theme")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit lsp-ivy lsp-ui lsp-mode flycheck vterm general evil-commentary counsel swiper company evil-surround evil-numbers evil use-package rainbow-delimiters ivy doom-themes doom-modeline command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
