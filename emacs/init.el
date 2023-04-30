(package-initialize)
(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files
   '("~/dropbox/agenda/work/job-search.org" "/home/sahana/dropbox/agenda/classes/cs7600.org" "/home/sahana/dropbox/agenda/research.org" "/home/sahana/dropbox/agenda/personal.org"))
 '(package-selected-packages
   '(org-roam perspective hl-todo multi-vterm helm-fuzzy-find vterm-toggle ccls lsp-ui lsp-mode magit helm-rg helm-xref helm-lsp helm evil-snipe counsel swiper company evil-surround evil-numbers evil use-package rainbow-delimiters ivy doom-themes doom-modeline command-log-mode))
 '(warning-suppress-log-types '((use-package)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
