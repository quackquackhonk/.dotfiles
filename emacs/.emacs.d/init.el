(package-initialize)
(org-babel-load-file "~/.emacs.d/config.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(projectile perspective protobuf-mode consult-todo fish-mode embark-consult fancy-compilation dimmer orderless just-mode cmake-mode consult autothemer solaire-mode hl-todo rainbow-mode rainbow-delimiters melancholy-theme doom-themes doom-modeline cargo rust-mode racket-mode sml-mode pyvenv flycheck-ocaml merlin-eldoc merlin dune tuareg glsl-mode lsp-haskell haskell-mode ccls dap-mode lsp-ui lsp-mode flycheck tree-sitter-langs format-all command-log-mode dockerfile-mode docker-compose-mode docker multi-vterm vterm org-superstar org-appear yasnippet which-key ripgrep org-roam magit hydra general exec-path-from-shell evil-surround evil-snipe evil-numbers evil-commentary evil-collection counsel company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1e1e2e" :foreground "#cdd6f4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "nil" :family "Maple Mono NF")))))
