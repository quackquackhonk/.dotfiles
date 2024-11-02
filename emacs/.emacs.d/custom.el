;;; custom.el
;;; Commentary:
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(custom-safe-themes
   '("e9aa348abd3713a75f2c5ba279aa581b1c6ec187ebefbfa33373083ff8004c7c" "6454421996f0508c38215a633256e36c19a28591542fb0946cfc40f1dceb89cf" default))
 '(ignored-local-variable-values
   '((eval progn
           (defun qqh/venv-on nil
             (pyvenv-activate "/Users/i34866/code/amps/amps-surface-roughness/spack_env/.spack-env/view")))))
 '(package-selected-packages
   '(avy cape cargo catppuccin-theme cmake-mode consult-todo corfu-terminal devdocs diminish dts-mode eat eglot-booster eldoc-box embark-consult exec-path-from-shell fancy-compilation forge grip-mode hl-todo json-mode just-mode kind-icon ligature marginalia meow nerd-icons orderless persp-projectile protobuf-mode pyvenv rainbow-delimiters rainbow-mode ripgrep rust-mode solaire-mode vertico wgrep yaml-mode))
 '(package-vc-selected-packages
   '((eglot-booster :vc-backend Git :url "https://github.com/jdtsmith/eglot-booster")
     (rose-pine-emacs :vc-backend Git :url "https://github.com/thongpv87/rose-pine-emacs.git")))
 '(safe-local-variable-values
   '((eval progn
           (pyvenv-activate
            (concat
             (projectile-project-root)
             "spack_env/.spack-env/view")))
     (eval progn
           (pyvenv-activate
            (projectile-project-root)
            "spack_env/.spack-env/view"))
     (eval progn
           (defun qqh/venv-on nil
             (interactive)
             (pyvenv-activate "/Users/i34866/code/amps/amps-surface-roughness/spack_env/.spack-env/view")))
     (eval progn
           (defun my-project-specific-function nil
             (pyvenv-activate "/Users/i34866/code/amps/amps-surface-roughness/spack_env/.spack-env/view")))
     (eval progn
           (message "chang")
           (defun qqh/venv-on nil
             (pyvenv-activate "/Users/i34866/code/amps/amps-surface-roughness/spack_env/.spack-env/view"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
