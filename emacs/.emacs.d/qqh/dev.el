;;; dev.el --- Configure tools for development -*- lexical-binding: nil; -*-

;;; Commentary:
;;;  - Built-in config for developers
;;;  - Tree-Sitter configuration
;;;  - Version Control
;;;  - Project Management
;;;  - Common file types
;;;  - LSP-mode
;;;  - PL Specific Configuration
;;;    - C/C++
;;;    - Python
;;;    - Rust

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :config
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

(use-package tree-sitter-langs)
(use-package tree-sitter
  :after tree-sitter-langs
  :config
  (setq treesit-font-lock-level 4)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook
            #'tree-sitter-hl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit)

(use-package forge
  :config
  ;; Configure auth source
  (setq auth-sources '("~/.authinfo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qqh/open-project-org-file ()
  (interactive)
  (require 'projectile)
  (let ((file     (projectile-expand-root "project.org"))
        (template (expand-file-name "templates/project-template.org"
                                    qqh/modules-dir)))
    (unless (file-exists-p file)
      (copy-file template file))
    (find-file file)))

(use-package projectile
  :init
  (projectile-mode +1)

  (setq projectile-project-search-path '(("~/code/" . 2)
					 "~/.sources/")
	projectile-mode-line-prefix " In "
    projectile-switch-project-action 'qqh/open-project-org-file))

(use-package perspective
  :after consult
  :custom
  (persp-mode-prefix-key (kbd "C-p"))
  :init
  (require 'consult)
  (persp-mode)

  ;; Add perspective mode source to buffer switcher
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode))
  :config
  (setq markdown-list-indent-width 2))

(use-package grip-mode
  :after markdown-mode
  :hook (markdown-mode . grip-mode)
  :config
  (setq grip-use-mdopen t
        grip-mdopen-path "/Users/i34866/.cargo/bin/mdopen"
        grip-preview-use-webkit nil
        grip-update-after-change nil))

(use-package yaml-mode)
(use-package json-mode)
(use-package protobuf-mode)

(use-package dts-mode)

(use-package just-mode)
(use-package cmake-mode)

;; Devdocs
(use-package devdocs
  :hook (('python-mode-hook . (lambda () (setq-local devdocs-current-docs '("python~3.11"))))
	 ('c-mode-hook . (lamdba () (setq-local devdocs-current-docs '("c"))))
	 ('c++-mode-hook . (lamdba () (setq-local devdocs-current-docs '("cpp"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  :ensure t
  :defer
  :hook ((python-mode . eglot-ensure)
	 ((c-mode c++-mode) . eglot-ensure))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  ;; Disable inlay hints globally
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))

  ;; clangd for c/c++
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

  ;; configure pylsp
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:pycodestyle (:enabled t
                                                  :maxLineLength 120))
                                   (:pylsp_mypy (:enabled t
                                                 :strict t
                                                 :live_mode t)))))
  
  ;; PERF: dont log every event
  (fset #'jsonrpc--log-event #'ignore))

(use-package eglot-booster
  ;; Needs to be installed from VC,
  ;; M-x package-vc-install RET https://github.com/jdtsmith/eglot-booster
  :ensure nil
  :after eglot
  :config (eglot-booster-mode))

;; Diagnostics
(use-package flymake
  :after eglot
  :hook (('emacs-lisp-mode-hook . flymake-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language specific configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PYTHON
;; Built-in Python utilities
(use-package python
  :custom
  (python-shell-interpreter "python3")

  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (pyvenv-mode 1))

;; Buffer formatting with Black
(use-package blacken
  :defer t
  :hook (python-mode . blacken-mode))


;;; RUST
(use-package rust-mode
  :config
  ;; rustfmt
  (setq rust-format-show-buffer nil)
  (setq rust-format-on-save t)

  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package cargo
  :after rust-mode)

;;; dev.el ends here
