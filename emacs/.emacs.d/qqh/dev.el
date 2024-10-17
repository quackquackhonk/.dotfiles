;;; dev.el --- Configure tools for development -*- lexical-binding: nil; -*-

;;; Commentary:
;;;  - Built-in config for developers
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
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)))
  :hook
  ;; Auto parenthesis matching
  ((prog-mode . electric-pair-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit)

(use-package forge
  :config
  (push '("gitlab.veriskweather.net"               ; GITHOST
	  "gitlab.veriskweather.net/api/v4"        ; APIHOST
	  "gitlab.veriskweather.net"               ; WEBHOST and INSTANCE-ID
	  forge-gitlab-repository)                 ; CLASS
	forge-alist)

  ;; COnfigure auth source
  (setq auth-sources '("~/.authinfo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Project Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '(("~/code/" . 2)
					 "~/.sources/")
	projectile-mode-line-prefix " In "
        projectile-switch-project-action 'consult-fd))

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

(use-package yaml-mode)
(use-package json-mode)
(use-package protobuf-mode)

(use-package just-mode)
(use-package cmake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

;; (use-package eglot
;;   :ensure t
;;   :custom
;;   (eglot-send-changes-idle-time 0.1)
;;   (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files
;; 
;;   :config
;;   ;; dont log every event
;;   (fset #'jsonrpc--log-event #'ignore))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   LSP-mode configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Taken from emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
	 (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
		       (fboundp 'json-parse-buffer))
		'json-parse-buffer
	      'json-read)
	    :around
	    #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)

  :init

  :config
  (add-hook 'lsp-mode-hook
            (lambda () (setq display-line-numbers 'relative)))

  (setq lsp-warn-no-matched-clients nil
        lsp-auto-execute-action nil)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.spack_env\\'"))

;; Diagnostics
(use-package flymake
  :after lsp-mode
  :hook (('emacs-lisp-mode-hook . flymake-mode)))

;; Snippets
(use-package yasnippet)

;; Devdocs
(use-package devdocs
  :hook (('python-ts-mode-hook . (lambda () (setq-local devdocs-current-docs '("python~3.11"))))
	 ('c-mode-hook . (lamdba () (setq-local devdocs-current-docs '("c"))))
	 ('c++-mode-hook . (lamdba () (setq-local devdocs-current-docs '("cpp"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Language specific configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TODO: C/C++
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) . lsp-deferred)
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "ccls")))

;;; PYTHON
;; Built-in Python utilities
(use-package python
  :bind (:map python-ts-mode-map
	      ("C-c C-SPC" . qqh/python-lsp-mode))
  :custom
  (python-shell-interpreter "python3")

  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :config
  (setenv "WORKON_HOME" "/opt/homebrew/Caskroom/miniconda/base/envs/")
  (pyvenv-mode 1))

;; Taken from pyvenv.el - work on the environment, then start an lsp
(defun qqh/python-lsp-mode (name)
  "Activate a virtual environment from $WORKON_HOME.

    If the virtual environment NAME is already active, this function
    does not try to reactivate the environment."
  (interactive
   (list
    (completing-read "Work on: " (pyvenv-virtualenv-list)
		     nil t nil 'pyvenv-workon-history nil nil)))

  (unless (member name (list "" nil pyvenv-virtual-env-name))
    (pyvenv-activate (format "%s/%s"
			     (pyvenv-workon-home)
			     name)))
  (lsp-mode))

(add-hook 'python-ts-mode-hook 'qqh/python-lsp-mode)

;; Buffer formatting with Black
(use-package blacken
  :defer t
  :hook (python-ts-mode-hook . blacken-mode))


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
