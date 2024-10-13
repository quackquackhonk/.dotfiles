;;; qqh-packages.el --- qqh: default package selection.

;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'package)

;;;; Package setup and additional utility functions

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(if (eq system-type 'windows-nt)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t))

;; load the pinned packages
(let ((qqh-pinned-packages-file (expand-file-name "qqh-pinned-packages.el" qqh-dir)))
  (if (file-exists-p qqh-pinned-packages-file)
      (load qqh-pinned-packages-file)))

;; set package-user-dir to be relative to qqh install path
(setq package-user-dir (expand-file-name "elpa" qqh-dir))
(package-initialize)

;; install & enable use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(defvar qqh-packages
  '(ace-window
    ag
    avy
    anzu
    browse-kill-ring
    crux
    discover-my-major
    diff-hl
    diminish
    easy-kill
    editorconfig
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    git-modes
    guru-mode
    hl-todo
    imenu-anywhere
    projectile
    magit
    move-text
    nlinum
    operate-on-number
    smartparens
    smartrep
    super-save
    undo-tree
    volatile-highlights
    which-key
    zenburn-theme
    zop-to-char)
  "A list of packages to ensure are installed at launch.")

(defun qqh-packages-installed-p ()
  "Check if all packages in `qqh-packages' are installed."
  (cl-every #'package-installed-p qqh-packages))

(defun qqh-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package qqh-packages)
    (add-to-list 'qqh-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun qqh-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'qqh-require-package packages))

(defun qqh-install-packages ()
  "Install all packages listed in `qqh-packages'."
  (unless (qqh-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "[qqh/packages] qqh is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" "[qqh/packages] done.")
    ;; install the missing packages
    (qqh-require-packages qqh-packages)))

;; run package installation
(qqh-install-packages)

(defun qqh-list-foreign-packages ()
  "Browse third-party packages not bundled with qqh.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `qqh-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-set-difference package-activated-list qqh-packages)))

;;;; Auto-installation of major modes on demand

(defmacro qqh-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar qqh-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; same with adoc-mode
(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

;; and pkgbuild-mode
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (qqh-auto-install extension package mode))))
 qqh-auto-install-alist)

(provide 'qqh-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; qqh-packages.el ends here
