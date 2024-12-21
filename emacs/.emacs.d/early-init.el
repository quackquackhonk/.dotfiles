;;; early-init.el --- Early init config file. -*- lexical-binding: t -*-

;;; Commentary:

;;; Basic settings for quick startup and convenience

;;; Code:

;; Startup speed, annoyance suppression
(setq qqh/initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Disable package.el for elpaca
(setq package-enable-at-startup nil)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq default-frame-alist '((fullscreen . maximized)
                            (background-color . "#000000")
                            (foreground-color . "#FFFFFF")
                            (ns-appearance . dark)))



;; OSX settings
(when (and (eq system-type 'darwin))
  ;; Setup PATH
  (add-to-list 'exec-path (format "%s/.local/bin" (getenv "HOME")))
  (add-to-list 'exec-path (format "%s/.cargo/bin" (getenv "HOME")))

  (setq explicit-shell-file-name "/bin/zsh")
  (setenv "LIBRARY_PATH"
          "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
  (setenv "XDG_CONFIG_HOME" "/Users/i34866/.config")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/opt/llvm/bin")
  (add-to-list 'exec-path "/opt/homebrew/Caskroom/miniconda/base/bin"))

(setenv "TERM" "xterm-256color")

;;; early-init.el ends here
