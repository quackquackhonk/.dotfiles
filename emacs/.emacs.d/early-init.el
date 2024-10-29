;;; early-init.el

;;; Commentary:

;;; Basic settings for quick startup and convenience

;;; Code:

;; Startup speed, annoyance suppression
(setq qqh/initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

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
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))



;; Setup PATH
(add-to-list 'exec-path (format "%s/.local/bin" (getenv "HOME")))
(add-to-list 'exec-path (format "%s/.cargo/bin" (getenv "HOME")))

(when (and (eq system-type 'darwin))
  (setenv "LIBRARY_PATH"
          "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/Caskroom/miniconda/base/bin"))

(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
 (add-to-list 'exec-path "/home/sahana/.local/share/mise/installs/python/3.12/bin/"))

(setenv "TERM" "xterm-256color")

;;; early-init.el ends here
