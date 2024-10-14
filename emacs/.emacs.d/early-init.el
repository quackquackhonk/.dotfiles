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

(setq vc-follow-symlinks t)                             ; auto follow VC links
(setq indicate-empty-lines t)
(setq inhibit-startup-message t)
(setq dired-kill-when-opening-new-dired-buffer t)

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq default-frame-alist '((fullscreen . maximized)
                            ;(undecorated-round . t)
                            ;; Setting the face in here prevents flashes of
                            ;; color as the theme gets activated
                            (background-color . "#000000")
                            (foreground-color . "#ffffff")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

;; Some important environment variable
;; TODO: This needs to be in macos specific setting dir
(setenv "LIBRARY_PATH" 
        "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14")

(setenv "COLORTERM" "truecolor")
(setenv "TERM" "xterm-256color")
