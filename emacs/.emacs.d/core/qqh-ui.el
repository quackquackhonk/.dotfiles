;;; qqh-ui.el --- Emacs qqh: UI optimizations and tweaks.
;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (zenburn).

;;; License:
;;; Code:
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when qqh-minimalistic-ui
  (menu-bar-mode -1))
(setq default-frame-alist '((undecorated-round . t)))
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line numbers at the beginning of each line
(unless qqh-minimalistic-ui
  ;; there's a built-in linum-mode, but we're using
  ;; display-line-numbers-mode or nlinum-mode,
  ;; as it's supposedly faster
  (if (fboundp 'global-display-line-numbers-mode)
      (global-display-line-numbers-mode)
      (global-nlinum-mode t)))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " qqh - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; configure catppuccin theme
(when qqh-theme
  (load-theme qqh-theme :no-confirm)
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

;; show available keybindings after you start typing
;; add to hook when running as a daemon as a workaround for a
;; which-key bug
;; https://github.com/justbur/emacs-which-key/issues/306
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode +1))

(provide 'qqh-ui)
;;; qqh-ui.el ends here
