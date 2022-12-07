(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(xterm-mouse-mode)          ; enable mouse control in terminal
(global-hl-line-mode)       ; cursor line
(menu-bar-mode -1)          ; Disable the menu b
(electric-pair-mode)        ; auto pairs
(electric-indent-mode)      ; auto pairs
(setq visible-bell t)       ; Set up the visible bell
(setq vc-follow-symlinks t) ; auto follow VC links
(setq indicate-empty-lines t)

;; column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; only "y or n" prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable file backups
(setq backup-inhibited t)
(setq auto-save-default nil)

;; expand tabs into spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
