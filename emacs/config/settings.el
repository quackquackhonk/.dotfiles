(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(xterm-mouse-mode)          ; enable mouse control in terminal
(menu-bar-mode -1)          ; Disable the menu b
(setq visible-bell t)       ; Set up the visible bell
(setq indicate-empty-lines t)

;; disable file backups 
(setq backup-inhibited t)
(setq auto-save-default nil)

;; expand tabs into spaces
(setq default-tab-width 4)
(setq tab-width 4)

