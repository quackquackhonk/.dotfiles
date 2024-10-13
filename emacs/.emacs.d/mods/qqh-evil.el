;;; qqh-evil.el --- Emacs qqh: evil-mode configuration.
;;; Commentary:

;; Some basic configuration for evil-mode.

;;; Code:
;;; goto-chg lets you use the g-; and g-, to go to recent changes
;;; evil-visualstar enables searching visual selection with *
;;; evil-numbers enables vim style numeric incrementing and decrementing

(qqh-require-packages '(evil goto-chg undo-fu evil-collection evil-surround evil-visualstar evil-numbers))

(require 'evil-visualstar)

(setq evil-mode-line-format 'before)

(setq evil-emacs-state-cursor  '("red" box))
(setq evil-normal-state-cursor '("gray" box))
(setq evil-visual-state-cursor '("gray" box))
(setq evil-insert-state-cursor '("gray" bar))
(setq evil-motion-state-cursor '("gray" box))

(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-shift-width 4)
(setq evil-esc-delay 0)

(evil-mode 1)
(evil-set-undo-system 'undo-fu)

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; prevent esc-key from translating to meta-key in terminal mode

(evil-mode 1)
(global-evil-surround-mode 1)

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-a") 'evil-numbers/dec-at-pt)

(define-key evil-normal-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
(define-key evil-normal-state-map (kbd "g C-S-a") 'evil-numbers/dec-at-pt-incremental)

;;
;; Other useful Commands
;;
(evil-ex-define-cmd "W"     'evil-write-all)
(evil-ex-define-cmd "Tree"  'speedbar-get-focus)
(evil-ex-define-cmd "linum" 'linum-mode)
(evil-ex-define-cmd "Align" 'align-regexp)

(defun qqh-yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(define-key evil-normal-state-map
  (kbd "Y") 'qqh-yank-to-end-of-line)

(defun qqh-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun qqh-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(define-key evil-visual-state-map (kbd ">") 'qqh-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'qqh-shift-left-visual)

;; Scrolling
(defun qqh-evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(defun qqh-evil-scroll-up-other-window ()
  (interactive)
  (scroll-other-window '-))

(define-key evil-normal-state-map
  (kbd "C-S-d") 'qqh-evil-scroll-down-other-window)

(define-key evil-normal-state-map
  (kbd "C-S-u") 'qqh-evil-scroll-up-other-window)

;;
;; Magit from avsej
;;
(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard
  "L" 'magit-log)
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard
  "l" 'magit-log
  "h" 'magit-diff-toggle-refine-hunk)

(setq evil-shift-width 2)

;;; snagged from Eric S. Fraga
;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-05/msg00153.html
(defun qqh-evil-key-bindings-for-org ()
  ;;(message "Defining evil key bindings for org")
  (evil-declare-key 'normal org-mode-map
    "gk" 'outline-up-heading
    "gj" 'outline-next-visible-heading
    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
    "t" 'org-todo ; mark a TODO item as DONE
    ",c" 'org-cycle
    (kbd "TAB") 'org-cycle
    ",e" 'org-export-dispatch
    ",n" 'outline-next-visible-heading
    ",p" 'outline-previous-visible-heading
    ",t" 'org-set-tags-command
    ",u" 'outline-up-heading
    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
    "^" 'org-beginning-of-line ; ditto
    "-" 'org-ctrl-c-minus ; change bullet style
    "<" 'org-metaleft ; out-dent
    ">" 'org-metaright ; indent
    ))
(qqh-evil-key-bindings-for-org)
(provide 'qqh-evil)
