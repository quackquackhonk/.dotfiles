
;;; bindings.el --- Global bindings -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;;  - Meow
;;;  - Global Keybindings

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Evil Mode
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install meow
(use-package meow :ensure t)

(defun meow-setup ()

  ;; colemak-dh cheatsheet
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)

  ;; sets the thing table characters to use ([{ for grouping
  ;; punctuations
  (setq meow-char-thing-table
        '((40 . round)
          (41 . round)
          (91 . square)
          (93 . square)
          (123 . curly)
          (125 . curly)
          (34 . string)
          (115 . symbol)
          (119 . window)
          (98 . buffer)
          (112 . paragraph)
          (108 . line)
          (118 . visual-line)
          (102 . defun)
          (46 . sentence)))

  (meow-motion-overwrite-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))

  ;; default meow leader bindings
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("SPC" . consult-buffer)
   '("TAB" . other-window)
   '("," . meow-last-buffer)
   '("g" . magit)
   '("f" . consult-fd)
   '("p" . projectile-command-map)
   '("RET" . avy-goto-line)

   '(";r" . (lambda ()
              (interactive)
              (load-file user-init-file)))
   '(";c" . (lambda ()
              (interactive)
              (find-file user-init-file)))

   ;; To execute the originally e in MOTION state, use SPC e.
   '("e" . "H-e")
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
;; enable esc mode for terminal use
(meow-esc-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global Keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qqh/kill-buffer ()
  (interactive)
  (persp-kill-buffer* (current-buffer)))

(meow-normal-define-key
 '("C-q" . delete-window)
 '("M-q" . qqh/kill-buffer))


;;   ;; defines leader key bindings
;;   (qqh/leader-definer
;;     ;; top level bindings
;;     "SPC" 'consult-buffer
;;     "TAB" 'other-window
;;     "RET" 'avy-goto-char-2
;;     "g" 'magit
;;     "," 'evil-switch-to-windows-last-buffer
;;     ":" 'eval-expression

;;     ;; Open (o)
;;     "of" 'find-file
;;     "oi" 'consult-imenu
;;     "ot" 'eat-project
;;     "od" 'flymake-diagnostics

;;     ;; emacs (;)
;;     ";r" (lambda ()
;; 	   (interactive)
;; 	   (load-file user-init-file))
;;     ";c" (lambda ()
;; 	   (interactive)
;; 	   (find-file user-init-file))
;;     ";p" 'qqh/open-project-org-file


;;     ;; global org bindings (;o)
;;     ";oa" 'org-agenda
;;     ";oc" 'org-roam-capture
;;     ";ol" 'org-roam-node-insert
;;     ";on" 'org-roam-node-find)

;;   ;; evil LSP keybindings
;;   (general-def
;;     :states '(normal)
;;     "gR" 'eglot-rename)

;;; bindings.el ends here
