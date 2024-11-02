
;;; bindings.el --- Global bindings -*- lexical-binding: t -*-

;;; Commentary:
;;;
;;;  - Meow
;;;  - Global Keybindings

;;; Code:


;;; Some declarations for Keybindings

(defun qqh/kill-buffer ()
  (interactive)
  (persp-kill-buffer* (current-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Meow for modal editing
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
        '((?\( . round)
          (?\) . round)
          (?\[ . square)
          (?\] . square)
          (?\{ . curly)
          (?\} . curly)
          (?\" . string)
          (?s . symbol)
          (?w . window)
          (?b . buffer)
          (?b . paragraph)
          (?l . line)
          (?v . visual-line)
          (?f . defun)
          (?\. . sentence)))

  ;; Change the keys used by keypad mode
  (setq meow-keypad-ctrl-meta-prefix ?\r          ;; Use RET for C-M-
        meow-keypad-meta-prefix ?z                ;; Use z for M-
        meow-keypad-literal-prefix ?\')           ;; Use ' for literal keys

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
   '(":" . eval-expression)
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
   '("(" . meow-block)
   '(")" . meow-to-block)
   '("/" . meow-visit)
   '("a" . embark-act)
   '("A" . ignore)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill)
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
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-append)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)
   '("RET" . avy-goto-line)
   ;; Some vim-like bindings
   '(":" . meow-M-x)
   '("="   . meow-indent)
   '("C-q" . delete-window)
   '("M-q" . qqh/kill-buffer)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)
;; enable esc mode for terminal use
(meow-esc-mode 1)

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
