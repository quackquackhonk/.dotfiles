;;; config.el --- -*- lexical-binding: t -*-
;; --------------------------------------------------------------------------------------
;; | Basic DOOM Emacs configuration                                                     |
;; --------------------------------------------------------------------------------------

;; Name and Email
(setq user-full-name "Sahana Tankala"
      user-mail-address "sahanatankala@gmail.com")

;; Theme
(setq doom-theme 'doom-nord)

;; Font
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'Regular))

;; Default line numbers
(setq display-line-numbers-type 'relative)
;; get it out of here
(global-prettify-symbols-mode 1)

;; Disable confirm kill
(setq confirm-kill-emacs nil)

;; put this in the doom-dashboard-widget-banner function
;; '("^...^"
;;             "/ o,o \\"
;;             "|):::(|git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d"
;;             "===w=w==="
;;             ""
;;             ""
;;             ""
;;             ""
;;             ""
;;             ""
;;             "")
;; --------------------------------------------------------------------------------------
;; | Org Mode configuration                                                             |
;; --------------------------------------------------------------------------------------

;; File template
(set-file-template! "\\.org$" :trigger "__org" :mode 'org-mode)

;; Customize variables
(setq
 ;; Org directory is in my Dropbox
 org-directory "~/Dropbox/"
 org-agenda-files '("~/Dropbox/agenda" "~/Dropbox/projects")
 org-icalendar-combined-agenda-file "~/Dropbox/calendar.ics"
 ;; Change ... to 
 org-ellipsis "  "
 ;; startup overview
 org-startup-folded 'overview
 ;; Hide emphasis markers like *...* or /.../
 org-hide-emphasis-markers t
 ;; Additional spacing for lists
 org-list-indent-offset 2
 org-file-apps '((auto-mode . emacs)
                 (directory . emacs)
                 ("\\.mm\\'" . default)
                 ("\\.x?html?\\'" . default)
                 ("\\.pdf\\'" . "zathura %s"))
 org-todo-keywords '(((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "QUICK(q)" "|" "DONE(d)" "KILL(k)")
                      (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))))

;; Configuring org-super-agenda
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Scheduled Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Due Today"
                                   :deadline today)
                                  (:name "Coop"
                                   :tag "coop")
                                  (:name "Assignments"
                                   :tag ("homework" "work"))
                                  (:name "Lectures / Readings"
                                   :tag "reading")))
  :config
  (org-super-agenda-mode))
;; agenda keybindings
;; TODO custom keybindings for custom views
;; one view with schedule for day, important items, quick items, and due today's
;; one view with no schedule, just homework, work, readings, etc for finding what to do

;; Configuring org-gcal package
;; (use-package! org-gcal
;;   :init
;;   (setq org-gcal-client-id "498090252798-s85ohiag6e608s8pt2arh85faqr4e351.apps.googleusercontent.com"
;;       org-gcal-client-secret "ecr3iD4XShO_2YXSHaHeyupE"
;;       org-gcal-file-alist '(("tankalavarun@gmail.com" . "~/Dropbox/agenda/gcal.org"))))

;; Make LaTeX previews much larger in org files
(after! org
        (if (or (eq (x-display-pixel-height) 1080) (eq (x-display-pixel-width) 1920))
                (setq org-format-latex-options '(:foreground default :background default :scale 2.0
                                                 :html-foreground "Black" :html-background "Transparent"
                                                 :html-scale 2.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
                (setq org-format-latex-options '(:foreground default :background default :scale 3.0
                                                 :html-foreground "Black" :html-background "Transparent"
                                                 :html-scale 3.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))))


;; prettier bullets
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([+]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; start org-fragtog-mode for all org-mode files
(add-hook! 'org-mode-hook
           #'org-fragtog-mode
           (lambda () (org-bullets-mode 1)))

(add-hook! 'org-agenda-mode-hook
           (lambda () (org-gcal-sync)))
;; add '$' to the auto-pair list, for LaTeX fragments
(sp-local-pair 'org-mode "$" "$")

;; Keybindings
;; Add keybindings for org-gcal package
(map! :map org-mode-map
      :localleader
      (:prefix ("j" . "gcal")
       :desc "Sync with gcal" "s" #'org-gcal-sync
       :desc "Fetch gcal events" "f" #'org-gcal-fetch
       :desc "Post new event" "p" #'org-gcal-post-at-point
       :desc "Delete event" "d" #'org-gcal-delete-at-point))


;; --------------------------------------------------------------------------------------
;; | Coding configuration                                                               |
;; --------------------------------------------------------------------------------------

;; Set projectile search path
;; On startup, look through this directory for git repos and add them to my project list
(setq projectile-project-search-path '("~/Code/"))

;; SML
;; Enable electric-indent mode in the current sml-mode buffer
;; This gives me sane indentation on <RET>
(add-hook 'sml-mode-hook (lambda () (electric-indent-local-mode 1)))

;; HASKELL
;; Pretty symbols for haskell code
;; (setq haskell-font-lock-symbols t)
(setq-hook! 'haskell-mode-hook
  haskell-indentation-layout-offset 4
  haskell-indentation-left-offset 4
  haskell-indentation-ifte-offset 4)

;; C
;; Get better indentation
(setq c-default-style "stroustrup")
;; disable auto format
(defun vt/c-after-change-hook ()
  (format-all-mode 0))
(add-hook 'c-mode-hook 'vt/c-after-change-hook)
