(require 'package)
(require 'use-package)

;; EVIL
(use-package evil
  :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "g +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "g -") 'evil-numbers/dec-at-pt))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1.5))

(use-package hydra)
(defhydra hydra-windows (:hint nil :rows 1)
  "Window Navigation..."
  ;; navigating windows
  ("<left>" evil-window-left)
  ("<up>" evil-window-up)
  ("<down>" evil-window-down)
  ("<right>" evil-window-right)
  ;; make windows  
  ("v" evil-window-vsplit)
  ("s" evil-window-split)
  ("q" evil-quit))

(defun my/toggle-relative-line ()
  (interactive)
  (if (eq display-line-numbers 'relative)
      (setq display-line-numbers t)
      (setq display-line-numbers 'relative)))

(defun my/emacs-reload ()
  (interactive)
  (load-file user-init-file))

(use-package general)

(defconst my/leader "SPC")
(defconst my/global-leader "S-SPC")

;; keybindings
(general-create-definer my/leader-top
  :keymaps '(normal insert visual emacs)
  :prefix my/leader
  :global-prefix my/global-leader)

;; defines leader key bindings
(my/leader-top
  ;; top level bindings
  "SPC" 'switch-to-buffer
  "," 'switch-to-prev-buffer
  "." 'switch-to-next-buffer
  "q" 'kill-current-buffer
  ;; misc
  "/r" 'my/emacs-reload
  ;; toggles (t)
  "tr" 'my/toggle-relative-line
  ;; projectile
  "p" 'projectile-command-map
  ;; LSP
  "ld" 'lsp-find-definition
  "lr" 'lsp-ui-peek-find-references
  "lc" 'lsp-rename
  "lI" 'lsp-ui-imenu
  "l SPC" 'lsp-execute-code-action
  ;; windows
  "w" 'hydra-windows/body
  ;; git bindings
  "gg" 'magit
  ;; files
  "ff" 'find-file
  "fs" 'swiper
  "fq" 'kill-buffer)

