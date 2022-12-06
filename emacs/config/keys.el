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

;; TODO: needs to actually bind these
(use-package evil-numbers)

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
  (setq which-key-idle-delay 0.5))

(use-package general
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "S-SPC"))

(defun my/emacs-reload ()
  (interactive)
  (load-file user-init-file))

;; defines leader key bindings
(my/leader-keys
  ;; top level bindings
  "SPC" 'switch-to-buffer
  "," 'switch-to-prev-buffer
  "." 'switch-to-next-buffer
  "q" 'kill-current-buffer
  ;; misc
  "/r" 'my/emacs-reload
  ;; projectile
  "p" 'projectile-command-map
  ;; git bindings
  "gg" 'magit
  ;; file bindings: f
  "ff" 'find-file
  "fq" 'kill-buffer)


