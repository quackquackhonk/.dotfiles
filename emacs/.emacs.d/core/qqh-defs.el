;;; qqh-defs.el --- qqh: Definitions, helpers, etc.

;;; Commentary:
;;; Code:

(defun qqh/emacs-reload ()
  (interactive)
  (load-file user-init-file))

(defun qqh/open-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun qqh/kill-current-buffer ()
  (interactive)
  (persp-kill-buffer* (current-buffer)))

(provide 'qqh-defs)
