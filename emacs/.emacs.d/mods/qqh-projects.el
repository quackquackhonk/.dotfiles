;;; qqh-projects.el --- configure packages related to projects
;;; Commentary:
;;; Code:

(qqh-require-packages '(projectile perspective persp-projectile consult))

;; Enable projectile
(projectile-mode +1)
(setq projectile-project-search-path '(("~/code/" . 2)))
(setq projectile-switch-project-action 'consult-fd)

;; Enable perspective
(setq persp-mode-prefix-key (kbd "C-c C-p"))
(persp-mode)

(require 'persp-projectile)

(provide 'qqh-projects)
;;; qqh-projects.el ends here
