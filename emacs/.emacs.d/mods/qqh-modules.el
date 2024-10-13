;;; qqh-modules.el --- A listing of modules to load on startup
;;; Commentary:
;;; Code:

;;; Uncomment the modules you'd like to use and restart qqh afterwards

;;; General productivity tools
(require 'qqh-minibuffer)
(require 'qqh-projects)

;;; Org-mode (a legendary productivity tool that deserves its own category)
;;
;; Org-mode helps you keep TODO lists, notes and more.
;; (require 'qqh-org)

;;; Programming languages support
;;
;; Modules for a few very common programming languages
;; are enabled by default.

;; Base setup for the Language Server Protocol
;; (require 'qqh-lsp)
;; (require 'qqh-c)
;; (require 'qqh-lisps)
;; (require 'qqh-python)
;; (require 'qqh-rust)

(provide 'qqh-modules)
;;; qqh-modules.el ends here
