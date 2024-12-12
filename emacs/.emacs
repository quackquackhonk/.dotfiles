;;; .emacs --- My entry point to my emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;;: This basically just loads my early-init.el and init.el in that order.

;;; Code:
(load-file (concat (getenv "HOME") "/.emacs.d/early-init.el"))
(setq-default user-init-file (concat (getenv "HOME") "/.emacs.d/init.el"))
(load-file user-init-file)

;;; .emacs ends here
