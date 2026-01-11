;;; .emacs --- My entry point to my emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;;: This basically just loads my early-init.el and init.el in that order.
;;; This is the file that gets linked by my home-manager configuration

;;; Code:
(setq-default early-init-file (concat (getenv "HOME") "/dotfiles/emacs/.emacs.d/early-init.el"))
(setq-default user-init-file (concat (getenv "HOME") "/dotfiles/emacs/.emacs.d/init.el"))
(load-file early-init-file)
(load-file user-init-file)

;;; .emacs ends here
