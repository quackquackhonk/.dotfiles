;;; .emacs --- My entry point to my emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;;: This basically just loads my early-init.el and init.el in that order.

;;; Code:
(load-file "/home/sahana/.emacs.d/early-init.el")
(setq-default user-init-file "/home/sahana/.emacs.d/init.el")
(load-file user-init-file)

;;; .emacs ends here
