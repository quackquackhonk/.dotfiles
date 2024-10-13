;;; init.el --- Prelude's configuration entry point.
;;; Commentary:
;;; Code:

(defvar qqh-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "[qqh] Emacs is powering up... Be patient, %s!" qqh-user)

(when (version< emacs-version "27.1")
  (error "[qqh]" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define directory structure
(defvar qqh-dir (file-name-directory load-file-name)
  "The root dir of my emacs distribution.")
(defvar qqh-core-dir (expand-file-name "core" qqh-dir)
  "The home of qqh's core functionality.")
(defvar qqh-modules-dir (expand-file-name  "mods" qqh-dir)
  "This directory houses all qqh's modules files.")
(defvar qqh-user-dir (expand-file-name "qqh" qqh-dir)
  "This directory is for your user configuration.")
(defvar qqh-user-preload-dir (expand-file-name "preload" qqh-user-dir)
  "This directory is for your user configuration, loaded before the rest of emacs")
(defvar qqh-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar qqh-modules-file (expand-file-name "qqh-modules.el" qqh-modules-dir)
  "This file contains a list of modules that will be loaded by qqh.")

(unless (file-exists-p qqh-savefile-dir)
  (make-directory qqh-savefile-dir))

(defun qqh-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (prelude-add-subfolders-to-load-path name)))))

;; add qqh's directories to Emacs's `load-path'
(add-to-list 'load-path qqh-core-dir)
(add-to-list 'load-path qqh-modules-dir)
;; (qqh-add-subfolders-to-load-path qqh-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the user settings from `qqh-user-preload-dir'
(when (file-exists-p qqh-user-preload-dir)
  (message "[qqh] Loading user configuration files in %s..." qqh-user-preload-dir)
  (mapc 'load (directory-files qqh-user-preload-dir 't "^[^#\.].*el$")))

(message "[qqh] Loading qqh's core modules...")

;; load the core stuff
(require 'qqh-packages)
(require 'qqh-custom)  ;; Needs to be loaded before core, editor and ui
(require 'qqh-ui)
(require 'qqh-core)
(require 'qqh-mode)
(require 'qqh-defs)
(require 'qqh-editor)
(require 'qqh-global-binds)
;; 
;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'qqh-macos))
;; 
;; ;; Linux specific settings
;; (when (eq system-type 'gnu/linux)
;;   (require 'qqh-linux))
;; 
;; WSL specific setting
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (require 'qqh-wsl))

(message "[qqh] Loading qqh's additional modules...")

;; Load my modules
(if (file-exists-p qqh-modules-file)
    (load qqh-modules-file)
  (message "[qqh] Missing user modules file %s" qqh-modules-file))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" qqh-user-dir))

;; load the user settings (this includes `custom-file')
(when (file-exists-p qqh-user-dir)
  (message "[qqh] Loading user configuration files in %s..." qqh-user-dir)
  (mapc 'load (delete
               qqh-modules-file
               (directory-files qqh-user-dir 't "^[^#\.].*\\.el$"))))

(message "[qqh] Emacs is ready to go, %s!" qqh-user)
;;; init.el ends here
