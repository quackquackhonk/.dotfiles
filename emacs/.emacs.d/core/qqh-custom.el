;;; qqh-custom.el --- qqh: customizable variables.
;;; Commentary::
;;; Code:

;; customize
(defgroup qqh nil
  "Emacs qqh configuration."
  :prefix "qqh-"
  :group 'convenience)

(defcustom qqh-minimalistic-ui t
  "Controls whether to display the menu-bar and line numbers.
Note that the toolbar is always hidden regardless of this setting."
  :type 'boolean
  :group 'qqh
  :package-version '(qqh . "1.1"))

(defcustom qqh-super-keybindings nil
  "Controls whether to use the Super key in keybindings.
They can be problematic in some operating systems (e.g. Windows)
or desktop environments that make heavy use of them."
  :type 'boolean
  :group 'qqh
  :package-version '(qqh . "1.1"))

(defcustom qqh-auto-save t
  "Non-nil values enable qqh's auto save."
  :type 'boolean
  :group 'qqh)

(defcustom qqh-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'qqh)

(defcustom qqh-whitespace t
  "Non-nil values enable qqh's whitespace visualization."
  :type 'boolean
  :group 'qqh)

(defcustom qqh-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `qqh-whitespace' is also enabled."
  :type 'boolean
  :group 'qqh)

(defcustom qqh-flyspell t
  "Non-nil values enable qqh's flyspell support."
  :type 'boolean
  :group 'qqh)

(defcustom qqh-user-init-file
  (expand-file-name "qqh" user-emacs-directory)
  "Path to your user customization file.
qqh recommends you only put user customizations in the
user folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`crux-find-user-init-file'.  This can be easily set to the desired buffer
in Lisp by putting `(setq qqh-user-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'qqh)

(defcustom qqh-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'qqh)

(defcustom qqh-format-on-save t
  "Run mode specific format on file before it's saved.
Currently only applies to tide-mode."
  :type 'boolean
  :group 'qqh)

(defcustom qqh-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'qqh)

(defcustom qqh-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'qqh)

(defcustom qqh-theme 'catppuccin
  "The default color theme, change this in your /user/preload config."
  :type 'symbol
  :group 'qqh)

(provide 'qqh-custom)

;;; qqh-custom.el ends here
