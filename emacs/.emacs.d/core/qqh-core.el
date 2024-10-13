;;; qqh-core.el --- Core emacs functions.

;;; Commentary:

;; Here are the definitions of most of the general-purpose functions and
;; commands added by qqh.  Some modules define additional module-specific
;; functions and commands.
;;
;; Note that many of the original core qqh commands were extracted to the
;; crux package (qqh installs it automatically).  qqh's auto-save
;; functionality was extracted to the super-save package.

;;; Code:

(require 'cl-lib)

(defun qqh-buffer-mode (buffer-or-name)
  "Retrieve the `major-mode' of BUFFER-OR-NAME."
  (with-current-buffer buffer-or-name
    major-mode))

(defun qqh-search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if mark-active
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro qqh-install-search-engine (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "qqh-%s" search-engine-name)) ()
       ,(format "Search %s with a query or region if any." search-engine-name)
       (interactive)
       (qqh-search ,search-engine-url ,search-engine-prompt)))

(qqh-install-search-engine "google"     "http://www.google.com/search?q="              "Google: ")
(qqh-install-search-engine "youtube"    "http://www.youtube.com/results?search_query=" "Search YouTube: ")
(qqh-install-search-engine "github"     "https://github.com/search?q="                 "Search GitHub: ")
(qqh-install-search-engine "duckduckgo" "https://duckduckgo.com/?t=lm&q="              "Search DuckDuckGo: ")

(defun qqh-recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory qqh-dir 0))

(defvar qqh-tips
  '("Press <C-c o> to open a file with external program."
    "Press <C-c p f> to navigate a project's files."
    "Press <s-r> to open a recently visited file."
    "Press <C-c p s g> to run grep on a project."
    "Press <C-c p p> to switch between projects."
    "Press <C-=> to expand the selected region."
    "Press <C-c C-/ g> to search in Google."
    "Press <C-c C-/ h> to search in GitHub."
    "Press <C-c C-/ y> to search in YouTube."
    "Press <C-c C-/ d> to search in DuckDuckGo."
    "Press <C-c r> to rename the current buffer and the file it's visiting if any."
    "Press <C-c t> to open a terminal in Emacs."
    "Press <C-c k> to kill all the buffers, but the active one."
    "Press <C-x g> to run magit-status."
    "Press <C-c D> to delete the current file and buffer."
    "Press <C-c s> to swap two windows."
    "Press <S-RET> or <M-o> to open a line beneath the current one."
    "Press <s-o> to open a line above the current one."
    "Press <C-c C-z> in a Elisp buffer to launch an interactive Elisp shell."
    "Press <C-Backspace> to kill a line backwards."
    "Press <C-S-Backspace> or <s-k> to kill the whole line."
    "Press <s-j> or <C-^> to join lines."
    "Press <s-.> or <C-c v> to jump to the start of a word in any visible window."
    "Press <f12> to toggle the menu bar."
    "Explore the qqh menu to find out about some of qqh extensions to Emacs."
    "Access the official Emacs manual by pressing <C-h r>."))

(defun qqh-tip-of-the-day ()
  "Display a random entry from `qqh-tips'."
  (interactive)
  (when (and qqh-tips (not (window-minibuffer-p)))
    ;; pick a new random seed
    (random t)
    (message
     (concat "qqh tip: " (nth (random (length qqh-tips)) qqh-tips)))))

(defun qqh-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(require 'epl)

(defun qqh-update ()
  "Update qqh to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update qqh? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating qqh...")
    (cd qqh-dir)
    (shell-command "git pull")
    (qqh-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun qqh-update-packages (&optional arg)
  "Update qqh's packages.
This includes package installed via `qqh-require-package'.

With a prefix ARG updates all installed packages."
  (interactive "P")
  (when (y-or-n-p "Do you want to update qqh's packages? ")
    (if arg
        (epl-upgrade)
      (epl-upgrade (cl-remove-if-not (lambda (p) (memq (epl-package-name p) qqh-packages))
                                     (epl-installed-packages))))
    (message "Update finished. Restart Emacs to complete the process.")))

(defun qqh-wrap-with (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))

(provide 'qqh-core)
;;; qqh-core.el ends here
