(require 'ert)
(require 'cl-lib)
(require 'espuds)
(require 'f)

(with-eval-after-load "python"
  (setq python-indent-guess-indent-offset-verbose nil))

(let* ((support-path (f-dirname load-file-name))
       (root-path (f-parent (f-parent support-path))))
  (add-to-list 'load-path (concat root-path "/lisp"))
  (add-to-list 'load-path (concat root-path "/tests"))
  (custom-set-variables
   '(gnus-before-startup-hook (quote (toggle-debug-on-error)))
   '(nnreddit-use-virtualenv nil)
   '(auto-revert-verbose nil)
   '(auto-revert-stop-on-user-input nil)
   '(gnus-read-active-file nil)
   `(gnus-home-directory ,(concat root-path "/tests"))
   '(gnus-use-dribble-file nil)
   '(gnus-read-newsrc-file nil)
   '(gnus-save-killed-list nil)
   '(gnus-save-newsrc-file nil)
   '(gnus-secondary-select-methods (quote ((nnreddit ""))))
   '(gnus-select-method (quote (nnnil)))
   '(gnus-verbose 8)
   '(gnus-interactive-exit (quote quiet))))

(require 'test)

(defun cleanup ()
  (let* ((newsrc-file gnus-current-startup-file)
         (quick-file (concat newsrc-file ".eld")))
    (when (file-exists-p quick-file)
      (message "Deleting %s" quick-file)
      (delete-file quick-file))))

(Setup
 )

(After
 )

(Teardown
 (cleanup)
)

(Fail
 (unless noninteractive
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
