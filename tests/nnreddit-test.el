;;; nnreddit-test.el --- Test utilities for nnreddit  -*- lexical-binding: t; coding: utf-8 -*-

;; The following is a derivative work of
;; https://github.com/millejoh/emacs-ipython-notebook
;; licensed under GNU General Public License v3.0.

(custom-set-variables
 '(gnus-before-startup-hook (quote (toggle-debug-on-error)))
 '(nnreddit-venv nil)
 `(nnreddit-python-command ,(or (getenv "PYTHON") "python"))
 '(auto-revert-verbose nil)
 '(auto-revert-stop-on-user-input nil)
 '(gnus-read-active-file nil)
 `(gnus-home-directory ,(file-name-directory load-file-name))
 '(gnus-use-dribble-file nil)
 '(gnus-read-newsrc-file nil)
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-secondary-select-methods (quote ((nnreddit ""))))
 '(gnus-select-method (quote (nnnil)))
 '(gnus-message-highlight-citation nil)
 '(gnus-verbose 8)
 '(gnus-interactive-exit (quote quiet)))

(require 'nnreddit)
(require 'cl-lib)
(require 'ert)
(require 'message)

(with-eval-after-load "python"
  (setq python-indent-guess-indent-offset-verbose nil))

(defun nnreddit-test-wait-for (predicate &optional predargs ms interval continue)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait.  INTERVAL is polling interval in milliseconds."
  (let* ((int (aif interval it (aif ms (max 300 (/ ms 10)) 300)))
         (count (max 1 (if ms (truncate (/ ms int)) 25))))
    (unless (or (cl-loop repeat count
                         when (apply predicate predargs)
                         return t
                         do (sleep-for 0 int))
                continue)
      (error "Timeout: %s" predicate))))

(mapc (lambda (sf)
        (add-function
         :around (symbol-function sf)
         (lambda (f &rest args)
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
                     ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
             (apply f args)))))
      '(message-cancel-news message-send-news find-file-noselect))

(provide 'nnreddit-test)
