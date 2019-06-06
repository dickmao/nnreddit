;; The following is a derivative work of
;; https://github.com/millejoh/emacs-ipython-notebook
;; licensed under GNU General Public License v3.0.

(require 'nnreddit)
(require 'cl-lib)
(require 'ert)

(defun test-wait-for (predicate &optional predargs ms interval continue)
  "Wait until PREDICATE function returns non-`nil'.
  PREDARGS is argument list for the PREDICATE function.
  MS is milliseconds to wait.  INTERVAL is polling interval in milliseconds."
  (let* ((int (nnreddit-aif interval it (nnreddit-aif ms (max 300 (/ ms 10)) 300)))
         (count (max 1 (if ms (truncate (/ ms int)) 25))))
    (unless (or (cl-loop repeat count
                         when (apply predicate predargs)
                         return t
                         do (sleep-for 0 int))
                continue)
      (error "Timeout: %s" predicate))))

(provide 'test)
