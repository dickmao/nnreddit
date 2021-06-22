(require 'ert)
(require 'cl-lib)
(require 'espuds)
(require 'f)

(let* ((support-path (f-dirname load-file-name))
       (root-path (f-parent (f-parent support-path))))
  (add-to-list 'load-path (concat root-path "/lisp"))
  (add-to-list 'load-path (concat root-path "/tests")))

(require 'nnreddit-test)

(defvar nnreddit--current-feature)
(add-hook 'ecukes-reporter-before-feature-hook
          (lambda (feature)
            (-when-let* ((intro (ecukes-feature-intro feature))
                         (header (ecukes-intro-header intro)))
              (setq nnreddit--current-feature header))))

(defmacro if-demote (demote &rest forms)
  (declare (debug t) (indent 1))
  `(if ,demote
       (with-demoted-errors "demoted: %s"
         ,@forms)
     ,@forms))

(defun cleanup ()
  (let* ((newsrc-file (if (bound-and-true-p gnus-current-startup-file)
			  gnus-current-startup-file
			gnus-dot-newsrc))
         (quick-file (concat newsrc-file ".eld")))
    (when (file-exists-p quick-file)
      (message "Deleting %s" quick-file)
      (delete-file quick-file))))

(Setup
 )

(After
 (setq nnreddit--whoami nil))

(Teardown
 (cleanup)
)

(Fail
 (if noninteractive
     (with-demoted-errors "demote: %s"
       (Then "end recordings")
       (Teardown))
   (backtrace)
   (keyboard-quit))) ;; useful to prevent emacs from quitting
