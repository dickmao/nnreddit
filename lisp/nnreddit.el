;;; nnreddit.el --- Gnus backend for reddit  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nnreddit.el

;; Authors: dickmao <github id: dickmao>
;; Version: 0
;; Keywords: news
;; URL: https://github.com/dickmao/nnreddit
;; Package-Requires: ((emacs "25"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with nnreddit.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A Gnus backend for Reddit.

;;; Code:

;; Gnus            Reddit
;; ----            ------
;; list            subscribed subreddits
;; group           subreddit
;; threads         threads
;; root article    link or submission
;; articles        {root article, comments}

(require 'nnoo)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'gnus-cite)
(require 'gnus-srvr)
(require 'gnus-cache)
(require 'gnus-bcklg)
(require 'gnus-score)
(require 'python)
(require 'subr-x)
(require 'json-rpc)
(require 'mm-url)
(require 'cl-lib)
(require 'virtualenvwrapper)
(require 'anaphora)
(require 'request)
(require 'url-http)

(nnoo-declare nnreddit)

(defcustom nnreddit-render-submission t
  "If non-nil, follow link upon `gnus-summary-select-article'.

Otherwise, just display link."
  :type 'boolean
  :group 'nnreddit)

(defmacro nnreddit--gethash (string hashtable)
  "Get corresponding value of STRING from HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-gethash-safe)
         'gnus-gethash-safe
       'gethash)
    ,string ,hashtable))

(defmacro nnreddit--sethash (string value hashtable)
  "Set corresponding value of STRING to VALUE in HASHTABLE.

Starting in emacs-src commit c1b63af, Gnus moved from obarrays to normal hashtables."
  `(,(if (fboundp 'gnus-sethash)
         'gnus-sethash
       'puthash)
    ,string ,value ,hashtable))

(defcustom nnreddit-python-command (if (equal system-type 'windows-nt)
                                       (or (executable-find "py")
                                           (executable-find "pythonw")
                                           "python")
                                     "python")
  "Python executable name."
  :type (append '(choice)
                (let (result)
                  (dolist (py '("python" "python2" "python3" "pythonw" "py")
                              result)
                    (setq result (append result `((const :tag ,py ,py))))))
                '((string :tag "Other")))
  :group 'nnreddit)

(defcustom nnreddit-venv
  (let* ((library-directory (file-name-directory (locate-library "nnreddit")))
         (defacto-version (file-name-nondirectory
                           (directory-file-name library-directory)))
         (venv-id (concat defacto-version "-" nnreddit-python-command))
         (result (concat venv-location venv-id))
         (requirements (concat library-directory "requirements.txt"))
         (install-args (if (file-exists-p requirements)
                           (list "-r" requirements)
                         (list "virtualenv")))
         (already-in-venv
          (not (zerop (apply #'call-process nnreddit-python-command
                             nil nil nil
                             (list
                              "-c"
                              "import sys; sys.exit(hasattr(sys, 'real_prefix'))")))))
         (pip-args (append (list "-m" "pip" "install")
                           (unless already-in-venv (list "--user"))
                           install-args))
         (pip-status
          (apply #'call-process nnreddit-python-command nil nil nil
                 pip-args)))
    (gnus-message 7 "nnreddit-venv: %s %s" nnreddit-python-command
                  (mapconcat 'identity pip-args " "))
    (cond ((numberp pip-status)
           (unless (zerop pip-status)
             (gnus-message 3 "nnreddit-venv: pip install exit %s" pip-status)))
          (t (gnus-message 3 "nnreddit-venv: pip install signal %s" pip-status)))
    (gnus-message 7 "nnreddit-venv: %s" result)
    (unless (file-exists-p venv-location)
      (make-directory venv-location))
    (cond ((member venv-id (split-string (venv-list-virtualenvs))) result)
          (t (gnus-message 5 "nnreddit-venv: installing venv to %s..." result)
             (condition-case err
                 (progn
                   (venv-mkvirtualenv-using nnreddit-python-command venv-id)
                   (venv-with-virtualenv-shell-command
                    venv-id
                    ;; `python` and not `nnreddit-python-command` because
                    ;; venv normalizes the executable to `python`.
                    (format "cd %s && python setup.py install" library-directory))
                   (gnus-message 5 "nnreddit-venv: installing venv to %s...done" result)
                   result)
               (error (when (venv-is-valid venv-id)
                        (condition-case rmerr
                            (venv-rmvirtualenv venv-id)
                          (error (gnus-message 3 (format "venv-rmvirtualenv: %s"
                                                         (error-message-string rmerr))))))
                      (gnus-message 3 (format "nnreddit-venv: %s"
                                              (error-message-string err)))
                      "/dev/null")))))
  "Full path to venv directory.

To facilitate upgrades, the name gloms a de facto version (the directory
name where this file resides) and the `nnreddit-python-command'."
  :type '(choice (string :tag "Directory" (get (quote nnreddit-env) (quote standard-value)))
                 (const :tag "Development" nil))
  :group 'nnreddit)

;; unconditional
(gnus-define-keys (nnreddit-group-mode-map "R" gnus-group-mode-map)
  "g" nnreddit-goto-group)

(defvar nnreddit-summary-voting-map
  (let ((map (make-sparse-keymap)))
    map)
  "Voting map.")

(defvar nnreddit-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'gnus-summary-followup)
    (define-prefix-command 'nnreddit-summary-voting-map)
    (define-key map "R" 'nnreddit-summary-voting-map)
    (define-key nnreddit-summary-voting-map "0" 'nnreddit-novote)
    (define-key nnreddit-summary-voting-map "-" 'nnreddit-downvote)
    (define-key nnreddit-summary-voting-map "=" 'nnreddit-upvote)
    (define-key nnreddit-summary-voting-map "+" 'nnreddit-upvote)
    map))

(defvar nnreddit-article-mode-map
  (copy-keymap nnreddit-summary-mode-map)) ;; how does Gnus do this?

(defcustom nnreddit-log-rpc nil
  "Turn on PRAW logging."
  :type 'boolean
  :group 'nnreddit)

(defcustom nnreddit-rpc-request-timeout 60
  "Turn on PRAW logging."
  :type 'integer
  :group 'nnreddit)

(defcustom nnreddit-localhost "127.0.0.1"
  "Some users keep their browser in a separate domain.

Do not set this to \"localhost\" as a numeric IP is required for the oauth handshake."
  :type 'string
  :group 'nnreddit)

(defvar nnreddit-rpc-log-filename nil)

(defvar nnreddit--python-module-extra-args nil "Primarily for testing.")

(define-minor-mode nnreddit-article-mode
  "Minor mode for nnreddit articles.  Disallow `gnus-article-reply-with-original'.

\\{gnus-article-mode-map}
"
  :lighter " Reddit"
  :keymap nnreddit-article-mode-map)

(define-minor-mode nnreddit-summary-mode
  "Disallow \"reply\" commands in `gnus-summary-mode-map'.

\\{nnreddit-summary-mode-map}
"
  :lighter " Reddit"
  :keymap nnreddit-summary-mode-map)

(define-minor-mode nnreddit-group-mode
  "Add `R-g' go-to-subreddit binding to *Group*.

\\{gnus-group-mode-map}
"
  :keymap gnus-group-mode-map)

(cl-defun nnreddit-novote (&key deprecated)
  "Retract vote."
  (interactive)
  (when deprecated
    (message "Deprecated.  Use V-V-0 instead."))
  (nnreddit-vote-current-article 0))

(cl-defun nnreddit-downvote (&key deprecated)
  "Downvote the article in current buffer."
  (interactive)
  (when deprecated
    (message "Deprecated.  Use V-V-- instead."))
  (nnreddit-vote-current-article -1))

(cl-defun nnreddit-upvote (&key deprecated)
  "Upvote the article in current buffer."
  (interactive)
  (when deprecated
    (message "Deprecated.  Use V-V-= or V-V-+ instead."))
  (nnreddit-vote-current-article 1))

(defvar nnreddit--seq-map-indexed
  (if (fboundp 'seq-map-indexed)
      #'seq-map-indexed
    (lambda (function sequence)
      (let ((index 0))
        (seq-map (lambda (elt)
                   (prog1
                       (funcall function elt index)
                     (setq index (1+ index))))
                 sequence)))))

(defmacro nnreddit--normalize-server ()
  "Disallow \"server\" from being empty string, which is unsettling.
Normalize it to \"nnreddit-default\"."
  `(let ((canonical "nnreddit-default"))
    (when (equal server "")
      (setq server nil))
    (unless server
      (setq server canonical))
    (unless (string= server canonical)
      (error "nnreddit--normalize-server: multiple servers unsupported!"))))

(defvar nnreddit-headers-hashtb (gnus-make-hashtable)
  "Group (subreddit) string -> interleaved submissions and comments sorted by created time.")

(defvar nnreddit-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom (global over all entries).")

(defvar nnreddit-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries).")

(defsubst nnreddit-get-headers (group)
  "List headers from GROUP."
  (nnreddit--gethash group nnreddit-headers-hashtb))

(defun nnreddit-find-header (group id)
  "O(n) search of GROUP headers for ID."
  (-when-let* ((headers (nnreddit-get-headers group))
               (found (seq-position headers id
                                    (lambda (plst id)
                                      (equal id (plist-get plst :id))))))
    (nnreddit--get-header (1+ found) group)))

(defsubst nnreddit-refs-for (name &optional depth)
  "Get message ancestry for NAME up to DEPTH."
  (unless depth
    (setq depth most-positive-fixnum))
  (when (> depth 0)
    (nreverse (cl-loop with parent-id = (nnreddit--gethash name nnreddit-refs-hashtb)
                       for level = 0 then level
                       for name = parent-id then
                       (nnreddit--gethash name nnreddit-refs-hashtb)
                       until (null name)
                       collect name
                       until (>= (cl-incf level) depth)))))

(defsubst nnreddit-sort-append-headers (group &rest lvp)
  "Append to hashed headers of GROUP the LVP (list of vector of plists)."
  (nnreddit--sethash group (nconc (nnreddit-get-headers group)
                              (apply #'nnreddit--sort-headers lvp))
                nnreddit-headers-hashtb))

(defvar nnreddit-directory (nnheader-concat gnus-directory "reddit")
  "Where to retrieve last read state.")

(defvar nnreddit-processes nil
  "Garbage collect PRAW processes.")

(nnoo-define-basics nnreddit)

(defsubst nnreddit-rpc-call (server generator_kwargs method &rest args)
  "Make jsonrpc call to SERVER with GENERATOR_KWARGS using METHOD ARGS.

Process stays the same, but the jsonrpc connection (a cheap struct) gets reinstantiated with every call."
  (nnreddit--normalize-server)
  (-when-let* ((proc (nnreddit-rpc-get server))
               (connection (json-rpc--create :process proc
                                             :host nnreddit-localhost
                                             :id-counter 0)))
    (apply #'nnreddit-rpc-request connection generator_kwargs method args)))

(defun nnreddit-goto-group (realname)
  "Jump to the REALNAME subreddit."
  (interactive (list (read-no-blanks-input "Subreddit: r/")))
  (let* ((canonical (nnreddit-rpc-call nil nil "canonical_spelling" realname))
         (group (gnus-group-full-name canonical "nnreddit")))
    (gnus-activate-group group t)
    (gnus-group-read-group t t group)))

(defun nnreddit-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-newsgroup-name
    (error "No current newgroup"))
  (if-let ((article-number (or (cdr gnus-article-current)
                               (gnus-summary-article-number))))
      (let* ((header (nnreddit--get-header article-number
                                           (gnus-group-real-name gnus-newsgroup-name)))
             (orig-score (format "%s" (plist-get header :score)))
             (new-score (if (zerop vote) orig-score
                          (concat orig-score " "
                                  (if (> vote 0) "+" "")
                                  (format "%s" vote))))
             (article-name (plist-get header :name)))
        (save-excursion
          (save-window-excursion
            (with-current-buffer gnus-summary-buffer
              (if (eq (gnus-summary-article-number) (cdr gnus-article-current))
                  (progn (with-current-buffer gnus-article-buffer
                           (let ((inhibit-read-only t))
                             (nnheader-replace-header "Score" new-score)))
                         (nnreddit-rpc-call nil nil "vote" article-name vote))
                (message "Open the article before voting."))))))
    (error "No current article")))

(defsubst nnreddit--gate (&optional group)
  "Apply our minor modes only when the following conditions hold for GROUP."
  (unless group
    (setq group gnus-newsgroup-name))
  (and (stringp group)
       (listp (gnus-group-method group))
       (eq 'nnreddit (car (gnus-group-method group)))))

(defun nnreddit-update-subscription (group level oldlevel &optional _previous)
  "Nnreddit `gnus-group-change-level' callback of GROUP to LEVEL from OLDLEVEL."
  (when (nnreddit--gate group)
    (let ((old-subbed-p (<= oldlevel gnus-level-default-subscribed))
          (new-subbed-p (<= level gnus-level-default-subscribed)))
      (unless (eq old-subbed-p new-subbed-p)
        ;; afaict, praw post() doesn't return status
        (if new-subbed-p
            (nnreddit-rpc-call nil nil "subscribe" (gnus-group-real-name group))
          (nnreddit-rpc-call nil nil "unsubscribe" (gnus-group-real-name group)))))))

(defun nnreddit-rpc-kill (&optional server)
  "Kill the jsonrpc process named SERVER."
  (interactive (list nil))
  (nnreddit--normalize-server)
  (let (new-processes)
    (mapc (lambda (proc) (if (and server (not (string= server (process-name proc))))
                             (push proc new-processes)
                           (delete-process proc)))
          nnreddit-processes)
    (setq nnreddit-processes new-processes)))

(deffoo nnreddit-request-close ()
  (nnreddit-close-server)
  t)

(deffoo nnreddit-request-type (_group &optional _article)
  'news)

(deffoo nnreddit-server-opened (&optional server)
  (nnreddit--normalize-server)
  (cl-remove-if-not (lambda (proc) (string= server (process-name proc)))
                 nnreddit-processes))

(deffoo nnreddit-status-message (&optional server)
  (nnreddit--normalize-server)
  "")

(deffoo nnreddit-open-server (_server &optional _defs)
  t)

(deffoo nnreddit-close-group (_group &optional server)
  (nnreddit--normalize-server)
  t)

(defmacro nnreddit--with-group (group &rest body)
  "Disambiguate GROUP if it's empty and execute BODY."
  (declare (debug (form &rest form))
           (indent 1))
  `(let* ((group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (gnus-group-prefixed-name group "nnreddit:")))
     ,@body))

(defun nnreddit--get-header (article-number &optional group)
  "Get header indexed ARTICLE-NUMBER for GROUP."
  (nnreddit--with-group group
    (let ((headers (nnreddit-get-headers group)))
      (elt headers (1- article-number)))))

(defun nnreddit--get-body (name &optional group server)
  "Get full text of submission or comment NAME for GROUP at SERVER."
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (nnreddit-rpc-call server nil "body" group name)))

(defsubst nnreddit-hack-name-to-id (name)
  "Get x from t1_x (NAME)."
  (cl-subseq name 3))

(defsubst nnreddit--br-tagify (body)
  "Reddit-html BODY shies away from <BR>.  Should it?"
  (replace-regexp-in-string "\n" "<br>" body))

(defsubst nnreddit--citation-wrap (author body)
  "Cite AUTHOR using `gnus-message-cite-prefix-regexp' before displaying BODY.

Originally written by Paul Issartel."
  (with-temp-buffer
    (insert body)
    (mm-url-remove-markup)
    (mm-url-decode-entities)
    (fill-region (point-min) (point-max))
    (let* ((trimmed-1 (replace-regexp-in-string "\\(\\s-\\|\n\\)+$" "" (buffer-string)))
           (trimmed (replace-regexp-in-string "^\\(\\s-\\|\n\\)+" "" trimmed-1)))
      (concat author " wrote:<br>\n"
              "<pre>\n"
              (cl-subseq (replace-regexp-in-string "\n" "\n> " (concat "\n" trimmed)) 1)
              "\n</pre>\n\n"))))

(defun nnreddit-add-entry (hashtb e field)
  "Add to HASHTB the pair consisting of entry E's name to its FIELD."
  (nnreddit--sethash (plist-get e :name) (plist-get e field) hashtb))

(defun nnreddit--filter-after (after-this vop)
  "Get elements created AFTER-THIS in VOP (vector of plists)."
  (cl-loop for elt-idx in (funcall nnreddit--seq-map-indexed
                                   (lambda (elt idx) (cons elt idx)) vop)
           until (>= (plist-get (car elt-idx) :created_utc) after-this)
           finally return (seq-drop vop (or (cdr elt-idx) 0))))

(defsubst nnreddit--base10 (base36)
  "Convert BASE36 reddit name encoding to a base10 integer."
  (apply #'+ (funcall nnreddit--seq-map-indexed
                      (lambda (elt idx)
                        (* (expt 36 idx)
                           (if (>= elt ?a) (+ 10 (- elt ?a)) (- elt ?0))))
                      (reverse base36))))

(deffoo nnreddit-request-group-scan (group &optional server _info)
  "M-g from *Group* calls this.

Set flag for the ensuing `nnreddit-request-group' to avoid going out to PRAW yet again."
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (gnus-message 5 "nnreddit-request-group-scan: scanning %s..." group)
    (gnus-activate-group (gnus-group-full-name group '("nnreddit" (or server ""))) t)
    (gnus-message 5 "nnreddit-request-group-scan: scanning %s...done" group)
    t))

;; gnus-group-select-group
;;   gnus-group-read-group
;;     gnus-summary-read-group
;;       gnus-summary-read-group-1
;;         gnus-summary-setup-buffer
;;           sets gnus-newsgroup-name
;;         gnus-select-newsgroup
;;           gnus-request-group
;;             nnreddit-request-group
(deffoo nnreddit-request-group (group &optional server _fast info)
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (let* ((info
            (or info
                (gnus-get-info gnus-newsgroup-name)
                (list gnus-newsgroup-name
                      gnus-level-default-subscribed
                      nil nil
                      (gnus-method-simplify (gnus-group-method gnus-newsgroup-name)))))
           (params (gnus-info-params info))
           (newsrc-read-ranges (gnus-info-read info))
           (newsrc-seen-cons (gnus-group-parameter-value params 'last-seen t))
           (newsrc-seen-index (car newsrc-seen-cons))
           (newsrc-seen-id (cdr newsrc-seen-cons)))
      (let* ((headers (nnreddit-get-headers group))
             (num-headers (length headers))
             (status (format "211 %d %d %d %s" num-headers 1 num-headers group)))
        (gnus-message 7 "nnreddit-request-group: %s" status)
        (nnheader-insert "%s\n" status)

        ;; remind myself how this works:
        ;; old-praw (1 - 20=emkdjrx)
        ;; read-ranges (1 - 10)                   (15 - 20)
        ;; unread-ranges       (11, 12, 13, 14)
        ;; new-praw    (12 13 14 15 16 17 18 19 20 - 100)
        ;; 20=emkdjrx in old-praw is 9=emkdjrx in new-praw.  index shift is 20-9=+11
        ;; new-unread-ranges   (0,  1,   2,  3)
        ;; new-read-ranges                        (4 - 9)
        (when (gnus-group-entry gnus-newsgroup-name)
          ;; seen-indices are one-indexed !
          (let* ((newsrc-seen-index-now
                  (if (or (not (stringp newsrc-seen-id))
                          (zerop (nnreddit--base10 newsrc-seen-id)))
                      1
                    (cl-loop with cand
                             for plst in headers
                             for i = 1 then (1+ i)
                             if (= (nnreddit--base10 (plist-get plst :id))
                                   (nnreddit--base10 newsrc-seen-id))
                             do (gnus-message 7 "nnreddit-request-group: exact=%s" i)
                             and return i ;; do not go to finally
                             end
                             if (and (null cand)
                                     (> (nnreddit--base10 (plist-get plst :id))
                                        (nnreddit--base10 newsrc-seen-id)))
                             do (gnus-message 7 "nnreddit-request-group: cand=%s" (setq cand i))
                             end
                             finally return (or cand 0))))
                 (updated-seen-index (- num-headers
                                        (aif
                                            (seq-position (reverse headers) nil
                                                          (lambda (plst _e)
                                                            (not (plist-get plst :title))))
                                            it -1)))
                 (updated-seen-id (aif (nth (1- updated-seen-index) headers)
                                      (plist-get it :id) nil))
                 (delta (if newsrc-seen-index
                            (max 0 (- newsrc-seen-index newsrc-seen-index-now))
                          0))
                 (newsrc-read-ranges-shifted
                  (cl-remove-if-not (lambda (e)
                                      (cond ((numberp e) (> e 0))
                                            (t (> (cdr e) 0))))
                                    (mapcar (lambda (e)
                                              (cond ((numberp e) (- e delta))
                                                    (t `(,(max 1 (- (car e) delta)) .
                                                         ,(- (cdr e) delta)))))
                                            newsrc-read-ranges))))
            (gnus-message 7 "nnreddit-request-group: seen-id=%s          seen-index=%s -> %s"
                          newsrc-seen-id newsrc-seen-index newsrc-seen-index-now)
            (gnus-message 7 "nnreddit-request-group: seen-id-to-be=%s seen-index-to-be=%s delta=%d"
                          updated-seen-id updated-seen-index delta)
            (gnus-message 7 "nnreddit-request-group: read-ranges=%s shifted-read-ranges=%s"
                          newsrc-read-ranges newsrc-read-ranges-shifted)
            (gnus-info-set-read info newsrc-read-ranges-shifted)
            (gnus-info-set-marks
             info
             (append (assq-delete-all 'seen (gnus-info-marks info))
                     (list `(seen (1 . ,num-headers)))))
            (when updated-seen-id
              (while (assq 'last-seen params)
                (gnus-alist-pull 'last-seen params))
              (gnus-info-set-params
               info
               (cons `(last-seen ,updated-seen-index . ,updated-seen-id) params)
               t))
            (gnus-set-info gnus-newsgroup-name info)
            (gnus-message 7 "nnreddit-request-group: new info=%s" info)))))
    t))

(deffoo nnreddit-request-scan (&optional group server)
  (nnreddit--normalize-server)
  (unless (null group)
    (nnreddit--with-group group
      (let* ((comments (nnreddit-rpc-call server nil "comments" group))
             (raw-submissions (nnreddit-rpc-call server nil "submissions" group))
             (submissions (if (zerop (length comments))
                              raw-submissions
                            (nnreddit--filter-after
                             (- (plist-get (aref comments 0) :created_utc) 7200)
                             raw-submissions))))
        (seq-doseq (e comments)
          (nnreddit-add-entry nnreddit-refs-hashtb e :parent_id)) ;; :parent_id is fullname
        (seq-doseq (e (vconcat submissions comments))
          (nnreddit-add-entry nnreddit-authors-hashtb e :author))
        (gnus-message 5 "nnreddit-request-scan: %s: +%s comments +%s submissions"
                      group (length comments) (length submissions))
        (nnreddit-sort-append-headers group submissions comments)))))

(defsubst nnreddit--make-message-id (fullname)
  "Construct a valid Gnus message id from FULLNAME."
  (format "<%s@reddit.com>" fullname))

(defsubst nnreddit--make-references (fullname)
  "Construct a space delimited string of message ancestors of FULLNAME."
  (mapconcat (lambda (ref) (nnreddit--make-message-id ref))
             (nnreddit-refs-for fullname) " "))

(defsubst nnreddit--make-header (article-number &optional group)
  "Construct full headers of articled indexed ARTICLE-NUMBER in GROUP."
  (let* ((header (nnreddit--get-header article-number group))
         (score (plist-get header :score))
         (num-comments (plist-get header :num_comments)))
    (make-full-mail-header
     article-number
     (or (plist-get header :title)
         (concat "Re: " (plist-get header :link_title)))
     (plist-get header :author)
     (format-time-string "%a, %d %h %Y %T %z (%Z)" (plist-get header :created_utc))
     (nnreddit--make-message-id (plist-get header :name))
     (nnreddit--make-references (plist-get header :name))
     0 0 nil
     (append `((X-Reddit-Name . ,(plist-get header :name)))
             `((X-Reddit-ID . ,(plist-get header :id)))
             (aif (plist-get header :permalink)
                           `((X-Reddit-Permalink . ,it)))
             (and (integerp score)
                  `((X-Reddit-Score . ,(number-to-string score))))
             (and (integerp num-comments)
                  `((X-Reddit-Comments . ,(number-to-string num-comments))))))))

(cl-defun nnreddit--request-error (caller
                                   &key response symbol-status error-thrown
                                   &allow-other-keys
                                   &aux (response-status
                                         (request-response-status-code response)))
  "Refer to CALLER when reporting a submit error."
  (gnus-message 3 "%s %s: http status %s, %s" caller symbol-status response-status
                (error-message-string error-thrown)))

(cl-defun nnreddit--request (caller
                             url
                             &rest attributes &key parser (backend 'url-retrieve)
                             &allow-other-keys)
  "Prefix errors with CALLER when executing synchronous request to URL."
  (unless parser
    (setq attributes (nconc attributes (list :parser #'buffer-string))))
  (setq attributes (cl-loop for (k v) on attributes by (function cddr)
                            unless (eq k :backend)
                            collect k and collect v))
  (let ((request-backend backend))
    (apply #'request url
           :sync t
           :error (apply-partially #'nnreddit--request-error caller)
           attributes)))

(cl-defun nnreddit--content-handler
    (&rest args &key data response &allow-other-keys
     &aux (header (request-response--raw-header response)))
  "For HEADER that is image, wrap DATA in data URI."
  (let* ((_ (string-match "Content-Type:\\s-*\\([[:graph:]]+\\)" header))
         (content-type (match-string 1 header)))
    (cl-destructuring-bind (type _subtype) (split-string content-type "/")
      (cond ((string= type "image")
             (format "<img src=\"data:%s;base64,%s\" />"
                     content-type
                     (base64-encode-string (encode-coding-string data 'binary) t)))
            ((string= type "text") data)
            (t (error "nnreddit--content-handler: passing on %s" content-type))))))

(deffoo nnreddit-request-article (article-number &optional group server buffer)
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (with-current-buffer (or buffer nntp-server-buffer)
      (erase-buffer)
      (let* ((header (nnreddit--get-header article-number group))
             (mail-header (nnreddit--make-header article-number))
             (score (cdr (assq 'X-Reddit-Score (mail-header-extra mail-header))))
             (permalink (cdr (assq 'X-Reddit-Permalink (mail-header-extra mail-header))))
             (body (nnreddit--get-body (plist-get header :name) group server)))
        (when body
          (insert
           "Newsgroups: " group "\n"
           "Subject: " (mail-header-subject mail-header)  "\n"
           "From: " (or (mail-header-from mail-header) "nobody") "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           "Content-Type: text/html; charset=utf-8" "\n"
           (if permalink
               (format "Archived-at: <https://www.reddit.com%s>\n" permalink)
             "")
           "Score: " score "\n"
           "\n")
          (-when-let*
              ((parent-name (plist-get header :parent_id)) ;; parent_id is full
               (parent-author (or (nnreddit--gethash parent-name nnreddit-authors-hashtb)
                                  "Someone"))
               (parent-body (nnreddit--get-body parent-name group server)))
            (insert (nnreddit--citation-wrap parent-author parent-body)))
          (aif (and nnreddit-render-submission
                    (eq (plist-get header :is_self) :json-false)
                    (plist-get header :url))
              (condition-case err
                  (nnreddit--request
                   "nnreddit-request-article" it
                   :success (lambda (&rest args)
                              (insert (apply #'nnreddit--content-handler args))))
                (error (gnus-message 5 "nnreddit-request-article: %s %s"
                                     it (error-message-string err))
                       (insert (nnreddit--br-tagify body))))
            (insert (nnreddit--br-tagify body)))
          (cons group article-number))))))

(deffoo nnreddit-retrieve-headers (article-numbers &optional group server _fetch-old)
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (i article-numbers)
        (nnheader-insert-nov (nnreddit--make-header i group)))
      'nov)))

(defsubst nnreddit--earliest-among (indices lvp)
  "Return (list-to-iterate . next-earliest) from INDICES (thus-far iterators)
and LVP (list of vectors of plists).  Used in the interleaving of submissions and comments."
  (let (earliest next-earliest)
    (dolist (plst-idx
             (cl-remove-if-not #'car
                            (funcall nnreddit--seq-map-indexed
                                     (lambda (plst idx) (cons plst idx))
                                     (seq-mapn
                                      (lambda (v i)
                                        (if (< i (length v)) (aref v i)))
                                      lvp indices)))
             (list (cdr earliest)
                   (aif next-earliest
                       (plist-get (car it) :created_utc))))
      (cond ((null earliest)
             (setq earliest plst-idx))
            ((< (plist-get (car plst-idx) :created_utc)
                (plist-get (car earliest) :created_utc))
             (setq next-earliest earliest)
             (setq earliest plst-idx))
            ((null next-earliest)
             (setq next-earliest plst-idx))))))

(defun nnreddit--sort-headers (&rest lvp)
  "Sort headers for LVP (list of vectors of plists)."
  (let* ((indices (make-list (length lvp) 0))
         result)
    (while (not (equal indices (mapcar #'length lvp)))
      (cl-destructuring-bind (to-iterate bogey-created)
          (nnreddit--earliest-among indices lvp)
        (cl-loop with arr = (elt lvp to-iterate)
                 for j in (number-sequence (elt indices to-iterate) (1- (length arr)))
                 for plst = (aref arr j)
                 for created = (plist-get plst :created_utc)
                 until (> created (or bogey-created most-positive-fixnum))
                 do (cl-incf (elt indices to-iterate))
                 do (push plst result))))
    (nreverse result)))

(deffoo nnreddit-close-server (&optional server)
  (nnreddit--normalize-server)
  (condition-case err
      (progn (nnreddit-rpc-kill server) t)
    (error
     (gnus-message 2 "nnreddit-close-server: %s" (error-message-string err))
     nil)))

(deffoo nnreddit-request-list (&optional server)
  (nnreddit--normalize-server)
  (with-current-buffer nntp-server-buffer
    (let ((groups (nnreddit-rpc-call server nil "user_subreddits"))
          (newsrc (cl-mapcan (lambda (info)
                               (when (and (equal "nnreddit:" (gnus-info-method info))
                                          (<= (gnus-info-level info)
                                              gnus-level-default-subscribed))
                                 (list (gnus-info-group info))))
                             gnus-newsrc-alist)))
      (mapc (lambda (realname)
              (let ((group (gnus-group-full-name realname '("nnreddit" (or server "")))))
                (erase-buffer)
                (gnus-message 5 "nnreddit-request-list: scanning %s..." realname)
                (gnus-activate-group group t)
                (gnus-message 5 "nnreddit-request-list: scanning %s...done" realname)
                (gnus-group-unsubscribe-group group gnus-level-default-subscribed t)
                (setq newsrc (cl-remove group newsrc :test #'string=))))
            groups)
      (mapc (lambda (fullname)
              (gnus-message 4 "nnreddit-request-list: missing subscription %s" fullname)
              (nnreddit-rpc-call nil nil "subscribe" (gnus-group-real-name fullname))
              (gnus-activate-group fullname t))
            newsrc)
      (erase-buffer)
      (mapc (lambda (group)
              (insert (format "%s %d 1 y\n" group
                              (length (nnreddit-get-headers group)))))
            groups)))
  t)

(defun nnreddit-sentinel (process event)
  "Wipe headers state when PROCESS dies from EVENT."
  (unless (string= "open" (substring event 0 4))
    (gnus-message 2 "nnreddit-sentinel: process %s %s"
                  (car (process-command process))
                  (replace-regexp-in-string "\n$" "" event))
    (setq nnreddit-headers-hashtb (gnus-make-hashtable))
    (gnus-backlog-shutdown)))

(defun nnreddit--message-user (server beg end _prev-len)
  "Message SERVER related alert with `buffer-substring' from BEG to END."
  (let ((string (buffer-substring beg end))
        (magic "::user::"))
    (when (string-prefix-p magic string)
      (message "%s: %s." server (substring string (length magic))))))

(defsubst nnreddit--install-failed ()
  "If we can't install the virtualenv then all bets are off."
  (string= nnreddit-venv "/dev/null"))

(defun nnreddit-rpc-get (&optional server)
  "Retrieve the PRAW process for SERVER."
  (nnreddit--normalize-server)
  (unless (nnreddit--install-failed)
    (let ((proc (get-buffer-process (get-buffer-create (format " *%s*" server)))))
      (unless proc
        (let* ((nnreddit-el-dir (directory-file-name (file-name-directory (locate-library "nnreddit"))))
               (nnreddit-py-dir (directory-file-name
                                 (if (string= "lisp" (file-name-base nnreddit-el-dir))
                                     (file-name-directory nnreddit-el-dir)
                                   nnreddit-el-dir)))
               (python-shell-extra-pythonpaths (list nnreddit-py-dir))
               (process-environment (python-shell-calculate-process-environment))
               (python-executable (if nnreddit-venv
                                      (format "%s/bin/python" nnreddit-venv)
                                    (executable-find nnreddit-python-command)))
               (python-module (if (featurep 'nnreddit-test) "tests" "nnreddit"))
               (praw-command (append (list python-executable "-m" python-module)
                                     nnreddit--python-module-extra-args)))
          (unless (featurep 'nnreddit-test)
            (setq praw-command (append praw-command (list "--localhost" nnreddit-localhost)))
            (when nnreddit-log-rpc
              (setq nnreddit-rpc-log-filename
                    (concat (file-name-as-directory temporary-file-directory)
                            "nnreddit-rpc-log."))
              (setq praw-command (append praw-command
                                         (list "--log" nnreddit-rpc-log-filename)))))
          (setq proc (make-process :name server
                                   :buffer (get-buffer-create (format " *%s*" server))
                                   :command praw-command
                                   :connection-type 'pipe
                                   :noquery t
                                   :sentinel #'nnreddit-sentinel
                                   :stderr (get-buffer-create (format " *%s-stderr*" server))))
          (with-current-buffer (get-buffer-create (format " *%s-stderr*" server))
            (add-hook 'after-change-functions
                      (apply-partially 'nnreddit--message-user server)
                      nil t)))
        (push proc nnreddit-processes))
      proc)))

(defun nnreddit-rpc-request (connection kwargs method &rest args)
  "Send to CONNECTION a request with generator KWARGS calling METHOD ARGS.

Library `json-rpc--request' assumes HTTP transport which jsonrpyc does not, so we make our own."
  (unless (hash-table-p kwargs)
    (setq kwargs #s(hash-table)))
  (let* ((id (cl-incf (json-rpc-id-counter connection)))
         (request `(:method ,method
                    :id ,id
                    :params (:args ,(apply json-array-type args) :kwargs ,kwargs)))
         (proc (json-rpc-process (json-rpc-ensure connection)))
         (encoded (json-encode (append '(:jsonrpc "2.0") request)))
         (json-object-type 'plist)
         (json-key-type 'keyword)
         (iteration-seconds 6))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (gnus-message 7 "nnreddit-rpc-request: send %s" encoded)
      (process-send-string proc (concat encoded "\n"))
      (cl-loop repeat (/ nnreddit-rpc-request-timeout iteration-seconds)
               with result
               until (or (not (json-rpc-live-p connection))
                         (and (not (zerop (length (buffer-string))))
                              (condition-case err
                                  (setq result (json-read-from-string (buffer-string)))
                                (error
                                 (let* ((resp (if (< (length (buffer-string)) 100)
                                                  (buffer-string)
                                                (format "%s...%s"
                                                        (cl-subseq (buffer-string) 0 50)
                                                        (cl-subseq (buffer-string) -50)))))
                                   (setq result
                                         `(:error ,(format "%s on %s"
                                                           (error-message-string err)
                                                           resp))))
                                 nil))))
               do (accept-process-output proc iteration-seconds 0)
               finally return
               (cond ((null result)
                      (error "nnreddit-rpc-request: response timed out"))
                     ((plist-get result :error)
                      (error "nnreddit-rpc-request: %s" (plist-get result :error)))
                     (t
                      (gnus-message 7 "nnreddit-rpc-request: recv ...%s"
                                    (cl-subseq (buffer-string)
                                               (- (min (length (buffer-string)) 50))))
                      (plist-get result :result)))))))

(defsubst nnreddit--extract-name (from)
  "String match on something looking like t1_es076hd in FROM."
  (and (stringp from) (string-match "\\(t[0-9]+_[a-z0-9]+\\)" from) (match-string 1 from)))

;; C-c C-c from followup buffer
;; message-send-and-exit
;; message-send
;; message-send-method-alist=message-send-news-function=message-send-news
;; gnus-request-post
;; nnreddit-request-post
(deffoo nnreddit-request-post (&optional server)
  (nnreddit--normalize-server)
  (let* ((ret t)
         (kwargs (make-hash-table))
         (title (or (message-fetch-field "Subject") (error "No Subject field")))
         (link (message-fetch-field "Link"))
         (reply-p (not (null message-reply-headers)))
         (edit-name (nnreddit--extract-name (message-fetch-field "Supersedes")))
         (cancel-name (nnreddit--extract-name (message-fetch-field "Control")))
         (root-p (message-fetch-field "Reply-Root"))
         (article-number (cdr gnus-article-current))
         (group (if (numberp article-number)
                    (gnus-group-real-name (car gnus-article-current))
                  (or (message-fetch-field "Newsgroups") (error "No Newsgroups field"))))
         (header (when (numberp article-number)
                   (nnreddit--get-header article-number group)))
         (body
          (save-excursion
            (save-restriction
              (message-goto-body)
              (narrow-to-region (point) (point-max))
              (buffer-string)))))
    (cond (cancel-name (nnreddit-rpc-call server nil "delete" cancel-name))
          (edit-name (nnreddit-rpc-call server nil "edit" edit-name body))
          (reply-p (nnreddit-rpc-call server nil "reply"
                                      (plist-get header :name)
                                      body (stringp root-p)))
          (link (let* ((parsed-url (url-generic-parse-url link))
                       (host (url-host parsed-url)))
                  (if (and (stringp host) (not (zerop (length host))))
                      (progn
                        (puthash 'url link kwargs)
                        (nnreddit-rpc-call server kwargs "submit" group title))
                    ;; gnus-error might be better here
                    (error "nnreddit-request-post: invalid url \"%s\"" link)
                    (setq ret nil))))
          (t (puthash 'selftext body kwargs)
             (nnreddit-rpc-call server kwargs "submit" group title)))
    ret))

(defun nnreddit--browse-root (&rest _args)
  "What happens when I click on Subject."
  (-when-let* ((article-number (cdr gnus-article-current))
               (group (gnus-group-real-name (car gnus-article-current)))
               (header (nnreddit--get-header article-number group))
               (permalink (plist-get header :permalink)))
    (cl-loop for name in (nnreddit-refs-for (plist-get header :name))
             for header1 = (nnreddit-find-header
                            group (nnreddit-hack-name-to-id name))
             for permalink1 = (plist-get header1 :permalink)
             until permalink1
             finally (browse-url (format "https://www.reddit.com%s"
                                         (or permalink1 permalink ""))))))

(defun nnreddit--header-button-alist ()
  "Construct a buffer-local `gnus-header-button-alist' for nnreddit."
  (let* ((result (copy-alist gnus-header-button-alist))
         (references-value (assoc-default "References" result
                                          (lambda (x y) (string-match-p y x))))
         (references-key (car (rassq references-value result))))
    (setq result (cl-delete "^Subject:" result :test (lambda (x y) (cl-search x (car y)))))
    (setq result (cl-delete references-key result :test (lambda (x y) (cl-search x (car y)))))
    (push (append '("^\\(Message-I[Dd]\\|^In-Reply-To\\):") references-value) result)
    (push '("^Subject:" ": *\\(.+\\)$" 1 (>= gnus-button-browse-level 0)
            nnreddit--browse-root 1)
          result)
    result))

(defsubst nnreddit--fallback-link ()
  "Cannot render submission."
  (let* ((group (gnus-group-real-name (car gnus-article-current)))
         (header (nnreddit--get-header (cdr gnus-article-current) group))
         (body (nnreddit--get-body (plist-get header :name) group)))
    (with-current-buffer gnus-original-article-buffer
      (article-goto-body)
      (delete-region (point) (point-max))
      (when body
        (insert (nnreddit--br-tagify body))))))

(defalias 'nnreddit--display-article
  (lambda (article &optional all-headers _header)
    (condition-case err
        (gnus-article-prepare article all-headers)
      (error
       (if nnreddit-render-submission
           (progn
             (gnus-message 7 "nnreddit--display-article: '%s' (falling back...)"
                           (error-message-string err))
             (nnreddit--fallback-link)
             (gnus-article-prepare article all-headers))
         (error (error-message-string err))))))
  "In case of shr failures, dump original link.")

(defsubst nnreddit--dense-time (time)
  "Convert TIME to a floating point number.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs)."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defalias 'nnreddit--format-time-elapsed
  (lambda (header)
    (condition-case nil
        (let ((date (mail-header-date header)))
          (if (> (length date) 0)
              (let*
                  ((then (nnreddit--dense-time
                          (apply 'encode-time (parse-time-string date))))
                   (now (nnreddit--dense-time (current-time)))
                   (diff (- now then))
                   (str
                    (cond
                     ((>= diff (* 86400.0 7.0 52.0))
                      (if (>= diff (* 86400.0 7.0 52.0 10.0))
                          (format "%3dY" (floor (/ diff (* 86400.0 7.0 52.0))))
                        (format "%3.1fY" (/ diff (* 86400.0 7.0 52.0)))))
                     ((>= diff (* 86400.0 30.0))
                      (if (>= diff (* 86400.0 30.0 10.0))
                          (format "%3dM" (floor (/ diff (* 86400.0 30.0))))
                        (format "%3.1fM" (/ diff (* 86400.0 30.0)))))
                     ((>= diff (* 86400.0 7.0))
                      (if (>= diff (* 86400.0 7.0 10.0))
                          (format "%3dw" (floor (/ diff (* 86400.0 7.0))))
                        (format "%3.1fw" (/ diff (* 86400.0 7.0)))))
                     ((>= diff 86400.0)
                      (if (>= diff (* 86400.0 10.0))
                          (format "%3dd" (floor (/ diff 86400.0)))
                        (format "%3.1fd" (/ diff 86400.0))))
                     ((>= diff 3600.0)
                      (if (>= diff (* 3600.0 10.0))
                          (format "%3dh" (floor (/ diff 3600.0)))
                        (format "%3.1fh" (/ diff 3600.0))))
                     ((>= diff 60.0)
                      (if (>= diff (* 60.0 10.0))
                          (format "%3dm" (floor (/ diff 60.0)))
                        (format "%3.1fm" (/ diff 60.0))))
                     (t
                      (format "%3ds" (floor diff)))))
                   (stripped
                    (replace-regexp-in-string "\\.0" "" str)))
                (concat (cond
                         ((= 2 (length stripped)) "  ")
                         ((= 3 (length stripped)) " ")
                         (t ""))
                        stripped))))
      ;; print some spaces and pretend nothing happened.
      (error "    ")))
  "Return time elapsed since HEADER was sent.

Written by John Wiegley (https://github.com/jwiegley/dot-emacs).")

;; Evade package-lint!
(fset 'gnus-user-format-function-S
      (symbol-function 'nnreddit--format-time-elapsed))

(add-to-list
 'gnus-parameters
 `("^nnreddit"
   (gnus-summary-make-false-root 'adopt)
   (gnus-cite-hide-absolute 5)
   (gnus-cite-hide-percentage 0)
   (gnus-cited-lines-visible '(2 . 2))
   (gnus-auto-extend-newsgroup nil)
   (gnus-add-timestamp-to-message t)
   (gnus-summary-line-format "%3t%U%R%uS %I%(%*%-10,10f  %s%)\n")
   (gnus-summary-display-article-function
    (quote ,(symbol-function 'nnreddit--display-article)))
   (gnus-header-button-alist
    (quote ,(nnreddit--header-button-alist)))
   (gnus-visible-headers ,(concat gnus-visible-headers "\\|^Score:"))))

(nnoo-define-skeleton nnreddit)

(defun nnreddit-article-mode-activate ()
  "Augment the `gnus-article-mode-map' conditionally."
  (when (nnreddit--gate)
    (nnreddit-article-mode)))

(defun nnreddit-summary-mode-activate ()
  "Shadow some bindings in `gnus-summary-mode-map' conditionally."
  (when (nnreddit--gate)
    (nnreddit-summary-mode)))

(defun nnreddit-group-mode-activate ()
  "Augment the `gnus-group-mode-map' unconditionally."
  (if (boundp 'gnus-group-change-level-functions)
      (add-hook 'gnus-group-change-level-functions 'nnreddit-update-subscription nil 'local)
    (custom-set-variables
     '(gnus-group-change-level-function (quote nnreddit-update-subscription))))
  (nnreddit-group-mode))

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook 'nnreddit-article-mode-activate)
(add-hook 'gnus-group-mode-hook 'nnreddit-group-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nnreddit-summary-mode-activate)

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nnreddit" post-mail) t)

;; Add prompting for replying to thread root to gnus-summary-followup.
;; The interactive spec of gnus-summary-followup is putatively preserved.
(let* ((prompt-loose
        (lambda (f &rest args)
          (cond ((nnreddit--gate)
                 (or (-when-let*
                      ((article-number (gnus-summary-article-number))
                       (header (nnreddit--get-header article-number))
                       (root-name (car (nnreddit-refs-for (plist-get header :name))))
                       (rootless (or (not (stringp root-name))
                                     (not (string-prefix-p "t3_" root-name))
                                     (not (nnreddit-find-header
                                           (gnus-group-real-name gnus-newsgroup-name)
                                           (nnreddit-hack-name-to-id root-name)))))
                       (reply-root (read-char-choice
                                    "Reply loose thread [m]essage or [r]oot: " '(?m ?r)))
                       (q-root (eq reply-root ?r)))
                      (let* ((link-header (apply-partially #'message-add-header
                                                           "Reply-Root: yes"))
                             (add-link-header (apply-partially #'add-hook
                                                               'message-header-setup-hook
                                                               link-header))
                             (remove-link-header (apply-partially #'remove-hook
                                                                  'message-header-setup-hook
                                                                  link-header)))
                        (funcall add-link-header)
                        (condition-case err
                            (progn
                              (apply f args)
                              (funcall remove-link-header))
                          (error (funcall remove-link-header)
                                 (error (error-message-string err)))))
                      t)
                     (apply f args)))
                (t (apply f args)))))
       (advise-gnus-summary-followup
        (lambda ()
          (add-function :around (symbol-function 'gnus-summary-followup) prompt-loose)))
       (suspend-prompt-loose
        (lambda (f &rest args)
          (cond ((nnreddit--gate)
                 (remove-function (symbol-function 'gnus-summary-followup) prompt-loose)
                 (condition-case err
                     (prog1 (apply f args)
                       (funcall advise-gnus-summary-followup))
                   (error (funcall advise-gnus-summary-followup)
                          (error (error-message-string err)))))
                (t (apply f args)))))
       (advise-gnus-summary-cancel-article
        (lambda ()
          (add-function :around (symbol-function 'gnus-summary-cancel-article)
                        suspend-prompt-loose))))
  (funcall advise-gnus-summary-cancel-article)
  (funcall advise-gnus-summary-followup))

(add-function
 :around (symbol-function 'message-supersede)
 (lambda (f &rest args)
   (cond ((nnreddit--gate)
          (add-function :override
                        (symbol-function 'mml-insert-mml-markup)
                        'ignore)
          (condition-case err
              (prog1 (apply f args)
                (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                (save-excursion
                  (save-restriction
                    (message-replace-header "From" (message-make-from))
                    (message-goto-body)
                    (narrow-to-region (point) (point-max))
                    (goto-char (point-max))
                    (mm-inline-text-html nil)
                    (delete-region (point-min) (point)))))
            (error (remove-function (symbol-function 'mml-insert-mml-markup) 'ignore)
                   (error (error-message-string err)))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'message-send-news)
 (lambda (f &rest args)
   (cond ((nnreddit--gate)
          (let* ((dont-ask (lambda (prompt)
                             (when (cl-search "mpty article" prompt) t)))
                 (link-p (not (null (message-fetch-field "Link"))))
                 (message-shoot-gnksa-feet (if link-p t message-shoot-gnksa-feet)))
            (condition-case err
                (progn
                  (when link-p
                    (add-function :before-until (symbol-function 'y-or-n-p) dont-ask))
                  (prog1 (apply f args)
                    (remove-function (symbol-function 'y-or-n-p) dont-ask)))
              (error (remove-function (symbol-function 'y-or-n-p) dont-ask)
                     (error (error-message-string err))))))
         (t (apply f args)))))

(add-function
 :around (symbol-function 'gnus-summary-post-news)
 (lambda (f &rest args)
   (cond ((nnreddit--gate)
          (let* ((nnreddit-post-type (read-char-choice "[l]ink / [t]ext: " '(?l ?t)))
                 (link-header (apply-partially #'message-add-header "Link: https://"))
                 (add-link-header (apply-partially #'add-hook
                                                   'message-header-setup-hook
                                                   link-header))
                 (remove-link-header (apply-partially #'remove-hook
                                                      'message-header-setup-hook
                                                      link-header)))
            (cl-case nnreddit-post-type
              (?l (funcall add-link-header)))
            (condition-case err
                (progn
                  (apply f args)
                  (funcall remove-link-header))
              (error (funcall remove-link-header)
                     (error (error-message-string err))))))
         (t (apply f args)))))

(add-function
 :filter-return (symbol-function 'message-make-fqdn)
 (lambda (val)
   (if (and (nnreddit--gate)
            (cl-search "--so-tickle-me" val))
       "reddit.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (nnreddit--gate)
     (concat (nnreddit-rpc-call nil nil "user_attr" "name") "@reddit.com"))))

(add-function
 :around (symbol-function 'message-is-yours-p)
 (lambda (f &rest args)
   (let ((concat-func (lambda (f &rest args)
                       (let ((fetched (apply f args)))
                         (if (string= (car args) "from")
                             (concat fetched "@reddit.com")
                           fetched)))))
     (when (nnreddit--gate)
       (add-function :around
                     (symbol-function 'message-fetch-field)
                     concat-func))
     (condition-case err
         (prog1 (apply f args)
           (remove-function (symbol-function 'message-fetch-field) concat-func))
       (error (remove-function (symbol-function 'message-fetch-field) concat-func)
              (error (error-message-string err)))))))

(add-function
 :around (symbol-function 'url-http-generic-filter)
 (lambda (f &rest args)
   (cond ((nnreddit--gate)
          (condition-case err
              (apply f args)
            (error (gnus-message 7 "url-http-generic-filter: %s"
                                 (error-message-string err)))))
         (t (apply f args)))))

;; the let'ing to nil of `gnus-summary-display-article-function'
;; in `gnus-summary-select-article' dates back to antiquity.
(add-function
 :around (symbol-function 'gnus-summary-display-article)
 (lambda (f &rest args)
   (cond ((nnreddit--gate)
          (let ((gnus-summary-display-article-function
                 (symbol-function 'nnreddit--display-article)))
            (apply f args)))
         (t (apply f args)))))

;; disallow caching as the article numbering is wont to change
;; after PRAW restarts!
(setq gnus-uncacheable-groups
      (aif gnus-uncacheable-groups
          (format "\\(%s\\)\\|\\(^nnreddit\\)" it)
        "^nnreddit"))

(provide 'nnreddit)

;;; nnreddit.el ends here
