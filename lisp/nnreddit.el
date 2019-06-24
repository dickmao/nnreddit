;;; nnreddit.el --- Gnus backend for reddit  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019 The Authors of nnreddit.el

;; Authors: Paul Issartel <paul.issartel@u-psud.fr>
;;          dickmao <github id: dickmao>
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
(require 'python)
(require 'json-rpc)
(require 'mm-url)
(require 'cl-lib)
(require 'virtualenvwrapper)

(nnoo-declare nnreddit)

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

(defconst nnreddit-venv
  (unless noninteractive
    (let* ((library-directory (file-name-directory (locate-library "nnreddit")))
           (defacto-version (file-name-nondirectory
                             (directory-file-name library-directory)))
           (result (concat defacto-version "-" nnreddit-python-command)))
      (prog1 result
        (gnus-message 7 "nnreddit-venv: %s%s" venv-location result)
        (unless (member result (split-string (venv-list-virtualenvs)))
          (gnus-message 5 "nnreddit-venv: installing venv to %s..." result)
          (condition-case err
              (progn
                (venv-mkvirtualenv-using nnreddit-python-command result)
                (venv-with-virtualenv-shell-command
                 result
                 (format "cd %s && python setup.py install" library-directory)))
            (error (venv-rmvirtualenv result)
                   (error (error-message-string err))))
          (gnus-message 5 "nnreddit-venv: installing venv to %s...done" result)))))
  "Venv directory name in `venv-location'.

To facilitate upgrades, the name gloms a de facto version (the directory
name where this file resides) and the `nnreddit-python-command'.")

;; keymaps made by `define-prefix-command' in `gnus-define-keys-1'
(defvar nnreddit-article-mode-map)
(defvar nnreddit-group-mode-map)

;; keymaps I make myself
(defvar nnreddit-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" 'gnus-summary-followup)
    (define-key map "R" 'gnus-summary-followup-with-original)
    (define-key map "F" 'gnus-summary-followup-with-original)
    map))

(defcustom nnreddit-log-rpc nil
  "Turn on PRAW logging."
  :type 'boolean
  :group 'nnreddit)

(defvar nnreddit-rpc-log-filename nil)

(define-minor-mode nnreddit-article-mode
  "Minor mode for nnreddit articles.  Disallow `gnus-article-reply-with-original'.

\\{gnus-article-mode-map}
"
  :lighter " Reddit"
  :keymap gnus-article-mode-map

  (when nnreddit-article-mode
    (gnus-define-keys (nnreddit-article-mode-map "R" gnus-article-mode-map)
      "0" nnreddit-novote
      "-" nnreddit-downvote
      "=" nnreddit-upvote
      "+" nnreddit-upvote)
    ;; WHY????
    (define-key gnus-article-mode-map "F" 'gnus-summary-followup-with-original)))

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
  :keymap gnus-group-mode-map
  (when nnreddit-group-mode
    (gnus-define-keys (nnreddit-group-mode-map "R" gnus-group-mode-map)
      "g" nnreddit-goto-group)))

(defun nnreddit-goto-group (realname)
  "Jump to the REALNAME subreddit."
  (interactive (list (read-no-blanks-input "Subreddit: r/")))
  (let ((group (gnus-group-full-name realname "nnreddit")))
    (gnus-activate-group group t)
    (gnus-group-read-group t t group)))

(defsubst nnreddit-novote ()
  "Retract vote."
  (interactive)
  (nnreddit-vote-current-article 0))

(defsubst nnreddit-downvote ()
  "Downvote the article in current buffer."
  (interactive)
  (nnreddit-vote-current-article -1))

(defsubst nnreddit-upvote ()
  "Upvote the article in current buffer."
  (interactive)
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

(defmacro nnreddit-aif (test-form then-form &rest else-forms)
  "Anaphoric if TEST-FORM THEN-FORM ELSE-FORMS.  Adapted from `e2wm:aif'."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'nnreddit-aif 'lisp-indent-function 2)

(defmacro nnreddit-aand (test &rest rest)
  "Anaphoric conjunction of TEST and REST.  Adapted from `e2wm:aand'."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(nnreddit-aand ,@rest)) 'it))))

(defmacro nnreddit-and-let* (bindings &rest form)
  "Gauche's `and-let*'.  Each of BINDINGS must resolve to t before evaluating FORM."
  (declare (debug ((&rest &or symbolp (form) (gate symbolp &optional form))
                   body))
           ;; See: (info "(elisp) Specification List")
           (indent 1))
  (if (null bindings)
      `(progn ,@form)
    (let* ((head (car bindings))
           (tail (cdr bindings))
           (rest (macroexpand-all `(nnreddit-and-let* ,tail ,@form))))
      (cond
       ((symbolp head) `(if ,head ,rest))
       ((= (length head) 1) `(if ,(car head) ,rest))
       (t `(let (,head) (if ,(car head) ,rest)))))))

(defvar nnreddit-headers-hashtb (gnus-make-hashtable)
  "Group (subreddit) string -> interleaved submissions and comments sorted by created time.")

(defvar nnreddit-refs-hashtb (gnus-make-hashtable)
  "Who replied to whom (global over all entries).")

(defvar nnreddit-authors-hashtb (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries).")

(defsubst nnreddit-get-headers (group)
  "List headers from GROUP."
  (gnus-gethash-safe group nnreddit-headers-hashtb))

(defun nnreddit-find-header (group id)
  "O(n) search of GROUP headers for ID."
  (nnreddit-and-let* ((headers (nnreddit-get-headers group))
                      (found (seq-position headers id
                                           (lambda (plst id)
                                             (equal id (plist-get plst :id))))))
                     (nnreddit--get-header (1+ found) group)))

(defsubst nnreddit-refs-for (name &optional depth)
  "Get message ancestry for NAME up to DEPTH."
  (unless depth
    (setq depth most-positive-fixnum))
  (when (> depth 0)
    (nreverse (cl-loop with parent-id = (gnus-gethash-safe name nnreddit-refs-hashtb)
                       for level = 0 then level
                       for name = parent-id then
                       (gnus-gethash-safe name nnreddit-refs-hashtb)
                       until (null name)
                       collect name
                       until (>= (cl-incf level) depth)))))

(defsubst nnreddit-sort-append-headers (group &rest lvp)
  "Append to hashed headers of GROUP the LVP (list of vector of plists)."
  (gnus-sethash group (append (nnreddit-get-headers group)
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
  (let* ((connection (json-rpc--create :process (nnreddit-rpc-get server)
                                       :host "localhost"
                                       :id-counter 0))
         (result (apply #'nnreddit-rpc-request connection generator_kwargs method args)))
    result))

(defun nnreddit-vote-current-article (vote)
  "VOTE is +1, -1, 0."
  (unless gnus-article-current ;; gnus-article-current or gnus-current-article?
    (error "No current article"))
  (unless gnus-newsgroup-name
    (error "No current newgroup"))
  (let* ((header (nnreddit--get-header (cdr gnus-article-current)
                                       (gnus-group-real-name (car gnus-article-current))))
         (orig-score (format "%s" (plist-get header :score)))
         (new-score (if (zerop vote) orig-score
                      (concat orig-score " "
                              (if (> vote 0) "+" "")
                              (format "%s" vote))))
         (article-name (plist-get header :name)))
    (let ((inhibit-read-only t))
      (nnheader-replace-header "score" new-score))
    (nnreddit-rpc-call nil nil "vote" article-name vote)))

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
          (gnus-newsgroup-name (gnus-group-prefixed-name group "nnreddit")))
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
  "Cite AUTHOR using `gnus-message-cite-prefix-regexp' before displaying BODY."
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
  (gnus-sethash (plist-get e :name) (plist-get e field) hashtb))

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
                (list group
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
                  (nnreddit-aif (seq-position
                                 headers
                                 newsrc-seen-id
                                 (lambda (plst newsrc-seen-id)
                                   (or (null newsrc-seen-id)
                                       (>= (nnreddit--base10 (plist-get plst :id))
                                           (nnreddit--base10 newsrc-seen-id)))))
                      (1+ it) 0))
                 (updated-seen-index (- num-headers
                                        (nnreddit-aif
                                            (seq-position (reverse headers) nil
                                                          (lambda (plst _e)
                                                            (not (plist-get plst :title))))
                                            it -1)))
                 (updated-seen-id (nnreddit-aif (nth (1- updated-seen-index) headers)
                                      (plist-get it :id) ""))
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
            (while (assq 'last-seen params)
              (gnus-alist-pull 'last-seen params))
            (gnus-info-set-params
             info
             (cons `(last-seen ,updated-seen-index . ,updated-seen-id) params)
             t)
            (gnus-set-info gnus-newsgroup-name info)
            (gnus-message 7 "nnreddit-request-group: new info=%s" info)))))
    t))

(deffoo nnreddit-request-scan (&optional group server)
  (nnreddit--normalize-server)
  (unless (null group)
    (nnreddit--with-group group
      (let* ((comments (nnreddit-rpc-call server nil "comments" group))
             (raw-submissions (nnreddit-rpc-call server nil "submissions" group))
             (submissions (and (> (length comments) 0)
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
             (and (integerp score)
                  `((X-Reddit-Score . ,(number-to-string score))))
             (and (integerp num-comments)
                  `((X-Reddit-Comments . ,(number-to-string num-comments))))))))

(deffoo nnreddit-request-article (article-number &optional group server buffer)
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (with-current-buffer (or buffer nntp-server-buffer)
      (erase-buffer)
      (let* ((header (nnreddit--get-header article-number group))
             (mail-header (nnreddit--make-header article-number))
             (score (cdr (assq 'X-Reddit-Score (mail-header-extra mail-header))))
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
           "Score: " score "\n"
           "\n")
          (nnreddit-and-let* ((parent-name (plist-get header :parent_id)) ;; parent-id is full
                         (parent-author (or (gnus-gethash-safe parent-name
                                                               nnreddit-authors-hashtb)
                                            "Someone"))
                         (parent-body (nnreddit--get-body parent-name group server)))
            (insert (nnreddit--citation-wrap parent-author parent-body)))
          (insert (nnreddit--br-tagify body))
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
                   (nnreddit-aif next-earliest
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
  "Set flag for ensuing `nnreddit-request-group' to avoid going out to PRAW yet again."
  (nnreddit--normalize-server)
  (with-current-buffer nntp-server-buffer
    (let ((groups (nnreddit-rpc-call server nil "user_subreddits")))
      (mapc (lambda (realname)
              (let ((group (gnus-group-full-name realname '("nnreddit" (or server "")))))
                (erase-buffer)
                (gnus-message 5 "nnreddit-request-list: scanning %s..." realname)
                (gnus-activate-group group t)
                (gnus-message 5 "nnreddit-request-list: scanning %s...done" realname)
                (gnus-group-unsubscribe-group group gnus-level-default-subscribed t)))
            groups)
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

(defun nnreddit-rpc-get (&optional server)
  "Retrieve the PRAW process for SERVER."
  (nnreddit--normalize-server)
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
                                    (format "%s%s/bin/python" venv-location nnreddit-venv)
                                  (executable-find nnreddit-python-command)))
             (python-module (if (featurep 'test) "tests" "nnreddit"))
             (praw-command (list python-executable "-m" python-module)))
        (when nnreddit-log-rpc
          (setq nnreddit-rpc-log-filename
                (concat (file-name-as-directory temporary-file-directory)
                        "nnreddit-rpc-log."))
          (setq praw-command (append praw-command (list "--log" nnreddit-rpc-log-filename))))
        (setq proc (make-process :name server
                                 :buffer (get-buffer-create (format " *%s*" server))
                                 :command praw-command
                                 :connection-type 'pipe
                                 :noquery t
                                 :sentinel #'nnreddit-sentinel
                                 :stderr (get-buffer-create (format " *%s-stderr*" server)))))
      (push proc nnreddit-processes))
    proc))

(defun nnreddit-rpc-request (connection kwargs method &rest args)
  "Send to CONNECTION a request with generator KWARGS calling METHOD ARGS.  `json-rpc--request' assumes HTTP transport which jsonrpyc does not."
  (unless (hash-table-p kwargs)
    (setq kwargs #s(hash-table)))
  (let* ((id (cl-incf (json-rpc-id-counter connection)))
         (request `(:method ,method
                    :id ,id
                    :params (:args ,(apply json-array-type args) :kwargs ,kwargs)))
         (proc (json-rpc-process (json-rpc-ensure connection)))
         (encoded (json-encode (append '(:jsonrpc "2.0") request)))
         (json-object-type 'plist)
         (json-key-type 'keyword))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (gnus-message 7 "nnreddit-rpc-request: send %s" encoded)
      (process-send-string proc (concat encoded "\n"))
      (cl-loop repeat 10
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
               do (accept-process-output proc 6 0)
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
    (cond (reply-p (nnreddit-rpc-call server nil "reply"
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

(add-to-list 'gnus-parameters `("^nnreddit"
                                (gnus-summary-make-false-root 'adopt)
                                (gnus-cite-hide-absolute 5)
                                (gnus-cite-hide-percentage 0)
                                (gnus-cited-lines-visible '(2 . 2))
                                (gnus-auto-extend-newsgroup nil)
                                (gnus-add-timestamp-to-message t)
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
  (setq gnus-group-change-level-function 'nnreddit-update-subscription)
  (nnreddit-group-mode))

;; I believe I did try buffer-localizing hooks, and it wasn't sufficient
(add-hook 'gnus-article-mode-hook 'nnreddit-article-mode-activate)
(add-hook 'gnus-group-mode-hook 'nnreddit-group-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nnreddit-summary-mode-activate)

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nnreddit" post-mail) t)

;; Add prompting for replying to thread root to gnus-summary-followup.
;; The interactive spec of gnus-summary-followup is putatively preserved.
(add-function :around (symbol-function 'gnus-summary-followup)
   (lambda (f &rest args)
     (cond ((eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
            (or (nnreddit-and-let*
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
                     ((eq reply-root ?r)))
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

(add-function
 :around (symbol-function 'message-send-news)
 (lambda (f &rest args)
   (cond ((eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
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
   (cond ((eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
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
   (if (and (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
            (cl-search "--so-tickle-me" val))
       "reddit.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest _args)
   (when (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
     (concat (nnreddit-rpc-call nil nil "user_attr" "name") "@reddit.com"))))

;; disallow caching as the article numbering is wont to change
;; after PRAW restarts!
(setq gnus-uncacheable-groups
      (nnreddit-aif gnus-uncacheable-groups
          (format "\\(%s\\)\\|\\(^nnreddit\\)" it)
        "^nnreddit"))

(provide 'nnreddit)

;;; nnreddit.el ends here
