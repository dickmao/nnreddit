;;; nnreddit.el --- gnus backend for reddit

;; Copyright (C) 2016-2019 Free Software Foundation, Inc.

;; Authors: Paul Issartel <paul.issartel@u-psud.fr>
;;          dickmao <github id: dickmao>
;; Keywords: gnus, reddit

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
;; along with nnreddit.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Gnus            Reddit
;; ----            ------
;; list            subscribed subreddits
;; group           subreddit
;; threads         threads
;; root article    link or submission
;; articles        {root article, comments}

;;; Code:

;;; set up all-things-python on `package-install', `package-install-file'
(eval-and-compile
  (require 'virtualenvwrapper)
  (defcustom nnreddit-python-command (if (equal system-type 'windows-nt)
                                         (or (executable-find "py")
                                             (executable-find "pythonw")
                                             "python")
                                       "python")
    "what is python on your system."
    :type (append '(choice)
                  (let (result)
                    (dolist (py '("python" "python2" "python3" "pythonw" "py")
                                result)
                      (setq result (append result `((const :tag ,py ,py))))))
                  '((string :tag "Other")))
    :group 'nnreddit)
  (unless (member "nnreddit" (split-string (venv-list-virtualenvs)))
    (venv-mkvirtualenv-using nnreddit-python-command "nnreddit")
    (venv-with-virtualenv-shell-command "nnreddit"
                                        (format "cd %s && pip install -r requirements.txt"
                                                (file-name-directory (locate-library "nnreddit"))))
    (venv-with-virtualenv-shell-command "nnreddit"
                                        (format "cd %s && python setup.py install"
                                                (file-name-directory (locate-library "nnreddit"))))))

(require 'nnoo)
(require 'gnus)
(require 'gnus-start)
(require 'gnus-art)
(require 'gnus-sum)
(require 'gnus-msg)
(require 'gnus-cite)
(require 'gnus-srvr)
(require 'python)
(require 'json-rpc)
(require 'mm-url)
(require 'cl-lib)

(nnoo-declare nnreddit)

;; keymaps made by `define-prefix-command' in `gnus-define-keys-1'
(defvar nnreddit-article-mode-map)
(defvar nnreddit-group-mode-map)

;; keymaps I make myself
(defvar nnreddit-summary-mode-map (make-sparse-keymap))

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

(let ((map nnreddit-summary-mode-map))
  (define-key map "r" 'gnus-summary-followup)
  (define-key map "R" 'gnus-summary-followup-with-original))

(define-minor-mode nnreddit-group-mode
  "Add `R-g' go-to-subreddit binding to *Group*.

\\{gnus-group-mode-map}
"
  :keymap gnus-group-mode-map
  (when nnreddit-group-mode
    (gnus-define-keys (nnreddit-group-mode-map "R" gnus-group-mode-map)
      "g" nnreddit-goto-group)))

(defun nnreddit-goto-group (realname)
  (interactive (list (read-no-blanks-input "Subreddit: r/")))
  (let ((group (gnus-group-full-name realname "nnreddit")))
    (gnus-group-read-group t t group)))

(defsubst nnreddit-novote ()
  (interactive)
  (nnreddit-vote-current-article 0))

(defsubst nnreddit-downvote ()
  (interactive)
  (nnreddit-vote-current-article -1))

(defsubst nnreddit-upvote ()
  (interactive)
  (nnreddit-vote-current-article 1))

(defvar seq-map-indexed-f (if (fboundp 'seq-map-indexed)
                              #'seq-map-indexed
                            (lambda (function sequence) 
                              (let ((index 0))
                                (seq-map (lambda (elt)
                                           (prog1
                                               (funcall function elt index)
                                             (setq index (1+ index))))
                                         sequence)))))

(defmacro nnreddit--normalize-server ()
  `(let ((canonical "nnreddit-default"))
    (when (equal server "")
      (setq server nil))
    (unless server
      (setq server canonical))
    (unless (string= server canonical)
      (error "nnreddit--normalize-server: multiple servers unsupported!"))))

(defmacro ein:aif (test-form then-form &rest else-forms)
  "Anaphoric IF.  Adapted from `e2wm:aif'."
  (declare (debug (form form &rest form)))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))
(put 'ein:aif 'lisp-indent-function 2)

(defmacro ein:aand (test &rest rest)
  "Anaphoric AND.  Adapted from `e2wm:aand'."
  (declare (debug (form &rest form)))
  `(let ((it ,test))
     (if it ,(if rest (macroexpand-all `(ein:aand ,@rest)) 'it))))

(defmacro ein:and-let* (bindings &rest form)
  "Gauche's `and-let*'."
  (declare (debug ((&rest &or symbolp (form) (gate symbolp &optional form))
                   body))
           ;; See: (info "(elisp) Specification List")
           (indent 1))
  (if (null bindings)
      `(progn ,@form)
    (let* ((head (car bindings))
           (tail (cdr bindings))
           (rest (macroexpand-all `(ein:and-let* ,tail ,@form))))
      (cond
       ((symbolp head) `(if ,head ,rest))
       ((= (length head) 1) `(if ,(car head) ,rest))
       (t `(let (,head) (if ,(car head) ,rest)))))))

(defvar *nnreddit-headers-hashtb* (gnus-make-hashtable)
  "group (subreddit) string -> interleaved submissions and comments sorted by created time")

(defvar *nnreddit-refs-hashtb* (gnus-make-hashtable)
  "Who replied to whom (global over all entries)")

(defvar *nnreddit-authors-hashtb* (gnus-make-hashtable)
  "For fast lookup of parent-author (global over all entries)")

(defsubst nnreddit-get-headers (group)
  (gnus-gethash-safe group *nnreddit-headers-hashtb*))

(defun nnreddit-find-header (group id)
  "O(n) search of headers for ID"
  (ein:and-let* ((headers (nnreddit-get-headers group))
                 (found (seq-position headers id
                                      (lambda (plst id)
                                        (equal id (plist-get plst :id))))))
    (nnreddit--get-header (1+ found) group)))

(defsubst nnreddit-refs-for (name &optional depth)
  (unless depth
    (setq depth most-positive-fixnum))
  (when (> depth 0)
    (nreverse (cl-loop with parent-id = (gnus-gethash-safe name *nnreddit-refs-hashtb*)
                       for level = 0 then level
                       for name = parent-id then
                       (gnus-gethash-safe name *nnreddit-refs-hashtb*)
                       until (null name)
                       collect name
                       until (>= (cl-incf level) depth)))))

(defsubst nnreddit--dig-submissions (group submissions comments)
  (let* ((accounted-for (gnus-make-hashtable)))
    (mapc (lambda (plst) (gnus-sethash (plist-get plst :name) t accounted-for)) submissions)
    (seq-doseq (e comments)
      (let ((upmost (car (nnreddit-refs-for (plist-get e :name)))))
        (unless (gnus-gethash-safe upmost accounted-for)
          nil)))))

(defsubst nnreddit-sort-append-headers (group &rest lvp)
  (gnus-sethash group (append (nnreddit-get-headers group)
                              (apply #'nnreddit--sort-headers lvp))
                *nnreddit-headers-hashtb*))

(defsubst gnus-hash-keys (hashtb)
  (let (lst)
    (mapatoms (lambda (e) (setq lst (nconc lst (list e)))) hashtb)
    lst))

(defsubst gnus-hash-values (hashtb)
  (let (lst)
    (mapatoms (lambda (e) (setq lst (nconc lst (list (symbol-value e))))) hashtb)
    lst))

(defvar *nnreddit-directory* (nnheader-concat gnus-directory "reddit")
  "Where to retrieve last read state.")

(defvar *nnreddit-processes* nil
  "Garbage collect PRAW processes.")

(nnoo-define-basics nnreddit)

(defcustom nnreddit-use-virtualenv (not noninteractive)
  "nnreddit developers will appreciate talking to the python module living in their dev space, not the virtualenv space."
  :type 'boolean
  :group 'nnreddit)

(defcustom nnreddit-page-size 25
  "Initially retrieve these many submissions."
  :type '(integer
          :validate (lambda (w) (let ((v (widget-value w)))
                                  (unless (and (<= v 200) (> v 0))
                                    (widget-put w :error "Page size range 1-200")
                                    w))))
  :group 'nnreddit)

(defsubst nnreddit-rpc-call (server generator_kwargs method &rest args)
  (nnreddit--normalize-server)
  (let* ((connection (json-rpc--create :process (nnreddit-rpc-get server)
                                       :host "localhost"
                                       :id-counter 0))
         (result (apply #'nnreddit-rpc-request connection generator_kwargs method args)))
    result))

(defun nnreddit-vote-current-article (vote)
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

(defun nnreddit-update-subscription (group level oldlevel &optional previous)
  "nnreddit `gnus-group-change-level' callback"
  (let ((old-subbed-p (<= oldlevel gnus-level-default-subscribed))
        (new-subbed-p (<= level gnus-level-default-subscribed)))
    (unless (eq old-subbed-p new-subbed-p)
      ;; afaict, praw post() doesn't return status
      (if new-subbed-p
          (nnreddit-rpc-call nil nil "subscribe" (gnus-group-real-name group))
        (nnreddit-rpc-call nil nil "unsubscribe" (gnus-group-real-name group))))))

(defun nnreddit-rpc-kill (&optional server)
  (interactive (list nil))
  (nnreddit--normalize-server)
  (let (new-processes)
    (mapc (lambda (proc) (if (and server (not (string= server (process-name proc))))
                             (push proc new-processes)
                           (delete-process proc)))
          *nnreddit-processes*)
    (setq *nnreddit-processes* new-processes)))

(deffoo nnreddit-request-close ()
  (nnreddit-close-server)
  t)

(deffoo nnreddit-request-type (group &optional article)
  'news)

(deffoo nnreddit-server-opened (&optional server)
  (nnreddit--normalize-server)
  (cl-remove-if-not (lambda (proc) (string= server (process-name proc)))
                 *nnreddit-processes*))

(deffoo nnreddit-status-message (&optional server)
  (nnreddit--normalize-server)
  "")

(deffoo nnreddit-open-server (server &optional defs)
  t)

(deffoo nnreddit-close-group (group &optional server)
  (nnreddit--normalize-server)
  t)

(defmacro nnreddit--with-group (group &rest body)
  (declare (debug (form &rest form))
           (indent 1))
  `(let* ((group (or ,group (gnus-group-real-name gnus-newsgroup-name)))
          (gnus-newsgroup-name (gnus-group-prefixed-name group "nnreddit")))
     ,@body))

(defun nnreddit--get-header (article-number &optional group)
  (nnreddit--with-group group
    (let ((headers (nnreddit-get-headers group)))
      (elt headers (1- article-number)))))

(defun nnreddit--get-body (name &optional group server)
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (nnreddit-rpc-call server nil "body" group name)))

(defsubst nnreddit-hack-name-to-id (name)
  (cl-subseq name 3))

(defsubst nnreddit--br-tagify (body)
  "Reddit-html shies away from <BR>.  Should it?"
  (replace-regexp-in-string "\n" "<br>" body))

(defsubst nnreddit--citation-wrap (author body)
  "Precede each line with `gnus-message-cite-prefix-regexp'."
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
  (gnus-sethash (plist-get e :name) (plist-get e field) hashtb))

(defun nnreddit--filter-after (after-this vop) ;; vector of plists
  (cl-loop for elt-idx in (funcall seq-map-indexed-f (lambda (elt idx) (cons elt idx)) vop)
           until (>= (plist-get (car elt-idx) :created_utc) after-this)
           finally return (seq-drop vop (or (cdr elt-idx) 0))))

(defsubst nnreddit--base10 (base36)
  (apply #'+ (funcall seq-map-indexed-f
                      (lambda (elt idx)
                        (* (expt 36 idx)
                           (if (>= elt ?a) (+ 10 (- elt ?a)) (- elt ?0))))
                      (reverse base36))))

;; gnus-group-select-group
;;   gnus-group-read-group
;;     gnus-summary-read-group
;;       gnus-summary-read-group-1
;;         gnus-summary-setup-buffer
;;           sets gnus-newsgroup-name
;;         gnus-select-newsgroup
;;           gnus-request-group
;;             nnreddit-request-group
(deffoo nnreddit-request-group (group &optional server fast info)
  (nnreddit--normalize-server)
  ;; (nnreddit-close-server server)
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
           (newsrc-seen-id (cdr newsrc-seen-cons))
           (comments (nnreddit-rpc-call server nil "comments" group))
           (raw-submissions (nnreddit-rpc-call server nil "submissions" group))
           (submissions (and (> (length comments) 0)
                             (nnreddit--filter-after
                              (- (plist-get (aref comments 0) :created_utc) 7200)
                              raw-submissions))))
      (seq-doseq (e comments)
        (nnreddit-add-entry *nnreddit-refs-hashtb* e :parent_id)) ;; :parent_id is fullname

      (seq-doseq (e (vconcat submissions comments))
        (nnreddit-add-entry *nnreddit-authors-hashtb* e :author))

      (nnreddit-sort-append-headers group submissions comments)

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
          (let* ((newsrc-seen-index-now
                  (ein:aif (seq-position
                            headers
                            newsrc-seen-id
                            (lambda (plst newsrc-seen-id)
                              (or (null newsrc-seen-id)
                                  (>= (nnreddit--base10 (plist-get plst :id))
                                      (nnreddit--base10 newsrc-seen-id)))))
                      (1+ it) 0))
                 (updated-seen-index (- num-headers
                                        (ein:aif
                                            (seq-position (reverse headers) nil
                                                          (lambda (plst _e)
                                                            (not (plist-get plst :title))))
                                            it -1)))
                 (updated-seen-id (ein:aif (nth (1- updated-seen-index) headers)
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
            (while (assq 'last-seen params)
              (gnus-alist-pull 'last-seen params))
            (gnus-info-set-params
             info
             (cons `(last-seen ,updated-seen-index . ,updated-seen-id) params)
             t)
            (gnus-set-info gnus-newsgroup-name info)
            (gnus-message 7 "nnreddit-request-group: new info=%s" info)))
        t))))

(defsubst nnreddit--make-message-id (fullname)
  (format "<%s@reddit.com>" fullname))

(defsubst nnreddit--make-references (fullname)
  (mapconcat (lambda (ref) (nnreddit--make-message-id ref))
             (nnreddit-refs-for fullname) " "))

(defsubst nnreddit--make-header (article-number &optional group)
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
           "From: " (mail-header-from mail-header) "\n"
           "Date: " (mail-header-date mail-header) "\n"
           "Message-ID: " (mail-header-id mail-header) "\n"
           "References: " (mail-header-references mail-header) "\n"
           "Content-Type: text/html; charset=utf-8" "\n"
           "Score: " score "\n"
           "\n")
          (ein:and-let* ((parent-name (plist-get header :parent_id)) ;; parent-id is full
                         (parent-author (or (gnus-gethash-safe parent-name
                                                               *nnreddit-authors-hashtb*)
                                            "Someone"))
                         (parent-body (nnreddit--get-body parent-name group server)))
            (insert (nnreddit--citation-wrap parent-author parent-body)))
          (insert (nnreddit--br-tagify body))
          (cons group article-number))))))

(deffoo nnreddit-retrieve-headers (article-numbers &optional group server fetch-old)
  (nnreddit--normalize-server)
  (nnreddit--with-group group
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (i article-numbers)
        (nnheader-insert-nov (nnreddit--make-header i group)))
      'nov)))

(defsubst nnreddit--earliest-among (indices lvp) ;; list-of-vectors-of-plists
  "Return (list-to-iterate . next-earliest)"
  (let (earliest next-earliest)
    (dolist (plst-idx
             (cl-remove-if-not #'car
                            (funcall seq-map-indexed-f
                                     (lambda (plst idx) (cons plst idx))
                                     (seq-mapn
                                      (lambda (v i)
                                        (if (< i (length v)) (aref v i)))
                                      lvp indices)))
             (list (cdr earliest)
                   (ein:aif next-earliest
                       (plist-get (car it) :created_utc))))
      (cond ((null earliest)
             (setq earliest plst-idx))
            ((< (plist-get (car plst-idx) :created_utc)
                (plist-get (car earliest) :created_utc))
             (setq next-earliest earliest)
             (setq earliest plst-idx))
            ((null next-earliest)
             (setq next-earliest plst-idx))))))

(defun nnreddit--sort-headers (&rest lvp) ;; list-of-vectors-of-plists
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
    (erase-buffer)
    (let ((groups (nnreddit-rpc-call server nil "user_subreddits")))
      (mapc (lambda (realname)
              (let ((group (gnus-group-full-name realname '("nnreddit" (or server "")))))
                (gnus-activate-group group)
                (gnus-group-unsubscribe-group group gnus-level-default-subscribed t)))
            groups)
      (erase-buffer)
      (mapc (lambda (group) 
              (insert (format "%s %d 1 y\n" group
                              (length (nnreddit-get-headers group)))))
            groups)))
  t)

(defun nnreddit-sentinel (process event)
  "Wipe headers state when PROCESS dies."
  (when (not (string= "open" (substring event 0 4)))
    (gnus-message 2 "nnreddit-sentinel: process %s %s"
                  (car (process-command process))
                  (replace-regexp-in-string "\n$" "" event))
    (setq *nnreddit-headers-hashtb* (gnus-make-hashtable))))

(defun nnreddit-rpc-get (&optional server)
  "Retrieve the PRAW process for SERVER."
  (nnreddit--normalize-server)
  (let ((proc (get-buffer-process (get-buffer-create (format " *%s*" server)))))
    (unless proc
      (let* ((python-shell-extra-pythonpaths
              `(,(file-name-directory (locate-library "nnreddit"))))
             (process-environment (python-shell-calculate-process-environment))
             (python-executable (if nnreddit-use-virtualenv
                                    (format "%snnreddit/bin/python" venv-location)
                                  (executable-find nnreddit-python-command)))
             (python-module (if (featurep 'test) "tests" "nnreddit")))
        (setq proc (make-process :name server
                                 :buffer (get-buffer-create (format " *%s*" server))
                                 :command (list python-executable "-m" python-module)
                                 :connection-type 'pipe
                                 :noquery t
                                 :sentinel #'nnreddit-sentinel)))
      (push proc *nnreddit-processes*))
    proc))

(defun nnreddit-rpc-wait (connection)
  "Wait for the response from CONNECTION and return it, or signal the error."
  (with-current-buffer (process-buffer (json-rpc-process connection))
    (with-local-quit
      (cl-loop until (or (not (zerop (length (buffer-string))))
                         (not (json-rpc-live-p connection)))
            do (accept-process-output)
            finally
            (goto-char (point-min))
            (let* ((json-object-type 'plist)
                   (json-key-type 'keyword)
                      (result (json-read)))
                 (if (plist-get result :error)
                     (signal 'json-rpc-error (plist-get result :error))
                   (cl-return (plist-get result :result))))))))

(defun nnreddit-rpc-request (connection kwargs method &rest args)
  "`json-rpc--request' assumes HTTP transport which jsonrpyc does not."
  (unless (hash-table-p kwargs)
    (setq kwargs #s(hash-table)))
  (let* ((id (cl-incf (json-rpc-id-counter connection)))
         (request `(:method ,method
                    :id ,id
                    :params (:args ,(apply json-array-type args) :kwargs ,kwargs)))
         (process (json-rpc-process (json-rpc-ensure connection)))
         (encoded (json-encode (nconc '(:jsonrpc "2.0") request))))
    (with-current-buffer (process-buffer process)
      (erase-buffer))
    (process-send-string process (concat encoded "\n"))
    (nnreddit-rpc-wait connection)))

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
                                      (plist-get header :name) body))
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
  "Augment the gnus-article-mode-map conditionally."
  (when (and (stringp gnus-newsgroup-name)
             (eq 'nnreddit (car (gnus-group-method gnus-newsgroup-name))))
    (nnreddit-article-mode)))

(defun nnreddit-summary-mode-activate ()
  "Shadow some bindings in gnus-summary-mode-map conditionally."
  (when (and (stringp gnus-newsgroup-name)
             (eq 'nnreddit (car (gnus-group-method gnus-newsgroup-name))))
    (nnreddit-summary-mode)))

(defun nnreddit-group-mode-activate ()
  "Augment the gnus-group-mode-map unconditionally."
  (setq gnus-group-change-level-function 'nnreddit-update-subscription)
  (nnreddit-group-mode))

(add-hook 'gnus-article-mode-hook 'nnreddit-article-mode-activate)
(add-hook 'gnus-group-mode-hook 'nnreddit-group-mode-activate)
(add-hook 'gnus-summary-mode-hook 'nnreddit-summary-mode-activate)

;; `gnus-newsgroup-p' requires valid method post-mail to return t
(add-to-list 'gnus-valid-select-methods '("nnreddit" post-mail) t)

(add-function
 :around (symbol-function 'message-send-news)
 (lambda (f &rest args)
   (let* ((dont-ask (lambda (prompt)
                      (when (cl-search "mpty article" prompt) t)))
          (link-p (not (null (message-fetch-field "Link"))))
          (message-shoot-gnksa-feet (if link-p t message-shoot-gnksa-feet)))
     (condition-case err
         (progn
           (when link-p
             (add-function :before-until (symbol-function 'y-or-n-p) dont-ask))
           (apply f args)
           (remove-function (symbol-function 'y-or-n-p) dont-ask))
       (error (remove-function (symbol-function 'y-or-n-p) dont-ask)
              (error (error-message-string err)))))))

(add-function
 :around (symbol-function 'gnus-summary-post-news)
 (lambda (f &rest args)
   (let* ((nnreddit-post-type (read-char-choice "[l]ink / [t]ext: " '(?l ?t)))
          (link-header (apply-partially #'message-add-header "Link: http://"))
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
              (error (error-message-string err)))))))

(add-function
 :filter-return (symbol-function 'message-make-fqdn)
 (lambda (val)
   (if (and (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
            (cl-search "--so-tickle-me" val))
       "reddit.com" val)))

(add-function
 :before-until (symbol-function 'message-make-from)
 (lambda (&rest args)
   (when (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnreddit)
     (concat (nnreddit-rpc-call nil nil "user_attr" "name") "@reddit.com"))))

(provide 'nnreddit)
