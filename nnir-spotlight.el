;;; nnir-spotlight.el --- nnir interface for Spotlight  -*- lexical-binding: t; -*-

;; Filename: nnir-spotlight.el
;; Description: nnir interface for Spotlight
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2014-02-01
;; Version: 1.140223
;; Keywords: mail
;; Human-Keywords: gnus nnir
;; URL: https://github.com/kawabata/nnir-spotlight

;;; Commentary:
;;
;; -*- mode: org -*-
;;
;; * Spotlight Interface for Gnus nnir Method.
;;
;; This file provides a Gnus nnir interface for MacOS Spotlight.
;; As of it, you must use MacOS X and Spotlight to be enabled.
;; You can use this search engine with `nnmh', `nnml', and `nnmaildir'.
;;
;; ** Emacs Setup
;;
;; In .emacs, you should set as the following. (`gnus-select-method' is
;; used in the following examples, but you can specify it in
;; `gnus-secondary-select-methods`, too.)
;;
;; First of all, you should choose which method to use Spotlight.
;;
;; : (require 'nnir-est)
;; : (setq nnir-method-default-engines
;; :        '((nnmaildir . spotlight)
;; :          (nnml . spotlight)
;; :          (nntp . gmane)))
;;
;; If you are using `nnml', specifying mail directory may be sufficient.
;;
;; : ;; nnml/nnmh
;; : (setq gnus-select-method '(nnml ""))
;; : (setq nnir-spotlight-prefix "/home/john/Mail/")
;;
;; or
;;
;; : (setq gnus-select-method '(nnml "" (nnir-spotlight-prefix "/home/john/Mail/")))
;;
;; or
;;
;; : ;; nnmaildir
;; : (setq gnus-select-method '(nnmaildir "" (directory "~/Maildir")))
;;
;; If `directory' attribute is specified, it will be used for prefix.
;; Otherwise, `nnir-spotlight-prefix' will be used.
;;
;; * Query Format
;;
;; In `gnus-group-make-nnir-group' query, you can specify following
;; attributes in addition to query word.
;;
;; - kMDItemTitle== :: Mail Subject
;; - kMDItemAuthorEmailAddresses== :: Email author addresses
;; - kMDItemAuthors== :: Email author
;; - kMDItemRecipientEmailAddresses== :: Email recipients addresses
;; - kMDItemRecipients== :: Email recipients

;;; Code:

(require 'nnir)

(defgroup nnir-spotlight nil
  "nnir interface for Spotlight."
  :group 'nnir)

(defcustom nnir-spotlight-program "mdfind"
  "*Name of Spotlight search executable."
  :type '(string)
  :group 'nnir-spotlight)

(defcustom nnir-spotlight-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by Spotlight.
in order to get a group name (albeit with / instead of .).

For example, suppose that Spotlight returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-spotlight-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory)
  :group 'nnir-spotlight)

;; Spotlight interface
(defun nnir-run-spotlight (query server &optional _group)
  "Run given QUERY against Spotlight for SERVER.
Returns a vector of (_GROUP name, file name)
pairs (also vectors, actually)."
  (save-excursion
    (let* ((article-pattern (if (string-match "\\`nnmaildir:"
                                              (gnus-group-server server))
                                ":[0-9]+"
                              "^[0-9]+$"))
           artlist
           (qstring (cdr (assq 'query query)))
           (directory (nnir-read-server-parm 'directory server))
           (prefix
            (if directory (file-name-as-directory (expand-file-name directory))
              (nnir-read-server-parm 'nnir-spotlight-prefix server)))
           score group article
           (process-environment (copy-sequence process-environment))
           )
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (let* ((cp-list
              `( ,nnir-spotlight-program
                 nil			; input from /dev/null
                 t			; output
                 nil			; don't redisplay
                 "-onlyin" ,prefix      ; specify directory
                 ,@(nnir-read-server-parm 'nnir-spotlight-additional-switches server)
                 ,qstring
                 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-spotlight-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run Spotlight: %s" exitstatus)
          ;; Spotlight failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))
      (goto-char (point-min))
      ;; debug
      ;; (message "buffer=%s" (buffer-string))
      (while (re-search-forward
              "^\\(/[^ ]+?\\)$" nil t)
        (setq score "1"
              group (file-name-directory (match-string-no-properties 1))
              article (file-name-nondirectory (match-string-no-properties 1)))
        ;; debug
        ;; (message "score=%s,group=%s,article=%s" score group article)
        ;; make sure article and group is sane
        (when (and (string-match article-pattern article)
                   (not (null group)))
	  (nnir-add-result group article score prefix server artlist)))
      ;; debug
      (message "prefix=%s,server=%s,artlist=%s" prefix server artlist)
      ;; sort artlist by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

(add-to-list 'nnir-engines '(spotlight nnir-run-spotlight nil))

(provide 'nnir-spotlight)

;;; nnir-spotlight.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
