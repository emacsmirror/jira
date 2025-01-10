;;; jira-utils.el --- Utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Mixed utility functions for jira.el

;;; Code:

(require 'tablist)

(defun jira-utils-marked-item ()
  "Return the marked item in the current tablist"
  (car (car (tablist-get-marked-items))))

(defun jira-utils-transient-description (prefix &optional item)
  "Return a transient description for the given ITEM (or the marked one)
with the given PREFIX"
  (let ((item (or item (jira-utils-marked-item))))
    (if (stringp item) (concat prefix " [" (format "%s" item) "]") prefix)))

(defun jira-utils-get-day-of-week (date-string)
  "Return the day of the week for a given DATE-STRING in the format YYYY-MM-DD"
  (let* ((parsed-time (parse-time-string (concat date-string "T00:00:00")))
         (time (encode-time (nth 0 parsed-time)  ; second
                            (nth 1 parsed-time)  ; minute
                            (nth 2 parsed-time)  ; hour
                            (nth 3 parsed-time)  ; day
                            (nth 4 parsed-time)  ; month
                            (nth 5 parsed-time)))) ; year
    (format-time-string "%a" time)))

(defun jira-utils-week-start-and-end (date)
  "Return the start and end dates of the given DATE week, in YYYY-MM-DD format"
  (let* ((decoded-time (decode-time date))
         (current-day (nth 6 decoded-time))
         (days-from-monday (mod (- current-day 1) 7))
         (start-of-week (time-subtract date (days-to-time days-from-monday)))
         (end-of-week (time-add start-of-week (days-to-time 6)))
         (format-time (lambda (time)
                        (format-time-string "%Y-%m-%d" time))))
    (list (funcall format-time start-of-week)
          (funcall format-time end-of-week))))

(provide 'jira-utils)

;;; jira-utils.el ends here
