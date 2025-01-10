;;; jira-doc.el --- Jira Doc  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Manage Altassian Document Format
;; See https://developer.atlassian.com/cloud/jira/platform/apis/document/
;; Not all the kinds of blocks are supported yet, only the most common ones.

;;; Code:

;; these blocks contain the content property
(defconst jira-doc--top-level-blocks
  '("blockquote" "bulletList" "codeBlock" "expand" "heading" "mediaGroup"
    "mediaSingle" "orderedList" "panel" "paragraph" "rule" "table"
    "multiBodiedExtension"))

;; these blocks contain the content property
(defconst jira-doc--child-blocks
  '("listItem" "media" "nestedExpand" "tableCell" "tableHeader"
    "tableRow" "extensionFrame"))

(defconst jira-doc--inline-blocks
  '("date" "emoji" "hardBreak" "inlineCard" "mention" "status"
    "text" "mediaInline"))

(defun jira-doc--list-to-str (items sep)
  (mapconcat #'identity items sep))

(defun jira-doc--format-inline-block(block)
  (let ((type (alist-get 'type block))
        (text (alist-get 'text block)))
    (cond ((string= type "hardBreak") "\n")
          ((string= type "inlineCard")
           (let* ((url (alist-get 'url (alist-get 'attrs block))))
             (buttonize url `(lambda (data) (interactive) (browse-url ,url)))))
          (text (format "%s " text)))))

(defun jira-doc--format-content-block(block)
  (let* ((type (alist-get 'type block))
         (sep (if (string= type "paragraph") "" "\n"))
         (prefix (if (string= type "listItem") " - " "")))
    (cond
     ((string= type "table")
      "\n<TABLES NOT SUPPORTED BY jira.el>\n")
     (t (concat prefix
                (jira-doc--list-to-str
                 (mapcar (lambda (b) (jira-doc--format-block b))
                         (alist-get 'content block))
                 sep))))))

(defun jira-doc--format-block(block)
  (let ((type (alist-get 'type block)))
    (if (or (member type jira-doc--top-level-blocks)
            (member type jira-doc--child-blocks))
        (jira-doc--format-content-block block)
      (jira-doc--format-inline-block block))))

(defun jira-doc-format (doc)
  "Format DOC in Jira Document Format to a string"
  (let* ((content (alist-get 'content doc)))
    (jira-doc--list-to-str
     (mapcar (lambda (block) (jira-doc--format-block block)) content)
     "\n")))


(provide 'jira-doc)
