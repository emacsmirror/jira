;;; jira-table.el --- Manage tables data  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Handle data to show in tables

;;; Code:

(require 'jira-api)

(defun jira-table-field-info (all-fields field)
  "Get FIELD info from ALL-FIELDS"
  (alist-get field all-fields))

(defun jira-table-field-path (all-fields field)
  "Get FIELD path from ALL-FIELDS"
  (alist-get :path (jira-table-field-info all-fields field)))

(defun jira-table-field-columns (all-fields field)
  "Get FIELD columns from ALL-FIELDS"
  (alist-get :columns (jira-table-field-info all-fields field)))

(defun jira-table-field-name (all-fields field)
  "Get FIELD name from ALL-FIELDS"
  (alist-get :name (jira-table-field-info all-fields field)))

(defun jira-table-field-formatter (all-fields field)
  "Get FIELD formatter from ALL-FIELDS"
  (alist-get :formatter (jira-table-field-info all-fields field)))

(defun jira-table-field-parent (all-fields field)
  "Get FIELD parent from ALL-FIELDS"
  (car (cdr (jira-table-field-path all-fields field))))

(defun jira-table-extract-field (all-fields field issue-data)
  "Extract FIELD value from ISSUE-DATA alist"
  (let ((field-path (jira-table-field-path all-fields field)))
    (if (listp field-path)
        (let ((result issue-data))
          (dolist (path field-path result)
            (setq result
                  (alist-get
                   (if (listp path) ;; custom fields
                       (intern (cdr (assoc (car (cdr path)) jira-fields)))
                     path)
                   result)))
          result)
      (alist-get field-path issue-data))))


(provide 'jira-table)

;;; jira-table.el ends here
