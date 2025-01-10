;;; jira-issues.el --- Jira Issues  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; List and manage Jira issues

;;; Code:

(require 'tablist)
(require 'transient)

(require 'jira-api)
(require 'jira-table)
(require 'jira-fmt)
(require 'jira-utils)
(require 'jira-tempo)
(require 'jira-actions)
(require 'jira-detail)

(defvar jira-issues-fields
  '((:key . ((:path . (key))
             (:columns . 10)
             (:name . "Key")
             (:formatter . jira-fmt-issue-key)))
    (:priority-name . ((:path . (fields priority name))
                       (:columns . 10)
                       (:name . "Priority")))
    (:priority-icon.  ((:path . (fields priority iconUrl))
                       (:columns . 10)
                       (:name . "Priority")))
    (:labels . ((:path . (fields labels))
                (:columns . 10)
                (:name . "Labels")))
    (:original-estimate . ((:path . (fields aggregatetimeoriginalestimate))
                           (:columns . 10)
                           (:name . "Estimate")
                           (:formatter . jira-fmt-time-from-secs)))
    (:work-ratio . ((:path . (fields workratio))
                    (:columns . 6)
                    (:name . "WR")
                    (:formatter . jira-fmt-issue-progress)))
    (:remaining-time . ((:path . (fields timeestimate))
                        (:columns . 10)
                        (:name . "Remaining")
                        (:formatter . jira-fmt-time-from-secs)))
    (:assignee-name . ((:path . (fields assignee displayName))
                       (:columns . 14)
                       (:name . "Assignee")))
    (:reporter-name . ((:path . (fields reporter displayName))
                       (:columns . 14)
                       (:name . "Reporter")))
    (:components . ((:path . (fields components))
                    (:columns . 10)
                    (:name . "Components")
                    (:formatter . jira-fmt-issue-components)))
    (:fix-versions . ((:path . (fields fixVersions))
                      (:columns . 10)
                      (:name . "Fix Versions")
                      (:formatter . jira-fmt-issue-fix-versions)))
    (:status-name . ((:path . (fields status name))
                     (:columns . 15)
                     (:name . "Status")
                     (:formatter . jira-fmt-issue-status)))
    (:status-category-name . ((:path . (fields status statusCategory name))
                              (:columns . 10)
                              (:name . "Status Category")))
    (:creator-name . ((:path (fields creator  displayName))
                      (:columns . 10)
                      (:name . "Creator")))
    (:progress-percent . ((:path . (fields progress  percent))
                          (:columns . 10)
                          (:name . "Progress")
                          (:formatter . jira-fmt-issue-progress)))
    (:issue-type-name . ((:path . (fields issuetype name))
                         (:columns . 15)
                         (:name . "Type")
                         (:formatter . jira-fmt-issue-type-name)))
    (:issue-type-icon . ((:path . (fields issuetype iconUrl))
                         (:columns .  10)
                         (:name . "Type")))
    (:project-key . ((:path . (fields project key))
                     (:columns . 10)
                     (:name . "Project")))
    (:project-name .  ((:path . (fields project name))
                       (:columns . 10)
                       (:name . "Project")))
    (:parent-type-name . ((:path . (fields parent fields issuetype name))
                          (:columns . 10)
                          (:name . "Parent Type")
                          (:formatter . jira-fmt-issue-type-name)))
    (:parent-status . ((:path . (fields parent fields status name))
                          (:columns . 10)
                          (:name . "Parent Status")
                          (:formatter . jira-fmt-issue-status)))
    (:parent-key . ((:path . (fields parent key))
                    (:columns . 10)
                    (:name . "Parent Key")
                    (:formatter . jira-fmt-issue-key)))
    (:created . ((:path . (fields created))
                 (:columns . 10)
                 (:name . "Created")))
    (:updated . ((:path . (fields updated))
                 (:columns . 10)
                 (:name . "Updated")))
    (:description . ((:path . (fields description))
                     (:columns . 10)
                     (:name . "Description")))
    (:summary . ((:path . (fields summary))
                 (:columns . 10)
                 (:name . "Summary")))
    (:due-date . ((:path . (fields duedate))
                 (:columns . 10)
                 (:name . "Due Date")
                 (:formatter . jira-fmt-date)))
    (:sprints . ((:path . (fields (custom "Sprint")))
                 (:columns . 10)
                 (:name . "Sprints")
                 (:formatter . jira-fmt-issue-sprints)))
    (:line . ((:path . (fields (custom "Business line")))
              (:columns . 10)
              (:name . "Business Line")
              (:formatter . jira-fmt-business-line)))
    (:cost-center . ((:path . (fields (custom "Cost center")))
                     (:columns . 10)
                     (:name . "Const Center")
                     (:formatter . jira-fmt-cost-center)))
    (:resolution . ((:path . (fields resolution name))
                    (:columns . 10)
                    (:name . "Resolution")))))

;; TODO: It would be nice to make it a defcustom but, currently, we
;; need it to always show the key at the beginning and the summary at
;; the end of the list, so we can extract it when we create a worklog
(defvar jira-issues-table-fields
  '(:key :issue-type-name :status-name :assignee-name
         :progress-percent :work-ratio :remaining-time :summary))

(defcustom jira-issues-max-results 50
  "Maximum number of Jira issues to retrieve"
  :group 'jira :type 'integer)

(defun jira-issues--api-get-issues (jql callback)
  (let* ((parent (lambda (fd) (jira-table-field-parent jira-issues-fields fd)))
         (fields (mapcar parent jira-issues-table-fields)))
    (when jira-debug
        (message (concat "Get issues with jql "jql)))
    (jira-api-call
     "GET" "search"
     :params `(("jql" . ,jql)
               ("maxResults" . ,jira-issues-max-results)
               ("startAt" . 0)
               ("fields" . ,(mapconcat (lambda (x) (format "%s" x))
                                       (cl-remove nil fields) ",")))
     :callback callback)))

(defun jira-issues--api-filters-and (filters)
  (mapconcat (lambda (filter) filter) (remove nil filters) " AND "))

(defun jira-issues--data-format-cell (issue field)
  (let* ((extracted (jira-table-extract-field jira-issues-fields field issue))
         (value (format "%s" (or extracted "")))
         (formatter (jira-table-field-formatter jira-issues-fields field)))
    (if formatter (funcall formatter value) value)))

(defun jira-issues--data-format-issue (issue)
  (let ((key (jira-table-extract-field jira-issues-fields :key issue))
        (formatter (lambda (fd) (jira-issues--data-format-cell issue fd))))
    (list key (vconcat (mapcar formatter jira-issues-table-fields)))))

(defun jira-issues--refresh-table (data response)
  (let* ((data-alist (json-read-from-string (json-encode data)))
         (issues (alist-get 'issues data-alist))
         (entries (mapcar #'jira-issues--data-format-issue issues)))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))

(defun jira-issues--refresh ()
  (let* ((args (transient-args 'jira-issues-menu))
         (myself (transient-arg-value "--myself" args))
         (current-sprint (transient-arg-value "--current-sprint" args))
         (jql (transient-arg-value "--jql=" args))
         (status (transient-arg-value "--status=" args))
         (project (transient-arg-value "--project=" args))
         (resolution (transient-arg-value "--resolution=" args))
         (version (transient-arg-value "--version=" args)))
    (jira-issues--api-get-issues
     (or jql
         (jira-issues--api-filters-and
          (list (when myself "assignee = currentUser()")
                (when current-sprint "sprint in openSprints()")
                (when status (concat "status = \"" status "\""))
                (when project (concat "project = \"" project "\""))
                (when resolution (concat "resolution = \"" resolution "\""))
                (when version (concat "fixversion = \"" version "\"")))))
     #'jira-issues--refresh-table)))

(defun jira-issues--filter-invalid-if-jql ()
  (if transient-current-command
      (transient-arg-value "--jql=" (transient-args transient-current-command))))

(transient-define-prefix jira-issues-menu ()
  "Show menu for listing Jira Issues"
   :refresh-suffixes t
   :value '("--myself" "--current-sprint")
   [["Arguments"
    ("m" "Just from myself" "--myself"
     :transient t
     :inapt-if jira-issues--filter-invalid-if-jql)
    ("c" "Just from current sprint" "--current-sprint"
     :transient t
     :inapt-if jira-issues--filter-invalid-if-jql)
    ("s" "Status" "--status="
     :transient t
     :inapt-if jira-issues--filter-invalid-if-jql
     :choices
     (lambda () (mapcar (lambda (t) (car t)) jira-statuses)))
    ("p" "Project" "--project="
     :transient t
     :inapt-if jira-issues--filter-invalid-if-jql
     :choices
     (lambda () (mapcar (lambda (t) (car t)) jira-projects)))
    ("r" "Resolution" "--resolution="
     :transient t
     :inapt-if jira-issues--filter-invalid-if-jql
     :choices
     (lambda () (mapcar (lambda (t) (car t)) jira-resolutions)))
    ("j""JQL filter" "--jql="
     :transient transient--do-call)
    ("v" "Fix Versions" "--version="
     :transient t
     :inapt-if jira-issues--filter-invalid-if-jql
     :choices
     (lambda () (apply #'append (mapcar #'cdr jira-projects-versions))))]
   ["Arguments Help"
    ("C-x" :info (concat (propertize "Check " 'face 'italic)
                         "additional options"))
    ("C-x s" :info (concat (propertize "Set" 'face 'italic)
                           " current arguments for Emacs session"))
    ("C-x C-s" :info (concat (propertize "Persist" 'face 'italic)
                             " current arguments for all Emacs sessions"))
    ("C-x C-k" :info (concat (propertize "Reset" 'face 'italic)
                           " arguments to default ones"))]]

  ["Actions"
   ("l" "List Jira Issues" tablist-revert)])

(transient-define-prefix jira-issues-actions-menu ()
  "Show menu for actions on Jira Issues"
  [[:description "Jira Issues List"
                ("?" "Show this menu" jira-issues-actions-menu)
                ("g" "Refresh list" tablist-revert)
                ("l" "List Jira Issues menu" jira-issues-menu)]]
  [[:description
    (lambda () (jira-utils-transient-description "Actions on issue"))
     :inapt-if-not jira-utils-marked-item
    ("C" "Change issue" jira-actions-change-issue-menu)
    ("I" "Show issue information"
     (lambda () (interactive) (jira-detail-show-issue (jira-utils-marked-item))))
    ("O" "Open issue in browser"
     (lambda () (interactive) (jira-actions-open-issue (jira-utils-marked-item))))
    ("W" "Add worklog to issue" jira-actions-add-worklog-menu)]])

(defvar jira-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'jira-issues-actions-menu)
    (define-key map "l" 'jira-issues-menu)
    (define-key map "C" 'jira-actions-change-issue-menu)
    (define-key map "I" (lambda () (interactive)
                          (jira-detail-show-issue (jira-utils-marked-item))))
    (define-key map "O" (lambda () (interactive)
                          (jira-actions-open-issue (jira-utils-marked-item))))
    (define-key map "W" 'jira-actions-add-worklog-menu)
    map)
  "Keymap for `jira-issues-mode'.")

;;;###autoload (autoload 'jira-issues "jira-issues" nil t)
(defun jira-issues ()
  "List Jira issues"
  (interactive)
  (pop-to-buffer "*Jira Issues*")
  (delete-other-windows)
  (jira-issues-mode)
  (jira-api-get-basic-data)
  (tablist-revert))

(define-derived-mode jira-issues-mode tabulated-list-mode "Jira Issues"
  "Major mode for listing Jira issues"
  :interactive nil
  (let* ((name (lambda (fd) (jira-table-field-name jira-issues-fields fd)))
         (columns (lambda (fd) (jira-table-field-columns jira-issues-fields fd)))
         (col-info (lambda (fd) (list (funcall name fd) (funcall columns fd) t))))
    (setq tabulated-list-format
          (vconcat (mapcar col-info jira-issues-table-fields))))

  (setq tabulated-list-sort-key (cons "Status" nil))
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'jira-issues--refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(add-hook 'jira-issues-changed-hook 'tablist-revert)

(provide 'jira-issues)

;;; jira-issues.el ends here
