;;; jira-users.el --- Jira user database             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dan McCarthy

;; Author: Dan McCarthy <daniel.c.mccarthy@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'jira-api)

;; jira-users hash table is retrieved within jira-api

(defun jira-users-read-user (prompt &optional watchers)
  "Complete a Jira username with PROMPT.

Returns a list: (NAME ACCOUNT-ID). WATCHERS is an optional list of
display names to complete from; the default is all names in
`jira-users'."
  (let* ((name (completing-read prompt (or watchers jira-users)))
         (account-id (gethash name jira-users)))
    (list name account-id)))

(provide 'jira-users)

;;; jira-users.el ends here
