# Changelog
## 1.1.0 (2025-06-03)
- Add keybinding to remove a comment from issue detail view
- Add help transient in Issue Detail mode, press '?' to open it

## 1.0.3 (2025-05-26)
- Fix another formatting issue in detail view

## 1.0.2 (2025-05-26)
- Fix formatting issues with progress fields

## 1.0.1 (2025-05-26)
- Fix formatting issues in `jira-tempo`

## 1.0.0 (2025-05-23)
- **Breaking Change**: `jira-statuses-done`, `jira-statuses-progress`,
`jira-statuses-todo`, `jira-statuses-error` doesn't work any more.

	- Replaced status lists by status categories to **group statuses** in
      `Todo`, `In progress` and `Done`, improving the maintainability of the
      config. Each category have an associated face.

    - Added possibility to custom faces based on status names, allowing to
      override status categories.

## 0.11.0 (2025-05-19)
- Display more kinds of markup in comments and descriptions: numeric labels for
  orderedLists, indentation for nested lists, user mentions, emoji, calendar
  dates, italic, underline, strike-through, inline code, super/subscript,
  foreground color, adapting to dark backgrounds


## 0.10.0 (2025-05-17):
- Write comments in a new buffer

## 0.9.1 (2025-04-17)
- Add logging messages to find problems with API auth

## 0.9.0 (2025-04-17)
- Display times in locale's preferred format
- Add defcustom to display comments in chronological order

## 0.8.0 (2025-04-09):
- Add support for attachments (thank you @danielcmccarthy!)

## 0.7.0 (2025-03-29):
- Add comments from jira detail view

## 0.6.0 (2025-03-28)
- Add keybinding in issue to jump to tempo and the other way around.
- Close the jira issues buffer before I start the jira-tempo one, to avoid issues
- Avoid deleting other windows when opening jira-issues

## 0.5.0 (2025-03-24)
- Add support for bold text and code in docs
- Show comments in issue detail view

## 0.4.0 (2025-03-23)
- Add `jira-issues-table-fields` so that users can select the fields
  to be visualized in the table
- `--current-sprint` is not set by default any more when retrieving
  issues

## 0.3.0 (2024-03-21)
- Add support for Jira Personal Access Token (PAT)

## 0.2.2 (2025-03-20)
- Convert windows line endings to unix ones in issues detail

## 0.2.1 (2025-03-20)
- Add constant with package version: `jira-version`

## 0.2.0 (2025-03-19)
- Add support for authentication with `auth-source`

## 0.1.1 (2025-03-18)
- Fix bug for missing fields in issue detail view

## 0.1.0 (2025-03-18)
- Allow configuring Jira REST API Version

## 0.0.2 (2025-03-17)
- Log requests input data (if `jira-debug`)
- Provide date suggestions while adding a worklog

## 0.0.1 (2025-03-16)
- First working version
