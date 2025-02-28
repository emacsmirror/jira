# jira.el

Emacs integration for [Altassian's Jira](https://www.atlassian.com/software/jira).

> If you have no choice but to use Jira, at least do it without leaving Emacs.

Supports listing and filtering issues, viewing issue details,
modifying certain properties, and adding worklogs.

Additionally, it provides support for displaying all worklogs from the
[Tempo](https://www.tempo.io/products/jira-time-tracking) integration.

## Screenshots

- List issues

![List issues](doc/list-issues.png)

- Filter issues

![Filter issues](doc/list-issues-filter.png)

- Change selected issue

![Change issue](doc/change-issue.png)

- List [Tempo](https://www.tempo.io/) worklogs

![List Worklogs](doc/list-worklogs.png)


## Installation
This package is not yet available in any official repository (though it's in progress).
You can install it using [straight.el](https://github.com/radian-software/straight.el)

```elisp
(use-package jira
  :straight (:host github :repo "unmonoqueteclea/jira.el")
  :demand t
  :config
  (setq jira-username "johndoe@acme.com") ;; Jira username (usually, an email)
  (setq jira-base-url "https://acme.atlassian.net") ;; Jira instance URL
  ;; API token for JIRA
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  (setq jira-token "foobar123123")
  ;; (Optional) API token for JIRA TEMPO plugin
  ;; See https://apidocs.tempo.io/
  (setq jira-tempo-token "foobar123123"))
```

## Quickstart

Use `M-x jira-issues` to check the list of issues assigned to the
configured user for the active sprint. Once the list is loaded, press
`?` to see the available actions. You can modify the filters, update
the selected issue, add worklogs, and more.

Press `l` to select the filters to be applied to the list of
issues. You can filter by several fields or even write your own `JQL`
filter.

- Thanks to
[transient](https://magit.vc/manual/transient/Saving-Values.html#Saving-Values),
all the transients arguments can be set temporarily or
permanently. See [transient
docs](https://magit.vc/manual/transient/Saving-Values.html#Saving-Values)
for more information.

- Check out [tablist docs](https://github.com/politza/tablist) for additional
keybindings to manage the table (sorting, filtering, exporting, etc)

If you configured [Tempo integration](https://www.tempo.io/), you can
also run `jira-tempo` to view the list of worklogs for the current
week.

## Customization

This is the list of customizations you can set:

- `jira-username`: **Mandatory**: Jira username (usually, an email)
- `jira-base-url`: **Mandatory**: Jira instance URL, like: https://acme.atlassian.net
- `jira-token`: **Mandatory**: Jira REST API token
- `jira-tempo-token`: Jira [tempo.io](https://www.tempo.io/) API token
- `jira-debug`: Whether to log jira.el internal processes data, including API responses
- `jira-issues-max-results`: Maximum number of Jira issues to retrieve
- `jira-tempo-max-results`: Maximum number of Tempo worklogs to retrieve
- `jira-statuses-done`: A list of statuses names representing done state
- `jira-statuses-progress`: A list of statuses names representing progress state
- `jira-statuses-todo`: A list of statuses names representing TODO
- `jira-statuses-error`: A list of statuses names representing problems
