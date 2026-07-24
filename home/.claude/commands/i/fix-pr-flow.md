---
description: Fix the failing test added in the current PR, get approval, then commit, update the PR, and monitor CI.
disable-model-invocation: true
allowed-tools: Bash(gh pr view *) Bash(gh pr edit *)
---

Fix the test added in this PR. Fix the issue & after I approve of the
change, commit your changes to the existing PR and edit the PR title &
body to reflect the changes, and mark the PR as ready for review. Use ponytail-review to review your changes
before claiming to be complete. After you finish, consider what you learned and see if there is a way to
simplify the code's implementation in light of the new information.

!`a=$(gh pr view --json assignees -q '.assignees[].login' 2>/dev/null) && { [ -n "$a" ] || gh pr edit --add-assignee @me >/dev/null 2>&1; } || echo 'If the PR is not currently assigned, assign it to @me via gh.'`

Don't run broad tests locally, only the tests that you believe are
needed. CI runs the full test suite. Every time you push a change,
monitor the PR and fix issues you introduced. Flag flakes and retry.

Do not write hacks, if a system cannot be implemented correctly, it's better to stop then
hack in garbage.

If the change is user facing, it should have a changelog entry. If not, add the
`impact/no-changelog-required` label.
