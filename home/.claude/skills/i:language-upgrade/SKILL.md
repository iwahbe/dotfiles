---
name: language-upgrade
description: Check out a renovate/dependency-upgrade PR, drive it to green, and monitor it until it merges.
argument-hint: "[pr-number-or-url]"
disable-model-invocation: true
allowed-tools: Bash(${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh *)
---

Drive the language-upgrade PR $ARGUMENTS in !`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh repo` to merge.

## 1. Find and check out the PR

If $ARGUMENTS names a PR, use it. Otherwise list open PRs and pick the
dependency-upgrade one (`gh pr list --json number,title,author,headRefName`;
look for the renovate bot or a `renovate/` head branch). If there's more than
one candidate, ask me which.

Check it out **in the current repo, no worktree**:

```sh
gh pr checkout <n>
git reset --hard origin/<branch>
```

The `reset` is required: a local branch of the same name may already exist and
be stale, and you must start from exactly what the PR contains. Assign the PR
to me if it's unassigned.

The upgrade is pinned here:

!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh pinned-dep`

## 2. Diagnose CI

Read the PR's checks before touching anything — the failures are the work
list. Use the `analyze-ci-failures` skill or `gh run view --log-failed`.

!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh ci-jobs`

Separate failures into three buckets and treat each differently:

- **Build/API breaks** — upstream renamed or changed a signature. Adapt our
  code. Mechanical.
- **New conformance tests** — tests that exist in the new upstream release but
  not the old one. These go in `expectedFailures` (§3).
- **Regressions** — a test that passed on the base branch and fails on the
  upgrade. See §4. These are *not* `expectedFailures` material.

To tell "new test" from "regression": check whether the test name exists at
the previously pinned version — `git log`/`git diff` the pinned dependency, or
diff the upstream test list between the two versions. Do not classify by
vibes; a regression mislabelled as a new test silently ships a bug.

## 3. expectedFailures — accept snapshots FIRST

`expectedFailures` lives in
!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh language-test-path`,
with snapshots under
!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh snapshot-dir`.

Before adding *anything* to that map, run every failing test in accept mode. A
large fraction of "failures" on an upgrade are stale snapshots, and accepting
them is the real fix.

1. Accept: !`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh accept-cmd`
2. Only what still fails gets an entry. Give each one a reason that is
   actually useful: the real error (`compilation error: cannot find symbol X`)
   plus `(added in <version>)`, not "test failing".
3. Failing tests do not include their snapshot data in the PR. Remove the
   snapshots of tests added to `expectedFailures`.
4. Review the snapshot diff before committing. An unexpectedly large or
   suspicious diff means read it properly — accepting a broken snapshot is
   worse than a red test.

Codegen golden files are separate:
!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh codegen-tests`

Gotchas that will waste your time otherwise:
!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh gotchas`

## 4. Regressions reject the upgrade

If the upgrade breaks something that used to work, and the fix isn't a small
correct change on our side: **stop**. Do not paper over it with an
`expectedFailures` entry. Report to me what regressed, which upstream change
caused it, and what the options are. That's a conversation, not a commit.

## 5. Push gating

- **Mechanical changes** — accepted snapshots, `expectedFailures` additions,
  API renames, `go mod tidy` — push without asking.
- **Non-trivial changes** — anything requiring real logic in the codegen,
  runtime, or plugin — show me the diff and wait for approval before pushing.

Verify before you push: at minimum
!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh lint-cmd`
and the tests you touched. Don't run the full suite locally; CI does that.

If the change is user-facing it needs a changelog entry, otherwise the
`impact/no-changelog-required` label:
!`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh changelog-cmd`

No hacks. If something can't be done correctly, stop and say so.

## 6. Monitor until merge

After each push, use the `watch-pr` skill to block until CI resolves. Do not
write your own polling loop — it will misread the gap between pushing and
checks registering, and it will spin forever when CI never starts because the
branch needs a rebase.

When it reports a failure:

- **Flake** — unrelated to the diff, passes on retry, or a known-flaky job:
  re-run it (`gh run rerun --failed`). Say out loud that you're calling it a
  flake and why.
- **Genuine failure** — fix it in a *follow-up commit on the same PR*. Never
  force-push over the history.

Keep the loop going until the PR is green and merged, or until you hit
something in §4 or §5 that needs me.

---

The repo-specific paths, commands and gotchas above are maintained by hand and
may have gone stale. **If any of them is wrong** — a path that moved, a command
that no longer works, a CI job that was renamed — tell me what you found and
what the correct value is, and ask whether to correct it. Don't work around a
stale reference silently; a wrong reference here costs every future run. You do
not need to know where these values come from to report one.
