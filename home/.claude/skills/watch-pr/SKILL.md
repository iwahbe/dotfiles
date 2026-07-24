---
name: watch-pr
description: Block until a GitHub PR's CI finishes, reporting failures as they occur. Use whenever you have pushed to a PR and need to know the outcome — instead of writing your own polling loop with sleep, gh pr checks, or a for/seq retry loop.
allowed-tools: Bash(${CLAUDE_SKILL_DIR}/watch-pr.sh *)
---

Do not hand-roll a CI polling loop. Run this and wait for it to return:

```sh
${CLAUDE_SKILL_DIR}/watch-pr.sh <pr-number>
```

It blocks until the CI outcome is decided and then exits. Every check is
decisive by default. `--required` narrows to required checks, but note that
many repos (pulumi-java among them) mark none as required, in which case it
says so and watches everything anyway.

## Interpreting the exit code

| Exit | Meaning | What to do |
| --- | --- | --- |
| 0 | All watched checks passed. | Done. |
| 1 | A check failed. Names and log links are printed. | Diagnose and fix, push, run this again. |
| 3 | Blocked — CI cannot progress on its own. | Read the message; it says whether the branch is conflicted, behind, or CI never started. Act, then run this again. |
| 4 | Usage or environment error. | Fix the invocation. |

Exit 1 arrives on the *first* failure rather than after the whole matrix
finishes, so start diagnosing as soon as it returns. Re-running after a push
resumes watching, so the fix/push/watch cycle is just calling it again.

## Why not a polling loop

A hand-written loop gets this wrong in ways that waste a lot of wall-clock:

- Right after a push, `gh pr checks` exits non-zero with "no checks reported"
  because checks have not registered yet. A naive loop reads that as failure.
- If CI never starts — most often because the branch has conflicts or is behind
  its base — nothing ever becomes non-pending and the loop spins until timeout.
  This script bounds the wait and then reports `mergeStateStatus` (`DIRTY`,
  `BEHIND`) so you fix the real problem instead of waiting.
- `gh pr checks --watch` alone still exits early on the no-checks-yet case.

## Shell gotcha

The interactive shell here is **zsh**, where `status` is a readonly builtin
parameter. `status=$(...)` fails with "read-only variable". Use any other name
(`st`, `result`) in shell one-liners.
