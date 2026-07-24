# i:language-upgrade — maintainer notes

Not loaded at invocation. Only `SKILL.md` is injected into the conversation;
these notes are for a human or agent that has been asked to *edit* the skill.

## Layout

| File | Role |
| --- | --- |
| `SKILL.md` | The prompt. Invoked as `/i:language-upgrade [pr]`. |
| `language-upgrade-ref.sh` | Resolves one repo-scoped reference value. |
| `language-upgrade-refs.json` | The reference data, keyed by `origin` remote. |

The directory name — colon included — is what determines the invocation name.
Renaming the directory renames the command.

## How the references work

`SKILL.md` contains `` !`${CLAUDE_SKILL_DIR}/language-upgrade-ref.sh <key>` ``
in place of every repo-specific path or command. Claude Code runs those before
the skill text reaches the model, so the model only ever sees resolved values
and never learns there is a lookup at all. That is deliberate: the agent should
be reading the *content*, not reverse-engineering the skill.

`language-upgrade-ref.sh` derives `owner/repo` from `git remote get-url origin`
and indexes into `language-upgrade-refs.json`. It always exits 0 — a missing
repo or key is reported as readable text so a broken lookup degrades to a
visible message rather than an empty interpolation.

## Adding a repo

Add a top-level key to `language-upgrade-refs.json` named for the remote
(`pulumi/pulumi-yaml`), with the same key set as an existing entry. Every key
referenced by `SKILL.md` must be present or the skill renders a `MISSING KEY`
line where the value should be.

## Checking a change

```sh
./language-upgrade-ref.sh all       # from inside a target repo
./language-upgrade-ref.sh repo      # confirm remote detection
```

To see what the skill actually expanded to on a real run, find the invocation
in the session transcript — the stored user message is the post-substitution
text:

```sh
grep -l "Drive the language-upgrade PR" ~/.claude/projects/*/*.jsonl
```
