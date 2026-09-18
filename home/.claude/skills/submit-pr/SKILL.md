---
name: submit-pr
description: Create a GitHub PR. Use whenever you want to create a new PR.
---

# Submit a PR

## Title

The PR title is a short sentience fragment of the change applied, as understood by
another engineer. Good examples:

- "Consolidate the weekly release jobs into `pulumi/home`"
- "Forward caller error hooks to remote component providers"
- "avoid trying to delete resources `PendingReplacement`"

## Description

The PR description should be a short description of the change and why. It should not
include markdown headers, a summary section, a test section or any structure beyond the
change itself. It should not include a testing section, nor mention that you ran the tests
locally.

Good examples:

```markdown
When a remote component’s caller supplied state migrations, the engine used that callback list instead of the provider’s list. Even a caller no-op could silently suppress a required provider migration. Compose both lists into a single migration chain, running provider callbacks first so caller callbacks see the provider’s upgraded state.
```

```markdown
The elasticsearch schema contains `@`, so we need to deal with that in Go codegen. Simply strip it out to avoid creating invalid identifiers.
```

````markdown
Python previously generated:

```python
def create_routes(range_body):
    for routes_range in [{"value": i} for i in range(0, range_body)]:
        routes.append(nestedobject.Target(
            f"routes-{routes_range['value']}",
            name=values.apply(
                lambda values: values.results[routes_range["value"]])))

(len(values.results)).apply(create_routes)
```
The length expression calls len on an Output and apply on the resulting int.

Worse, since the lambda runs later, it receives the last value of `routes_range`, not the one of the iteration it belongs to.

To fix this we use a lambda default argument for the loop variable to ensure we capture it:

```python
def create_routes(range_body):
    for routes_range in [{"value": i} for i in range(0, range_body)]:
        routes.append(nestedobject.Target(
            f"routes-{routes_range['value']}",
            name=values.apply(
                lambda values, _routes_range=routes_range:     # default arg to make sure we have the correct route_range
                    values.results[_routes_range["value"]])))

(pulumi.Output.from_input(values.results)
    .apply(lambda value: len(value))).apply(create_routes)
```
````

### Footer

PRs that close an issue have a "Fixes
https://github.com/<org>/<repo>/issues/<issue-number>" suffix line at the end of the PR
description. If the PR closes multiple issues, each "Fixes" should get its own line.

PRs that replace another PR should include a "Closes <pr>" suffix line at the end of the
PR description. Formatting is the same as for issues.

Good example:

```markdown
<description body>

Fixes https://github.com/<org>/<repo>/issues/123
Fixes https://github.com/<org>/<repo>/issues/456
```

## Changelog — only when the change is user-visible

Add a repo appropriate changelog **iff** the change is user visible. **Skip it** for
refactors, tests, CI, comments, or any internal-only change.

- `body` is **terse** — a short phrase naming the user-facing change (e.g.
  ``Add `base64gunzip`, `urldecode` and `cidrcontains` ``), **not** an
  explanatory sentence. The why and how belong in the PR description, not here.
- Quote every function or feature name in `backticks`.
- It **does not end in punctuation**.

Some repos don't have a changelog, thats OK.

## 4. Create

Use `gh pr create` to create a single PR. Use `gs stack submit` to submit a stack of
changes. If you only want to submit a PR, always use `gh`. If you want to submit a stack,
always use `gs`.

Because `gs stack submit --fill` populates titles & PR bodies from commit messages, if you
use that option you need to ensure that your commit messages comply with the above
standard.
