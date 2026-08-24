---
name: file-issue
description: File a GitHub issue on Ian's behalf. Use whenever asked to create, file, or draft a GitHub issue in any repo.
---

# Filing GitHub issues

## Rules

- **Check for duplicates first.** Search the repo's existing issues (open and closed) for the symptom and key error text before filing. If one exists, report it instead of filing; add a comment only if asked.
- Short and to the point. Cut anything the reader doesn't need to understand or reproduce the problem.
- Bug issues anchor on a **user-visible problem**: what the user did, what they saw, what they expected to happen instead. Do not focus on the fix — no implementation details, no root-cause code tour, no "suggested fix" section, even if a fix already exists.
- Start the body with the AI-authorship note, exactly as in the example below. No "(Claude Code)" or other variants.
- Refer to Ian as `@iwahbe`, never by name.
- Verify the repro before filing. Ask before filing if the target repo is ambiguous.

## Model issue

Title: ``Packages added with `--server` break after `rm -rf sdks && pulumi install`: regenerated SDK loses the plugin download URL``

```markdown
> [!NOTE]
> This issue was authored by an AI agent on @iwahbe's behalf.

## What happened?

`pulumi package add <name> --server <url>` records the package in the `packages`
section of Pulumi.yaml, including its `pluginDownloadURL`. That entry should be
enough to recreate the generated SDK: deleting `./sdks` and running
`pulumi install` is expected to restore the project to a working state.

Instead, the SDK that `pulumi install` regenerates no longer knows the plugin's
download URL. As a result `pulumi install` itself can fail trying to fetch the
plugin from `get.pulumi.com`, and `pulumi up` fails on any machine that doesn't
already have the plugin cached.

## Example

​```console
$ pulumi package add file@0.1.0 --server git://github.com/iwahbe/pulumi-go-provider-example
$ pulumi up     # ✓ works
$ rm -rf ./sdks
$ pulumi install
error: installing packages: failed to download plugin: file-0.1.0: 403 HTTP error
fetching plugin from https://get.pulumi.com/releases/plugins/pulumi-resource-file-v0.1.0-darwin-arm64.tar.gz
​```

**Expected:** `rm -rf ./sdks && pulumi install && pulumi up` succeeds, leaving
the project in the same state `pulumi package add` produced.

## Output of `pulumi about`

​```
CLI
Version      3.258.0-alpha.1786723376
...
​```
```

Shape: the AI note, `## What happened?` (scenario, failure, expectation in a few sentences), `## Example` (minimal repro commands with verbatim error output and an **Expected:** line), then environment (`pulumi about` or equivalent) when relevant.
