---
name: retriage
description: Use when Ian has asked you to retriage a repository
---

# Retriage

Retriaging is the process of validating that old issues are still useful, correct and have
not been fixed yet. 

For a bug, this means reproducing the bug on the latest version of everything. If the bug
is fixed, its best to find the PR that fixed it. It the bug no longer applies because it
doesn't make sense, say its a bug in a feature that is no longer supported, that is also a
reason to close.

For an enhancement request, this means validating that the request hasn't been fulfilled,
and that apply the request is still coherent - that there isn't already a blessed way to
do something or that the feature the request applies to is still supported.

For engineering issues, we validate that they are requesting work that makes sense, and
that has not already been done yet.

## Flow

- Use the `retriage_checkout_issue` tool to find an issue to retriage. Read the issue
  (with `gh issue view <number> --repo <owner/repo> --comments`), then attempt to
  reproduce / confirm / check as appropriate for the issue kind.
  
- If the issue no longer applies, bubble that up to your user.
  
- After you have made a decision, finish the issue with `retriage_finish_issue` to mark
  the issue as retriaged.

## Actionable

### Bugs

A bug is actionable if and only if:

It has been fixed in the main branch, meaning **both** that:

- [ ] You can reproduce it on an old commit
- [ ] You cannot reproduce it on the default branch.

This requires a genuine reproduction against the binaries we shipped or against the
interfaces we expose to users.
  
**or**

It no longer applies:

- It is a problem in a feature that is no longer supported

### Enhancements

An enhancement issue is actionable if and only if:

it has been applied as described to the main branch - already implemented

**or**

it doesn't make sense to implement the enhancement because it applies to a feature that no longer exists

### Engineering

An engineering issue is actionable if and only if:

the suggested change has already been applied on the main branch

**or**

it doesn't make sense to implement the suggested change because it applies to a feature or part of the code that no longer exists

## The note

The note for `retriage_finish_issue` is what the next pass will read. Keep it under about
five sentences. Say:

- what you checked (version, code path, reproduction result),
- what changed since the last note,
- for `action`: the concrete next step

Notes should be short and factual.
