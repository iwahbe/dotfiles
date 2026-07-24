---
name: analyze-ci-failures
description: Analyze GitHub Actions CI/CD test failures for a pull request. Use this when the user asks about failing tests, CI errors, or wants to understand why their PR is failing.
---

# Analyze CI Failures

This skill helps you systematically analyze test failures in GitHub Actions CI runs.

## Instructions

When the user provides a PR URL or asks about CI failures, follow these steps:

### 1. Gather Basic Information

If the user provides a PR URL, extract the PR number. If they ask about the current branch, use `gh pr view` to get the PR information.

```bash
# Get PR info for current branch
gh pr view --json number,statusCheckRollup,headRefName

# Or for a specific PR
gh pr view <PR_NUMBER> --json number,statusCheckRollup,headRefName
```

### 2. Identify Failed Runs and Jobs

List recent workflow runs for the PR branch:

```bash
gh run list --repo pulumi/pulumi --branch <BRANCH_NAME> --limit 5
```

Identify failed jobs in the most recent failed run:

```bash
gh run view <RUN_ID> --json jobs --jq '.jobs[] | select(.conclusion == "failure") | {name: .name, id: .databaseId}'
```

### 3. Download and Analyze Logs

For each failed job, download the logs using the GitHub API (this avoids rate limits and is more reliable than `gh run view --log`):

```bash
gh api repos/pulumi/pulumi/actions/jobs/<JOB_ID>/logs > /tmp/job_<JOB_ID>.txt 2>&1
```

### 4. Search for Failure Patterns

Use targeted searches to find the actual errors:

```bash
# Find test failures
grep -E "FAIL|--- FAIL|panic|fatal error" /tmp/job_<JOB_ID>.txt

# Get context around failures (adjust line numbers as needed)
grep -A 50 "FAIL.*Test" /tmp/job_<JOB_ID>.txt | head -200

# Look for specific error messages
grep -B 5 -A 30 "Error\|error:" /tmp/job_<JOB_ID>.txt
```

### 5. Correlate with Code Changes

Check what files were changed in the PR that might be related:

```bash
# Show files changed in the PR
git diff master...HEAD --stat

# Show specific changes if relevant
git diff master...HEAD -- path/to/relevant/file.go
```

### 6. Provide Analysis

Structure your analysis with:

1. **Summary**: Brief overview of what's failing
2. **Root Cause**: The actual error with relevant log excerpts
3. **Location**: File and line numbers where the issue originates
4. **Related Changes**: How the PR changes relate to the failure
5. **Recommendation**: Suggested fix or next steps
6. **Links**: Direct links to failing jobs using the format:
   ```
   https://github.com/pulumi/pulumi/actions/runs/<RUN_ID>/job/<JOB_ID>
   ```

## Tips

- Use `/tmp/` for storing downloaded logs to avoid cluttering the workspace
- Check multiple failed jobs to see if they share the same root cause
- Look at the test name and the last 100-200 lines of logs for integration test failures
- For Go tests, search for `FAIL:` followed by the test name
- For build failures, look for compilation errors or missing dependencies
- Use `grep -C` for context lines around matches when needed
- Remember that some failures may be unrelated to the PR (flaky tests, infrastructure issues)

## Common Error Patterns

- **Go test failures**: `=== FAIL: TestName` followed by assertion errors
- **Python errors**: Look for tracebacks and `Error:` messages
- **Node.js errors**: Look for `Error:` and stack traces
- **Build failures**: `make: *** [target] Error N`
- **Timeout failures**: `context deadline exceeded` or `timed out`
