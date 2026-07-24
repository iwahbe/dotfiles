#!/usr/bin/env bash
# Block until a PR's CI reaches a terminal state, or until something needs a human.
#
# Usage: watch-pr.sh [<pr-number-or-url>] [--required]
#   default: every check is decisive. --required narrows to required checks, but
#   falls back to all if the repo marks none as required (many do not).
#
# Exit codes:
#   0  all watched checks passed
#   1  at least one check failed (failing checks printed, with links)
#   3  blocked: CI cannot progress without action (rebase, conflicts, closed PR)
#   4  usage/environment error
#
# Never hangs on the "CI never started" case: if no checks register within
# STARTUP_TIMEOUT, it diagnoses the merge state instead of polling forever.
set -uo pipefail

STARTUP_TIMEOUT="${STARTUP_TIMEOUT:-180}" # seconds to wait for checks to appear
INTERVAL="${INTERVAL:-20}"                # watch refresh, seconds

pr=""
scope=""
for a in "$@"; do
	case "$a" in
	--required) scope="--required" ;;
	-*) echo "unknown flag: $a" >&2; exit 4 ;;
	*) pr="$a" ;;
	esac
done

command -v gh >/dev/null || { echo "gh not installed" >&2; exit 4; }

# `gh pr view` with no argument resolves the PR for the current branch.
view() { gh pr view ${pr:+"$pr"} --json "$1" --jq "$2" 2>/dev/null; }

state="$(view state .state)"
[ -n "$state" ] || { echo "BLOCKED: no PR found (pass a number, or check out the branch)."; exit 3; }
[ "$state" = OPEN ] || { echo "BLOCKED: PR is $state, not OPEN."; exit 3; }

# mergeStateStatus is the reliable signal for "CI will never start".
#   DIRTY    — merge conflicts; CI will not run until resolved
#   BEHIND   — base moved and the repo requires up-to-date branches
#   BLOCKED  — usually just a missing approval; CI still runs, so keep watching
diagnose_stall() {
	local ms
	ms="$(view mergeStateStatus .mergeStateStatus)"
	case "$ms" in
	DIRTY)
		echo "BLOCKED: merge conflicts (mergeStateStatus=DIRTY). CI will not run."
		echo "Rebase onto the base branch, resolve, push, then watch again."
		exit 3
		;;
	BEHIND)
		echo "BLOCKED: branch is behind its base (mergeStateStatus=BEHIND)."
		echo "Update it (\`gh pr update-branch${pr:+ $pr}\` or rebase + push), then watch again."
		exit 3
		;;
	esac
	return 0
}

diagnose_stall

# Wait for checks to register. Immediately after a push `gh pr checks` fails with
# "no checks reported", which is indistinguishable from "CI is never coming"
# except by waiting — so wait, but bounded.
waited=0
until [ "$(gh pr checks ${pr:+"$pr"} --json name --jq length 2>/dev/null || echo 0)" -gt 0 ]; do
	if [ "$waited" -ge "$STARTUP_TIMEOUT" ]; then
		echo "No checks registered after ${STARTUP_TIMEOUT}s."
		diagnose_stall
		echo "BLOCKED: CI has not started and the PR is not conflicted or behind."
		echo "Likely no workflow matched the push, or the run needs approval. Check the Actions tab."
		exit 3
	fi
	sleep "$INTERVAL"
	waited=$((waited + INTERVAL))
done

# Many repos configure no required checks at all, and `--required` then fails
# outright ("no required checks reported") rather than matching everything.
if [ -n "$scope" ] &&
	! gh pr checks ${pr:+"$pr"} --required --json name >/dev/null 2>&1; then
	echo "note: no required checks configured; watching all checks instead."
	scope=""
fi

# --watch blocks until terminal; --fail-fast returns as soon as one check fails,
# so the caller can start fixing instead of waiting out the rest of the matrix.
err="$(mktemp)"
trap 'rm -f "$err"' EXIT
gh pr checks ${pr:+"$pr"} $scope --watch --fail-fast --interval "$INTERVAL" >/dev/null 2>"$err"
rc=$?

failed="$(gh pr checks ${pr:+"$pr"} $scope --json bucket,name,link \
	--jq '[.[] | select(.bucket == "fail")] | .[] | "  \(.name)\n    \(.link)"' 2>/dev/null)"

if [ -n "$failed" ]; then
	echo "FAILED:"
	echo "$failed"
	exit 1
fi

# rc 8 means "still pending" — with --watch that means the watch was interrupted.
if [ "$rc" -ne 0 ]; then
	echo "Watch ended with gh exit $rc but no check is in a failed state."
	[ -s "$err" ] && sed 's/^/  gh: /' "$err"
	exit 3
fi

echo "PASSED: all checks green."
exit 0
