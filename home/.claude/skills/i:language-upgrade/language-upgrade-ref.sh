#!/usr/bin/env bash
# Look up a reference for the /i:language-upgrade command, scoped to the
# `origin` remote of the repo it is run in (e.g. "pulumi/pulumi-java").
# Usage: language-upgrade-ref.sh <key> | all | repo
# Always exits 0 so `!` interpolation in a command file never breaks; missing
# data is reported in-band for the agent to read.
set -uo pipefail

dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
db="$dir/language-upgrade-refs.json"
key="${1:-all}"

url="$(git remote get-url origin 2>/dev/null || true)"
repo="$(printf '%s' "$url" | sed -E 's#^[a-z]+://[^/]+/##; s#^[^:/]+@[^:]+:##; s#\.git$##')"
[ "$key" = repo ] && { echo "${repo:-<no origin remote>}"; exit 0; }

if [ ! -r "$db" ]; then
	echo "MISSING DB: $db is not readable. Stop and tell the user."
	exit 0
fi

if [ -z "$repo" ] || ! jq -e --arg r "$repo" 'has($r)' "$db" >/dev/null 2>&1; then
	echo "NO ENTRY for origin remote '${url:-<none>}' (resolved to '${repo:-}') in $db."
	echo "Stop and ask the user to add one before continuing."
	exit 0
fi

if [ "$key" = all ]; then
	jq -r --arg r "$repo" '.[$r] | to_entries[] | "- \(.key): \(.value)"' "$db"
	exit 0
fi

jq -r --arg r "$repo" --arg k "$key" \
	'.[$r][$k] // "MISSING KEY \($k) for \($r) — ask the user to add it."' "$db"
