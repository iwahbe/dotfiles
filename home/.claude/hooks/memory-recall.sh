#!/bin/bash
# UserPromptSubmit hook: search the memory MCP server with the prompt and
# inject relevant notes as context. Fail-open: any error exits 0 with no
# output — recall must never block a prompt.
#
# MEMORY_RECALL_URL overrides the server (e.g. http://localhost:8100/memory
# for testing against a local instance, which needs no Access token).
set -uo pipefail
exec 2>/dev/null

URL=${MEMORY_RECALL_URL:-https://mcp.iwahbe.com/memory}

INPUT=$(cat) || exit 0
PROMPT=$(jq -r '.prompt // ""' <<<"$INPUT") || exit 0
CWD=$(jq -r '.cwd // ""' <<<"$INPUT") || exit 0

# Tiny prompts ("yes", "proceed") make useless queries.
[ "${#PROMPT}" -lt 40 ] && exit 0

PROJECT=$(basename "$(git -C "$CWD" rev-parse --show-toplevel 2>/dev/null || echo "$CWD")")

AUTH=()
case "$URL" in
  https://mcp.iwahbe.com/*)
    if ! TOKEN=$(cloudflared access token -app=https://mcp.iwahbe.com); then
      echo "<memory-recall>"
      echo "Memory recall is unavailable: the Cloudflare Access token has expired or is missing."
      echo 'Run `cloudflared access login --quiet https://mcp.iwahbe.com` via Bash to restore it (the command is pre-approved), then continue with the task. Recall resumes on the next prompt.'
      echo "</memory-recall>"
      exit 0
    fi
    AUTH=(-H "cf-access-token: $TOKEN")
    ;;
esac

# The AUTH expansion guard keeps macOS bash 3.2's `set -u` from treating an
# empty array as unbound.
HITS=$(jq -n --arg q "$PROMPT" --arg p "$PROJECT" \
  '{jsonrpc:"2.0",id:1,method:"tools/call",params:{name:"search_notes",
    arguments:{query:$q,tags:{project:$p},limit:5}}}' |
  curl -sf --max-time 5 "$URL" ${AUTH[@]+"${AUTH[@]}"} \
    -H 'Content-Type: application/json' \
    -H 'Accept: application/json, text/event-stream' \
    --data-binary @- |
  jq '.result.content[0].text | fromjson | [.[] | select(.superseded | not)]') || exit 0

[ -z "$HITS" ] && exit 0
[ "$(jq 'length' <<<"$HITS")" -eq 0 ] && exit 0

echo "<memory-recall>"
echo "Notes from persistent memory that may be relevant; read_note(id) fetches a full note:"
jq -r '.[] | "- [\(.id)] \(.title): \(.excerpt)"' <<<"$HITS"
echo "</memory-recall>"
