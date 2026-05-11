;;; claudes.el --- Track Claude Code sessions inside Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ian Wahbe

;; Author: Ian Wahbe
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (eat "0.9") (magit "4.0"))
;; Keywords: tools, processes, terminals

;;; Commentary:

;; `claudes' tracks Claude Code sessions running inside Emacs terminals
;; (`eat', `vterm') and lets you list them with their current status
;; (working / waiting / stopped) and jump back to the buffer that owns
;; each one.
;;
;; The package keeps state in a single hash table -- nothing is written
;; to disk.  Claude Code's hook system reaches into Emacs via
;; `emacsclient', which means an Emacs server must be running.
;;
;; =============================== Setup ===============================
;;
;;   1. Make sure the Emacs server is up:
;;
;;        (require 'server)
;;        (unless (server-running-p) (server-start))
;;
;;   2. Hook the terminal modes you use so the spawned shells inherit
;;      the buffer name (this is what lets the session list jump back
;;      to the right buffer):
;;
;;        (require 'claudes)
;;        (claudes-setup-eat)     ; required for eat
;;        (claudes-setup-vterm)   ; optional, only if you also use vterm
;;
;;   3. Wire the Claude Code hooks.  Two ways:
;;
;;      a) AUTOMATIC (recommended): `claudes-setup-eat' exports the
;;         hooks JSON in the env var named by `claudes-hooks-env-var'
;;         (default `CLAUDE_HOOKS_JSON').  If your shell rc defines a
;;         function like:
;;
;;            if [[ -n "$CLAUDE_HOOKS_JSON" ]]; then
;;              claude() {
;;                command claude \
;;                  --settings <(printf '%s' "$CLAUDE_HOOKS_JSON") "$@"
;;              }
;;            fi
;;
;;         then every `claude' run inside an eat shell picks the hooks
;;         up via process substitution -- nothing is written to disk
;;         and your existing `~/.claude/settings.json' hooks still fire
;;         (Claude merges both sets).
;;
;;      b) MANUAL: run `M-x claudes-show-settings-snippet' and paste
;;         the "hooks" block into ~/.claude/settings.json.
;;
;; =============================== Usage ===============================
;;
;;   M-x claudes-sessions
;;
;;     Pops to a tabulated list of every tracked session.  RET on a row
;;     switches to the eat/vterm buffer that owns the session; `k'
;;     forgets the row; `g' refreshes and prunes dead entries.
;;
;; ============================ Requirements ============================
;;
;;   * Emacs 29.1+ (for `json-parse-string')
;;   * `emacsclient' on $PATH inside the eat shell
;;   * `base64' on $PATH (POSIX standard)

;;; Code:

(require 'cl-lib)
(require 'eat)
(require 'json)
(require 'magit)
(require 'project)
(require 'subr-x)
(require 'tabulated-list)

(defgroup claudes nil
  "Track Claude Code sessions inside Emacs."
  :group 'tools
  :prefix "claudes-")

(defcustom claudes-buffer-env-var "CLAUDE_EMACS_BUFFER"
  "Environment variable used to pass the spawning Emacs buffer name."
  :type 'string)

(defcustom claudes-hooks-env-var "CLAUDE_HOOKS_JSON"
  "Environment variable used to pass the hooks JSON to the wrapped shell.
A `claude' shell function reads this and feeds it to `claude' via the
`--settings' flag, so the hooks fire without editing
`~/.claude/settings.json'.  See the package commentary for the
shell-function snippet."
  :type 'string)

(defcustom claudes-debug nil
  "When non-nil, log every hook callback to `claudes-debug-buffer-name'."
  :type 'boolean)

(defcustom claudes-debug-buffer-name "*claudes log*"
  "Buffer that receives debug messages when `claudes-debug' is non-nil."
  :type 'string)

(defcustom claudes-list-buffer-name "*Claude Sessions*"
  "Name of the buffer that displays the session list."
  :type 'string)

(defvar claudes--sessions (make-hash-table :test #'equal)
  "Map of Claude session-id (string) to a plist.
Keys: :status :cwd :buffer :title :started.")

;;;; Internal: state mutation -----------------------------------------------

(defun claudes--upsert (session-id status cwd buffer title)
  "Create or update the entry for SESSION-ID.
TITLE is set only once -- subsequent non-nil TITLE values are ignored
so a session keeps the human-readable label derived from its first
user message even after later hooks fire."
  (let ((entry (or (gethash session-id claudes--sessions)
                   (list :started (current-time)))))
    (when status (setq entry (plist-put entry :status status)))
    (when (and cwd (not (string-empty-p cwd)))
      (setq entry (plist-put entry :cwd cwd)))
    (when (and buffer (not (string-empty-p buffer)))
      (setq entry (plist-put entry :buffer buffer)))
    (when (and title (not (string-empty-p title))
               (not (plist-get entry :title)))
      (setq entry (plist-put entry :title title)))
    (puthash session-id entry claudes--sessions))
  (claudes--refresh-list))

(defun claudes--remove (session-id)
  "Drop SESSION-ID from the tracker."
  (remhash session-id claudes--sessions)
  (claudes--refresh-list))

(defun claudes--refresh-list ()
  "Re-render the session list buffer if it is currently displayed."
  (when-let* ((buf (get-buffer claudes-list-buffer-name)))
    (with-current-buffer buf
      (let ((line (line-number-at-pos)))
        (revert-buffer nil t)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun claudes--prune-dead ()
  "Remove sessions whose recorded buffer no longer exists."
  (let (dead)
    (maphash
     (lambda (sid p)
       (let ((b (plist-get p :buffer)))
         (when (and b (not (get-buffer b)))
           (push sid dead))))
     claudes--sessions)
    (dolist (sid dead) (remhash sid claudes--sessions))))

;;;; Hook entry point -------------------------------------------------------
;;
;; Called from a Claude Code hook via emacsclient.  Inputs are
;; base64-encoded so we never have to worry about quoting buffer
;; names, JSON payloads, or shell metacharacters.

(defun claudes--decode-b64 (s)
  "Decode the base64 string S into a UTF-8 multibyte string."
  (decode-coding-string (base64-decode-string (or s "")) 'utf-8))

(defun claudes--extract-title (transcript-path &optional quiet)
  "Return the first real user prompt from the JSONL at TRANSCRIPT-PATH.

Records in a transcript that *look* like user input but aren't real
prompts are skipped:
  * `isMeta' records (the local-command-caveat header etc.)
  * synthesised wrappers whose content starts with `<' --
    `<command-name>', `<system-reminder>', `<local-command-stdout>',
    and friends.  A real prompt never begins that way.

Returns nil if the file has no qualifying user message yet.
Newlines and runs of whitespace are collapsed to single spaces.

Normally emits a `claudes' warning when TRANSCRIPT-PATH is nil or
names a file that is not readable.  When QUIET is non-nil, these
warnings are suppressed -- used for early-lifecycle hooks where the
transcript may legitimately not have been written yet."
  (cond
   ((null transcript-path)
    (unless quiet
      (display-warning 'claudes "Hook payload missing transcript_path"))
    nil)
   ((not (file-readable-p transcript-path))
    (unless quiet
      (display-warning 'claudes
                       (format "Transcript not readable: %s" transcript-path)))
    nil)
   (t
    (with-temp-buffer
      (insert-file-contents transcript-path)
      (goto-char (point-min))
      (catch 'found
        (while (not (eobp))
          (let ((line-end (line-end-position)))
            (when (> line-end (point))
              (let* ((line (buffer-substring-no-properties (point) line-end))
                     (rec (ignore-errors
                            (json-parse-string line
                                               :object-type 'hash-table
                                               :null-object nil
                                               :false-object nil))))
                (when (claudes--user-prompt-p rec)
                  (let* ((msg (gethash "message" rec))
                         (content (and (hash-table-p msg)
                                       (gethash "content" msg)))
                         (text (claudes--first-text content)))
                    (when text
                      (throw 'found
                             (replace-regexp-in-string
                              "[ \t\n\r]+" " " text))))))))
          (forward-line 1))
        nil)))))

(defun claudes--user-prompt-p (rec)
  "Return non-nil if REC is a real user prompt entry.
Filters out `isMeta' caveats and synthesised wrappers
(`<command-name>...', `<system-reminder>...', etc.)."
  (and (hash-table-p rec)
       (equal (gethash "type" rec) "user")
       (not (eq (gethash "isMeta" rec) t))
       (let* ((msg (gethash "message" rec))
              (content (and (hash-table-p msg) (gethash "content" msg)))
              (text (claudes--first-text content)))
         (and text
              (not (string-match-p "\\`[ \t\n\r]*<" text))))))

(defun claudes--first-text (content)
  "Return the first text payload in CONTENT, which may be a string or vector.
Claude transcripts encode user message bodies either as a plain string
or as a vector of `{type, text, ...}' parts; handle both."
  (cond
   ((stringp content) content)
   ((vectorp content)
    (let (result)
      (seq-doseq (part content)
        (when (and (not result)
                   (hash-table-p part)
                   (equal (gethash "type" part) "text"))
          (setq result (gethash "text" part))))
      result))))

(defun claudes--log (fmt &rest args)
  "Append a formatted message to `claudes-debug-buffer-name'."
  (when claudes-debug
    (with-current-buffer (get-buffer-create claudes-debug-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S.%3N] "))
      (insert (apply #'format fmt args))
      (insert "\n"))))

(defun claudes-handle-hook (status buffer-b64 payload-b64)
  "Update session state from a Claude Code hook callback.

STATUS is one of \"start\" \"working\" \"waiting\" \"stopped\" \"end\".
BUFFER-B64 is base64 of the value of the env var named by
`claudes-buffer-env-var' (empty if not set).  PAYLOAD-B64 is base64 of
the JSON Claude wrote to the hook's stdin.

This function is invoked by the snippet from
`claudes-show-settings-snippet'; it is not meant for direct use."
  (claudes--log "hook: status=%s buffer-b64-len=%d payload-b64-len=%d"
                status (length (or buffer-b64 "")) (length (or payload-b64 "")))
  (let* ((buffer (claudes--decode-b64 buffer-b64))
         (json (json-parse-string (claudes--decode-b64 payload-b64)
                                  :object-type 'hash-table
                                  :null-object nil
                                  :false-object nil))
         (session-id (gethash "session_id" json))
         (cwd (gethash "cwd" json))
         (transcript (gethash "transcript_path" json))
         (existing (gethash session-id claudes--sessions))
         (title (and (not (plist-get existing :title))
                     (claudes--extract-title
                      transcript
                      (member status '("start" "working"))))))
    (claudes--log "  decoded buffer=%S session-id=%S cwd=%S title=%S"
                  buffer session-id cwd title)
    (unless (and session-id (not (string-empty-p session-id)))
      (error "claudes: hook payload missing session_id"))
    (if (equal status "end")
        (claudes--remove session-id)
      (claudes--upsert session-id status cwd buffer title))
    nil))

;;;; Tabulated list view ----------------------------------------------------

(defvar claudes-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'claudes-sessions-jump)
    (define-key map (kbd "s")   #'claudes-new-session)
    (define-key map (kbd "w")   #'claudes-new-worktree-session)
    map)
  "Keymap for `claudes-sessions-mode'.")

(define-derived-mode claudes-sessions-mode tabulated-list-mode "Claude-Sessions"
  "Major mode for listing tracked Claude Code sessions."
  (setq tabulated-list-format
        [("Status" 9 t)
         ("Title"  50 t)
         ("CWD"    50 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'claudes--list-entries)
  (add-hook 'tabulated-list-revert-hook #'claudes--prune-dead nil t)
  (tabulated-list-init-header))

(defun claudes--list-entries ()
  "Return the rows for `tabulated-list-entries'."
  (let (rows)
    (maphash
     (lambda (sid p)
       (push (list sid
                   (vector (or (plist-get p :status) "?")
                           (or (plist-get p :title)
                               (plist-get p :buffer)
                               "—")
                           (abbreviate-file-name (or (plist-get p :cwd) ""))))
             rows))
     claudes--sessions)
    rows))

;;;###autoload
(defun claudes-sessions ()
  "Show a tabulated list of tracked Claude Code sessions in this window.
Press RET on a row to jump to the buffer that owns the session."
  (interactive)
  (let ((buf (get-buffer-create claudes-list-buffer-name)))
    (with-current-buffer buf
      (claudes-sessions-mode)
      (claudes--prune-dead)
      (tabulated-list-print))
    (switch-to-buffer buf)))

(defun claudes--launch (dir initial-prompt)
  "Open a fresh `eat' buffer running `claude' in DIR.
Always creates a new buffer (auto-numbered if the name is in use), so
existing claude sessions are never disturbed.  If INITIAL-PROMPT is
non-nil and non-empty, pass it as `claude's initial query.  Hooks are
wired up by passing the snippet returned by `claudes-settings-snippet'
through claude's `--settings' flag, so this works even though no shell
is involved."
  (let* ((default-directory (file-name-as-directory dir))
         (claude (or (executable-find "claude")
                     (user-error "`claude' not found on PATH")))
         (proj (file-name-nondirectory (directory-file-name dir)))
         (buf (generate-new-buffer (format "*claude-%s*" proj)))
         (args (append (list "--settings" (claudes-settings-snippet))
                       (and initial-prompt
                            (not (string-empty-p initial-prompt))
                            (list initial-prompt)))))
    (with-current-buffer buf (eat-mode))
    (eat-exec buf (buffer-name buf) claude nil args)
    (switch-to-buffer buf)))

;;;###autoload
(defun claudes-new-session (&optional initial-prompt)
  "Start a new `claude' session in a project directory.
The directory is chosen with `project-prompt-project-dir'.  With a
prefix argument, prompt for INITIAL-PROMPT and pass it as the
initial query."
  (interactive (list (when current-prefix-arg
                       (read-string "Initial claude prompt: "))))
  (claudes--launch (project-prompt-project-dir) initial-prompt))

;;;###autoload
(defun claudes-new-worktree-session (&optional initial-prompt)
  "Create a git worktree via `magit-worktree-branch' and `claude' inside it.

Prompts for a source project with `project-prompt-project-dir', then
delegates the path / branch / start-point prompts to magit.  After the
worktree exists, the new directory is registered with `project.el' and
a `claude' session is launched there.

With prefix argument, prompt for INITIAL-PROMPT and pass it to `claude'
as the initial query."
  (interactive (list (when current-prefix-arg
                       (read-string "Initial claude prompt: "))))
  (let* ((src (project-prompt-project-dir))
         (default-directory (file-name-as-directory src))
         (before (mapcar #'car (magit-list-worktrees))))
    (call-interactively #'magit-worktree-branch)
    (let* ((after (mapcar #'car (magit-list-worktrees)))
           (new (cl-set-difference after before :test #'equal)))
      (cond
       ((null new)
        (user-error "No new worktree was created"))
       ((cdr new)
        (user-error "Multiple new worktrees found -- can't disambiguate: %S"
                    new))
       (t
        (let* ((path (file-name-as-directory (car new)))
               (proj (project-current nil path)))
          (if proj
              (project-remember-project proj)
            (message "claudes: %s not detected as a project; not remembered"
                     path))
          (claudes--launch path initial-prompt)))))))

(defun claudes-sessions-jump ()
  "Switch to the buffer owning the session at point in the current window."
  (interactive)
  (let* ((sid (tabulated-list-get-id))
         (entry (and sid (gethash sid claudes--sessions)))
         (name (and entry (plist-get entry :buffer))))
    (cond
     ((not entry) (user-error "No session at point"))
     ((not name)  (user-error "No buffer recorded for session %s" sid))
     ((not (get-buffer name))
      (user-error "Buffer %s no longer exists" name))
     (t (switch-to-buffer name)))))

;;;; Terminal integration ---------------------------------------------------

(defun claudes--inject-buffer-env ()
  "Mode hook: expose buffer name and hooks JSON to subprocesses.
Sets `process-environment' buffer-locally so the shell `eat' or
`vterm' is about to launch (and any descendants -- including
`claude') inherits both:

  * `claudes-buffer-env-var' (default CLAUDE_EMACS_BUFFER): the
    name of this Emacs buffer; used by the hook callback to
    associate the session with a buffer to jump to.

  * `claudes-hooks-env-var' (default CLAUDE_HOOKS_JSON): the
    JSON snippet returned by `claudes-settings-snippet'.  A
    `claude' shell function in your shell rc forwards this to
    `claude --settings <(...)' so the hooks fire without
    touching `~/.claude/settings.json'."
  (setq-local process-environment
              (append
               (list (format "%s=%s"
                             claudes-buffer-env-var
                             (buffer-name))
                     (format "%s=%s"
                             claudes-hooks-env-var
                             (claudes-settings-snippet)))
               process-environment))
  (add-hook 'kill-buffer-hook #'claudes--forget-buffer-sessions nil t))

(defun claudes--forget-buffer-sessions ()
  "Remove tracker entries whose :buffer is the current buffer."
  (let ((name (buffer-name))
        dead)
    (maphash
     (lambda (sid p)
       (when (equal (plist-get p :buffer) name)
         (push sid dead)))
     claudes--sessions)
    (dolist (sid dead) (remhash sid claudes--sessions)))
  (claudes--refresh-list))

;;;###autoload
(defun claudes-setup-eat ()
  "Make `eat' shells expose their buffer name via the env var.
Call this once after loading `claudes'.  Idempotent."
  (interactive)
  (add-hook 'eat-mode-hook #'claudes--inject-buffer-env))

;;;###autoload
(defun claudes-setup-vterm ()
  "Make `vterm' shells expose their buffer name via the env var.
Call this once after loading `claudes'.  Idempotent."
  (interactive)
  (add-hook 'vterm-mode-hook #'claudes--inject-buffer-env))

;;;; Settings snippet -------------------------------------------------------

(defconst claudes--hook-events
  '(("SessionStart"     . "start")
    ("UserPromptSubmit" . "working")
    ("Notification"     . "waiting")
    ("Stop"             . "stopped")
    ("SessionEnd"       . "end"))
  "Mapping from Claude Code hook event name to the STATUS we record.")

(defun claudes--hook-command (status)
  "Return the shell command to register STATUS with this Emacs.
Reads the JSON payload Claude writes to stdin, base64-encodes it
along with the buffer name in $CLAUDE_EMACS_BUFFER, and hands both
to `claudes-handle-hook' through `emacsclient'.

Failures propagate: stderr from `base64' or `emacsclient' surfaces
in Claude's hook output, and a non-zero exit is returned to Claude
unchanged.  No errors are swallowed."
  (format
   (concat
    "set -e; "
    "B=$(printf '%%s' \"${%s:-}\" | base64 | tr -d '\\n'); "
    "P=$(base64 | tr -d '\\n'); "
    "emacsclient --no-wait "
    "-e \"(claudes-handle-hook \\\"%s\\\" \\\"$B\\\" \\\"$P\\\")\"")
   claudes-buffer-env-var status))

(defun claudes-settings-snippet ()
  "Return the JSON snippet to merge into ~/.claude/settings.json."
  (let* ((blocks
          (mapcar
           (lambda (cell)
             (cons (intern (car cell))
                   `[((hooks . [((type . "command")
                                 (command . ,(claudes--hook-command
                                              (cdr cell))))]))]))
           claudes--hook-events))
         (json-encoding-pretty-print t)
         (json-encoding-default-indentation "  "))
    (json-encode `((hooks . ,blocks)))))

;;;###autoload
(defun claudes-show-settings-snippet ()
  "Display the JSON to merge into ~/.claude/settings.json."
  (interactive)
  (let ((buf (get-buffer-create "*claudes settings*")))
    (with-current-buffer buf
      (erase-buffer)
      (when (fboundp 'js-json-mode) (js-json-mode))
      (insert "// Merge the contents of \"hooks\" below into your\n"
              "// ~/.claude/settings.json (or .claude/settings.json in a\n"
              "// project).  Existing hooks are preserved -- Claude\n"
              "// merges them and fires every match.\n\n")
      (insert (claudes-settings-snippet))
      (insert "\n")
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(provide 'claudes)
;;; claudes.el ends here
