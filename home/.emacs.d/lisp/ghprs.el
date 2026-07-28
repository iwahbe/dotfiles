;;; ghprs.el --- Dashboard of my open GitHub PRs -*- lexical-binding: t; -*-

;;; Commentary:
;; ghprs shows every open PR I have authored, across all repos, grouped
;; by repo with stacked PRs drawn as a tree.  Each line leads with two
;; glyphs: review state and required-check CI state.  All data comes from
;; the `gh' CLI, which must be installed and authenticated.
;;
;; Review state comes back in the initial search query.  Required-check CI
;; state cannot be fetched in one query (GraphQL's `isRequired' needs a
;; per-PR argument and large repos overflow context pagination), so each
;; PR gets its own `gh pr checks --required' call, a few at a time, and
;; the buffer updates as results land.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar ghprs-search-query "is:pr is:open author:@me archived:false"
  "GitHub search query selecting the PRs to display.")

(defconst ghprs--max-check-procs 8
  "How many `gh pr checks' processes to run at once.")

(defconst ghprs--graphql
  (concat
   "query { search(query: \"%s\", type: ISSUE, first: 100) { issueCount nodes {"
   " ... on PullRequest {"
   " repository { nameWithOwner } number title url isDraft"
   " baseRefName headRefName reviewDecision additions deletions createdAt"
   " timelineItems(last: 1, itemTypes: [REVIEW_REQUESTED_EVENT])"
   " { nodes { ... on ReviewRequestedEvent { createdAt } } }"
   " latestReviews(first: 10) { nodes { state } } } } } }")
  "Search query template, instantiated with `ghprs-search-query'.")

(defvar-local ghprs--prs nil
  "List of PR plists currently displayed.")

(defvar-local ghprs--issue-count 0
  "Total number of PRs matching the search, which may exceed those shown.")

(defvar-local ghprs--generation 0
  "Incremented on each refresh so stale process callbacks can be dropped.")

(defvar-local ghprs--check-queue nil
  "PRs still waiting for a `gh pr checks' process.")

(defun ghprs--run (command callback)
  "Run COMMAND asynchronously, then call CALLBACK with (EXIT-CODE OUTPUT).
OUTPUT contains both stdout and stderr."
  (let ((buf (generate-new-buffer " *ghprs-proc*")))
    (make-process
     :name "ghprs" :buffer buf :command command :noquery t
     :connection-type 'pipe
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let ((out (with-current-buffer buf (buffer-string))))
                     (kill-buffer buf)
                     (funcall callback (process-exit-status proc) out)))))))

(defun ghprs--review-state (node)
  "Compute the review state symbol for search result NODE."
  (let ((latest (mapcar (lambda (r) (gethash "state" r))
                        (gethash "nodes" (gethash "latestReviews" node)))))
    (pcase (gethash "reviewDecision" node)
      ("APPROVED" 'approved)
      ("CHANGES_REQUESTED" 'changes-requested)
      ;; No decision: the repo requires no review, so fall back to the
      ;; latest review from each reviewer, worst state first.
      (_ (cond ((member "CHANGES_REQUESTED" latest) 'changes-requested)
               ((member "APPROVED" latest) 'approved)
               ((member "COMMENTED" latest) 'commented)
               (t 'pending))))))

(defun ghprs--parse-search (out)
  "Parse OUT, the GraphQL search response, into a list of PR plists."
  (let ((search (gethash "search" (gethash "data" (json-parse-string out)))))
    (setq ghprs--issue-count (gethash "issueCount" search))
    (mapcar (lambda (node)
              (list :repo (gethash "nameWithOwner" (gethash "repository" node))
                    :number (gethash "number" node)
                    :title (gethash "title" node)
                    :url (gethash "url" node)
                    :draft (eq (gethash "isDraft" node) t)
                    :base (gethash "baseRefName" node)
                    :head (gethash "headRefName" node)
                    :additions (gethash "additions" node)
                    :deletions (gethash "deletions" node)
                    :created-at (gethash "createdAt" node)
                    :review-requested-at
                    (let ((events (gethash "nodes" (gethash "timelineItems" node))))
                      (unless (seq-empty-p events)
                        (gethash "createdAt" (elt events 0))))
                    :review (ghprs--review-state node)
                    :ci 'loading))
            (seq-filter (lambda (node) (gethash "number" node))
                        (gethash "nodes" search)))))

(defun ghprs--parse-checks (out)
  "Parse OUT from `gh pr checks --required --json bucket' into a CI symbol."
  (cond
   ;; "no required checks reported" or, when the PR has no checks at all,
   ;; "no checks reported".
   ((string-match-p "no\\( required\\)? checks reported" out) 'none)
   ((not (string-prefix-p "[" out)) 'error)
   (t (let ((buckets (mapcar (lambda (c) (gethash "bucket" c))
                             (json-parse-string out))))
        (cond ((or (member "fail" buckets) (member "cancel" buckets)) 'fail)
              ((member "pending" buckets) 'pending)
              ((member "pass" buckets) 'pass)
              (t 'none))))))

(defun ghprs--next-check (buffer gen)
  "Launch a check process for the next queued PR in BUFFER.
GEN identifies the refresh this work belongs to; stale results are dropped."
  (with-current-buffer buffer
    (when (and (= gen ghprs--generation) ghprs--check-queue)
      (let ((pr (pop ghprs--check-queue)))
        (ghprs--run
         (list "gh" "pr" "checks" (plist-get pr :url) "--required" "--json" "bucket")
         (lambda (_code out)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (= gen ghprs--generation)
                 (plist-put pr :ci (ghprs--parse-checks out))
                 (ghprs--render)
                 (ghprs--next-check buffer gen))))))))))

(defun ghprs--fetch (buffer)
  "Refresh the PR list in BUFFER."
  (with-current-buffer buffer
    (cl-incf ghprs--generation)
    (setq ghprs--check-queue nil)
    (let ((gen ghprs--generation))
      (ghprs--run
       (list "gh" "api" "graphql"
             "-f" (concat "query=" (format ghprs--graphql ghprs-search-query)))
       (lambda (code out)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (= gen ghprs--generation)
               (if (/= code 0)
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert "gh failed:\n\n" out))
                 (setq ghprs--prs (ghprs--parse-search out)
                       ghprs--check-queue (copy-sequence ghprs--prs))
                 (ghprs--render)
                 (dotimes (_ ghprs--max-check-procs)
                   (ghprs--next-check buffer gen)))))))))))

(defun ghprs--review-glyph (state)
  "Return the display glyph for review STATE."
  (pcase state
    ('approved (propertize "✓" 'face 'success))
    ('changes-requested (propertize "✗" 'face 'error))
    ('commented (propertize "✎" 'face 'warning))
    (_ (propertize "○" 'face 'shadow))))

(defun ghprs--ci-glyph (state)
  "Return the display glyph for CI STATE."
  (pcase state
    ('pass (propertize "✓" 'face 'success))
    ('fail (propertize "✗" 'face 'error))
    ('pending (propertize "●" 'face 'warning))
    ('none (propertize "—" 'face 'shadow))
    ('error (propertize "!" 'face 'error))
    (_ (propertize "…" 'face 'shadow))))

(defun ghprs--insert-line (pr prefix)
  "Insert the display line for PR, with tree PREFIX before the title."
  (let ((title (format "#%d %s%s" (plist-get pr :number) (plist-get pr :title)
                       (if (plist-get pr :draft) " (draft)" ""))))
    (insert (propertize
             (concat " " (ghprs--review-glyph (plist-get pr :review))
                     " " (ghprs--ci-glyph (plist-get pr :ci))
                     "  " prefix
                     (if (plist-get pr :draft) (propertize title 'face 'shadow) title)
                     " " (propertize (format "+%d" (plist-get pr :additions))
                                     'face 'success)
                     " " (propertize (format "-%d" (plist-get pr :deletions))
                                     'face 'error)
                     "\n")
             'ghprs-url (plist-get pr :url)))))

(defun ghprs--insert-tree (pr prs prefix child-prefix ancestors)
  "Insert PR and, recursively, the PRs in PRS stacked on it.
PREFIX draws this line's branch; CHILD-PREFIX is the base for its
children.  ANCESTORS guards against base/head reference cycles."
  (ghprs--insert-line pr prefix)
  (let ((children
         (seq-sort-by (lambda (p) (plist-get p :number)) #'<
                      (seq-filter
                       (lambda (p) (and (not (memq p ancestors))
                                        (equal (plist-get p :base) (plist-get pr :head))))
                       prs)))
        (ancestors (cons pr ancestors)))
    (while children
      (let ((child (pop children)))
        (if children
            (ghprs--insert-tree child prs (concat child-prefix "├─ ")
                                  (concat child-prefix "│  ") ancestors)
          (ghprs--insert-tree child prs (concat child-prefix "└─ ")
                                (concat child-prefix "   ") ancestors))))))

(defun ghprs--repos ()
  "Return the sorted list of repos appearing in `ghprs--prs'."
  (sort (delete-dups (mapcar (lambda (p) (plist-get p :repo)) ghprs--prs))
        #'string<))

(defun ghprs--repo-prs (repo)
  "Return the PRs in `ghprs--prs' belonging to REPO."
  (seq-filter (lambda (p) (equal (plist-get p :repo) repo)) ghprs--prs))

(defun ghprs--roots (prs)
  "Return the PRs in PRS that are not stacked on another PR in PRS.
Sorted by PR number."
  (let ((heads (mapcar (lambda (p) (plist-get p :head)) prs)))
    (seq-sort-by (lambda (p) (plist-get p :number)) #'<
                 (seq-remove (lambda (p) (member (plist-get p :base) heads))
                             prs))))

(defun ghprs--render ()
  "Redraw the dashboard from `ghprs--prs', keeping point on the same line."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (insert (propertize
             (concat "Review ✓ approved ✗ changes-requested ✎ commented ○ awaiting"
                     "  ·  CI (required checks) ✓ passed ✗ failed ● pending — none\n")
             'face 'shadow))
    (when (> ghprs--issue-count (length ghprs--prs))
      (insert (propertize (format "Showing %d of %d PRs.\n"
                                  (length ghprs--prs) ghprs--issue-count)
                          'face 'warning)))
    (dolist (repo (ghprs--repos))
      (insert "\n" (propertize repo 'face 'bold) "\n")
      (let ((prs (ghprs--repo-prs repo)))
        (dolist (root (ghprs--roots prs))
          (ghprs--insert-tree root prs "" "" nil))))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun ghprs--days-waiting (pr)
  "Return whole days PR has been waiting for review.
Counted from the last review request, falling back to the PR's creation
when no review was ever requested."
  (floor (/ (float-time
             (time-since (date-to-time (or (plist-get pr :review-requested-at)
                                           (plist-get pr :created-at)))))
            86400)))

(defun ghprs-report ()
  "Copy a Markdown report of PRs ready for review to the kill ring.
A PR is ready when it is the root of its stack (or standalone), is not a
draft, is awaiting review, and its required checks are not failing or
still running.

Works from any buffer by reading the dashboard's data."
  (interactive)
  (with-current-buffer (or (get-buffer "*ghprs*")
                           (user-error "No PR dashboard; run `ghprs' first"))
    (ghprs--report)))

(defun ghprs--report ()
  "Build the review report from the current buffer's PR data."
  (let (chunks (count 0))
    (dolist (repo (ghprs--repos))
      (let ((ready
             (seq-filter
              (lambda (pr)
                (and (not (plist-get pr :draft))
                     (eq (plist-get pr :review) 'pending)
                     (progn
                       (when (eq (plist-get pr :ci) 'loading)
                         (user-error "Required-check state still loading; retry shortly"))
                       (memq (plist-get pr :ci) '(pass none)))))
              (ghprs--roots (ghprs--repo-prs repo)))))
        (when ready
          (push (concat
                 (format "%s:\n" repo)
                 (mapconcat
                  (lambda (pr)
                    (cl-incf count)
                    (let ((days (ghprs--days-waiting pr)))
                      (format "- [%s](%s) (waiting %d day%s for a review)\n"
                              (plist-get pr :title) (plist-get pr :url)
                              days (if (= days 1) "" "s"))))
                  ready))
                chunks))))
    (unless chunks
      (user-error "No PRs are awaiting review with passing required checks"))
    (kill-new (string-join (nreverse chunks) "\n"))
    (message "Copied review report of %d PR%s" count (if (= count 1) "" "s"))))

(defun ghprs-open ()
  "Open the PR at point in the browser."
  (interactive)
  (if-let* ((url (get-text-property (point) 'ghprs-url)))
      (browse-url url)
    (user-error "No PR on this line")))

(defvar-keymap ghprs-mode-map
  :parent special-mode-map
  "RET" #'ghprs-open
  "n" #'next-line
  "p" #'previous-line
  "r" #'ghprs-report)

(define-derived-mode ghprs-mode special-mode "GHPRs"
  "Major mode showing all my open GitHub PRs.
\\{ghprs-mode-map}"
  (setq truncate-lines t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (ghprs--fetch (current-buffer)))))

;;;###autoload
(defun ghprs ()
  "Show a dashboard of all my open GitHub PRs."
  (interactive)
  (let ((buffer (get-buffer-create "*ghprs*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'ghprs-mode)
        (ghprs-mode)
        (let ((inhibit-read-only t))
          (insert "Loading…")))
      (ghprs--fetch buffer))
    (pop-to-buffer buffer)))

(provide 'ghprs)

;;; ghprs.el ends here
