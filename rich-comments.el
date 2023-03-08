;;; rich-comments.el --- Rich text in comments -*- lexical-binding: t; -*-

;;; Commentary:

;;-
;; Allow custom faces in fenced comments.
;;
;; The origin goal is to allow `variable-pitch' comments to encourage long form explinations of the content of my `init.el'.
;; A literate config provided excelent readability, but poor writeability.  A pure `emacs-lisp' file gave excellent writeability,
;; but so-so readability.  Here I'm trying to get the best of both worlds.
;;-

;;; Code:

(require 'font-lock)

;;;###autoload
(define-minor-mode rich-comments-mode
  "Provide rich fontification to fenced comments."
  :lighter " ;Rich;"

  (if rich-comments-mode
      (rich-comments--enable)
    (rich-comments--disable))
  (font-lock-flush))

(defface rich-comments-fence-face
  '((t . (:inherit font-lock-string-face :extend t)))
  "The face used for rich-comment's comment fences.")

(defface rich-comments-body-face
  '((t . (:inherit (font-lock-doc-face variable-pitch) :extend t :height 1.2)))
  "The face used for rich-comment's comment bodies.")

;;-
;; Here we set up custom `rich-comments' highlighting within body blocks.
;;
;; TODO We want the following changes:
;;
;; - Text between ?` and ?' is highlighted in a fixed width font.
;;
;; - If it is a symbol, it is highlighted according to its definition. That means that the
;;   appearance of `foo' will depend on if `foo' is a constant, variable or function. This
;;   behavior should mirror how it works in the rest of the buffer.
;;-

(defmacro rich-comments-defface-code (&rest faces)
  "Define faces for quoted code with a `rich-comments' block.
Each element in FACES is expect to be a symbol."
  `(progn
     ,@(mapcar
	(lambda (face)
	  (let ((new-face (intern (concat "rich-comments-" (symbol-name face) "-face")))
		(old-face (intern (concat "font-lock-" (symbol-name face) "-face"))))
	    (unless (facep old-face)
	      (user-error "Attempting to define equivalent of non-existent face: %s" old-face))
	    `(defface ,new-face
	       '((t . '(:inherit (,old-face fixed-pitch) :extend t)))
	       ,(concat "The `rich-comments' equivalent of `" (symbol-name old-face) "-face'."))))
	faces)))

(rich-comments-defface-code
  constant variable-name)

(with-eval-after-load 'flyspell
  ;; `flyspell' uses faces to determine what should be commented on.  We need to add
  ;; `rich-comments-body-face' to that list, since it is effectively an extended doc
  ;; comment.
  (eval-when-compile (defvar flyspell-prog-text-faces))
  (push 'rich-comments-body-face flyspell-prog-text-faces))

(defvar rich-comments-font-lock-keywords
  `((rich-comments-match-fence 0 'rich-comments-fence-face t)
    (rich-comments-match-body 0 'rich-comments-body-face t))
  "The keyword locking for `font-lock-keywords'.")

(defun rich-comments--enable ()
  "Enable `rich-comments-mode'."
  (add-to-list 'font-lock-extend-region-functions
	       #'rich-comments--font-lock-extend-region)
  (font-lock-add-keywords nil rich-comments-font-lock-keywords))

(defun rich-comments--disable ()
  "Disable `rich-comments-mode'."
  (setq font-lock-extend-region-functions
	(delq #'rich-comments--font-lock-extend-region
	      font-lock-extend-region-functions))
  (font-lock-remove-keywords nil rich-comments-font-lock-keywords))

(defun rich-comments-match-fence (bound)
  "Set `match-data' to describe the first fence before BOUND.
The search starts from `point'."
  (let (found origin)
    (save-excursion
      (while (and (not found) (< (point) bound)
		  (re-search-forward "^;;;?-.*\n" bound t))
	(setq origin (point)
	      found (save-match-data
		      ;; go back 1, moving before the matched \n
		      (goto-char (1- (point)))
		      (rich-comments--region-bounds)))
	(goto-char origin)))
    (when found
      (goto-char origin)
      found)))

(defun rich-comments-match-body (bound)
  "Set `match-data' to describe the next line of a body bounded by two fences.
This intentionally does not match any leading `?;' within the body.
The search terminates at BOUND."
  (let ((origin (point)))
    (if (when-let* ((region (save-excursion (rich-comments-match-fence bound)))
		    (body-start (progn
				  (goto-char (car region))
				  (forward-line)
				  (point)))
		    (body-end (progn
				(goto-char (cdr region))
				(forward-line -1)
				(end-of-line)
				(1+ (point)))))
	  (when (< origin body-end)
	    (goto-char (max origin body-start))
	    (while (and (char-after) (eq (char-after) ?\;))
	      (forward-char))
	    (set-match-data (list (point-marker)
				  (progn
				    (end-of-line)
				    (goto-char (1+ (point)))
				    (point-marker))))
	    t))
	t
      (goto-char origin)
      nil)))

(defun rich-comments--font-lock-extend-region ()
  "Extend the font lock region to include full pretty comments."
  ;; These globals are only defined when the buffer is being fontified.
  ;; See `font-lock-extend-region-functions' for details.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (save-match-data
      (let* ((beginning-bounds (rich-comments--region-bounds font-lock-beg))
	     (start (car-safe beginning-bounds))
	     (end (cdr-safe beginning-bounds))
	     changed)
	(when (and start (< start font-lock-beg))
	  (setq font-lock-beg start
		changed t))
	(if (and end (> end font-lock-end))
	    ;; `font-lock-end' is part of the already identified region, so that region
	    ;; should define the end.
	    (setq font-lock-end end
		  changed t)
	  ;; `font-lock-end' is not part of the identified region, so we need to check if it
	  ;; is part of another region.
	  (when-let ((end (cdr-safe (rich-comments--region-bounds font-lock-end))))
	    (when (> end font-lock-end)
	      (setq font-lock-end end
		    changed t))))
	changed))))


(defun rich-comments--region-bounds (&optional point)
  "The bounds of the rich-comments region at point.
POINT defaults to `point'.
nil if no comment is found.
This function is not excursion safe."
  (when point
    (goto-char point))
  (when (rich-comments--comment-p)
    ;; We are in a comment, so it is possible we are in a rich-comment region.
    (let ((origin (point)) above-valid above-border below-border
	  exit-loop)
      ;; The search above is bounded only by the size of the comment, since successive
      ;; fences invalidate the previous fence.
      (while (and (rich-comments--comment-p) (not exit-loop))
	(when (rich-comments--border-p t)
	  ;; We only want to keep track of the location of the nearest fence.
	  (unless above-border
	    (setq above-border (point)))
	  ;; An above fence is only valid if it is not the bottom fence of yet another
	  ;; fence.
	  (setq above-valid (not above-valid)))
	(let ((current-line (line-number-at-pos)))
	  (beginning-of-line 0)
	  (when (eq current-line (line-number-at-pos))
	    ;; We are at the top, so we should stop now
	    (setq exit-loop t))))
      (goto-char origin)
      (if (and (not above-valid)
	       above-border
	       (eq (line-number-at-pos above-border)
		   (line-number-at-pos origin)))
	  ;; We are at a fence that is not valid (because is matched with a upper fence).
	  ;; That means we are at the bottom of a valid block.
	  (progn
	    (setq below-border above-border)
	    (beginning-of-line 0) ; Go up 1 line
	    (while (not (rich-comments--border-p nil))
	      (beginning-of-line 0))
	    (cons (point) below-border))
	(when above-valid
	  ;; We only need to bother searching down if the search up was successful
	  (while
	      (and
	       (progn
		 (beginning-of-line 2)
		 (rich-comments--comment-p))
	       (not below-border))
	    (when (rich-comments--border-p t)
	      (setq below-border
		    (unless below-border
		      (point)))))
	  (when below-border
	    (cons above-border below-border)))))))

(defun rich-comments--comment-p ()
  "If point is on a leading ;."
  (beginning-of-line)
  (while (and (char-after)
	      (or
	       (eq (char-after) ?\ )
	       (eq (char-after) ?\t)))
    (forward-char))
  (and (char-after) (eq (char-after) ?\;)))

(defun rich-comments--border-p (assume-comment)
  "If point is on a possible rich-comments comment border.
ASSUME-COMMENT asserts that POINT is before the first ?\; and
`rich-comments--comment-p' returns non-nil."
  (when (or assume-comment (rich-comments--comment-p))
    (while (and (char-after) (eq (char-after) ?\;))
      (forward-char))
    (and (char-after) (eq (char-after) ?-))))

(provide 'rich-comments)

;;; rich-comments.el ends here
