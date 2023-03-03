;;; rich-comments.el --- Rich text in comments -*- lexical-binding: t; -*-

;;; Commentary:

;; Rich text in code comments.

;;; Foo
;;-
;; foo
;; this also
;; bizz
;;-

;;; Code:

(require 'font-lock)

;;;###autoload
(define-minor-mode rich-comments-mode
  "Provide rich fontification to fenced comments."
  :lighter " ;Rich;"

  (if rich-comments-mode
      (rich-comments--enable)
    (rich-comments--disable)))

(defvar rich-comments-font-lock-keywords
  `((rich-comments-match-fence 'font-lock-string-face prepend)
    (rich-comments-match-body 'font-lock-function-name-face prepend))
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
		  (re-search-forward "^;;\\(;?\\)-.*" bound t))
	(setq origin (point)
	      found (save-match-data (rich-comments--region-bounds)))
	(goto-char origin)))
    (when found
      (goto-char origin)
      found)))

(defun rich-comments-match-body (bound)
  (ignore bound))

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
	(when (< start font-lock-beg)
	  (setq font-lock-beg start
		changed t))
	(if (> end font-lock-end)
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
    (let ((origin (point)) above-valid above-border below-border)
      ;; The search above is bounded only by the size of the comment, since successive
      ;; fences invalidate the previous fence.
      (while (rich-comments--comment-p)
	(when (rich-comments--border-p t)
	  ;; We only want to keep track of the location of the nearest fence.
	  (unless above-border
	    (setq above-border (point)))
	  ;; An above fence is only valid if it is not the bottom fence of yet another
	  ;; fence.
	  (setq above-valid (not above-valid)))
	(beginning-of-line 0))
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
  "If point is on a leading 2 ; or 3 ; depth comment."
  (beginning-of-line)
  (while (and (char-after)
	      (or
	       (eq (char-after) ?\ )
	       (eq (char-after) ?\t)))
    (forward-char))
  (and (char-after) (char-after (1+ (point)))
       (eq (char-after) ?\;) (eq (char-after (1+ (point))) ?\;)))

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
