;;; gtq.el --- GoTo Quickly -*- lexical-binding: t; -*-

;;; Commentary:
;; gtq is a library to quickly move the cursor around.  It is inspired
;; by evil's philosophy of combining nouns and verbs into easily
;; memorable Emacs commands.

;;; Code:

(require 'cl-lib)

(defun gtq--read-key (display-state)
  "Read a key from the user.
DISPLAY-STATE is show while reading."
  (read-key (concat display-state ": ")))

(defun gtq--state-flip (state prop)
  "Flip the value of PROP in STATE."
  (let ((v (plist-get state prop)))
    (plist-put state prop (not v))))

(defun gtq--state-format (noun state)
  "Format NOUN & STATE for human consumption."
  (concat "["
   (if (plist-get state :inner) "I" "i")
   (if (plist-get state :pair) "P" "p")
   (if (plist-get state :backward) "B" "b")
   "] " (gtq-noun-name noun)))

(cl-defstruct (gtq-noun
	       (:constructor nil)
	       (:constructor gtq-noun-new
			     (name search &key within)))
  "A noun in the gtq language, describing a thing that can be searched for"
  (name nil :documentation "The name of the kind of search.")
  (search nil
   :documentation "A function that takes (NEEDLE FROM TO) and returns the bounds of
the object as a tuple (start . end). If not object is found, nil
should be returned.  Bounds are considered starting from
`char-before' and ending at `char-after'. This means that a
character is defined with (n . n) for some n.")
  (within (lambda (x) (cons x x))
   :documentation "A function that maps a within query (NEEDLE)
 and returns an adjusted (NEEDLE-BACK . NEEDLE-FRONT)."))

(defvar gtq--noun-char
  (gtq-noun-new
   "Char" (lambda (needle from to)
	    (let (found)
	      (if (<= from to)
		  ;; We are searching forward
		  (while (and (< from to) (not found))
		    (if (= (char-after from) needle)
			(setq found from)
		      (cl-incf from)))
		(while (and (> from to) (not found))
		  (if (= (char-after from) needle)
		      (setq found from)
		    (cl-decf from))))
	      (when found
		(cons found (1+ found)))))
   :within (lambda (needle)
	     (pcase needle
	       ((or 40 41) ;; ( and ) respectively
		(cons 40 41))
	       ((or 91 93) ;; [ and ] respectively
		(cons 91 93))
	       ((or 123 125) ;; { and } respectively
		(cons 123 125))
	       (_ (cons needle needle)))))
  "The definition of a character noun.")

(defvar gtq--noun-thing
  (gtq-noun-new
   "Thing" (lambda (needle from to)
	     (when-let ((bounds
			 (bounds-of-thing-at-point
			  (pcase needle
			    (34 ;; "
			     'string)
			    (100 ;; d
			     'defun)))))
	       (if (<= from to)
		   (cons (cdr bounds) (cdr bounds))
		 (cons (car bounds) (car bounds))))))
  "Project Emacs's `thing-at-point' framework into `gtq'.")

(defvar gtq-nouns `(3 ;; \C-c
		    ,gtq--noun-char
		    20 ;; \C-t
		    ,gtq--noun-thing)
  "A plist of keybinds to noun definitions available within a search.")

(defmacro gtq-defun-command (name &rest state)
  "Create new gtq command called NAME with and initial STATE.
The first cell may be a docstring."
  (declare (indent defun))
  `(defun ,name ()
     ,@(when (stringp (car-safe state))
	 (let ((docstring (car state)))
	   (setq state (cdr state))
	   (list docstring)))
     (interactive)
     (gtq--goto (list ,@state))))

(defun gtq--goto (state)
  "Move point to the selected character, with the search initialized to STATE."
  (let ((noun (cadr gtq-nouns))
	stop)
    (while (not stop)
      (pcase (gtq--read-key (gtq--state-format noun state))
	((or 7  ;; \C-g
	     27 ;; ESC
	     )
	 (setq stop t))
	(1 ;; \C-a
	 (gtq--state-flip state :inner))
	(16 ;; \C-p
	 (gtq--state-flip state :pair))
	(2 ;; \C-b
	 (gtq--state-flip state :backward))
	((pred
	  (lambda (c)
	    (when-let ((new-noun (plist-get gtq-nouns c)))
	      (unless (gtq-noun-p new-noun)
		(error "%s is not a `gtq-noun'" new-noun))
	      (setq noun new-noun)
	      t))))
	(c
	 (setq stop t)
	 (let ((inner (plist-get state :inner))
	       (backward (plist-get state :backward))
	       (pair (plist-get state :pair)))
	   (if pair
	       (if-let ((points (funcall (gtq-noun-within noun) c))
			(back (funcall (gtq-noun-search noun) (car points) (point) (point-min)))
			(front (funcall (gtq-noun-search noun) (cdr points) (point) (point-max))))
		   (progn
		     (goto-char (funcall (if inner #'car #'cdr) front))
		     (push-mark (funcall (if inner #'cdr #'car) back) t t))
		 (unless back
		   (user-error "Back search failed"))
		 (unless front
		   (user-error "Front search failed")))
	     (let ((needle (funcall (gtq-noun-search noun) c (point) (if backward (point-min) (point-max)))))
	       (unless t (user-error "Search failed"))
	       (goto-char
		(if inner
		    (car needle)
		  (cdr needle)))))))))))

(gtq-defun-command gtq-goto
  "Goto the next character pressed."
  :inner nil :pair nil :backward nil)

(provide 'gtq)

;;; gtq.el ends here
