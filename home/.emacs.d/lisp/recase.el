;;; recase.el -- Case conversions between kebab-case, snake_case and more -*- lexical-binding: t -*-

;;; Commentary:

;; This library is part of iwahbe's dotfile config.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))

(defgroup recase nil
  "Case conversions on the region."
  :prefix "recase-"
  :group 'text)

(defvar recase-case-list
  '(recase-kebab
    recase-upper)
  "The list of available cases.")

;; Case selection

(defun recase-region (&optional beginning end from to)
  "Re-case the region (BEGINNING,END), converting FROM to TO.

The body of the function is as simple as (apply to (parse from
region)).  The bulk of the complexity is providing high quality
completions while the user is choosing FROM and TO."
  (interactive
   (let* ((beginning (region-beginning))
          (end (region-end))
          (s (buffer-substring-no-properties beginning end))
          (annotate-from (lambda (case)
                           (mapconcat (apply-partially #'format " %S")
                                      (recase-parse (intern-soft case)
                                                    s))))
          (from (intern-soft (completing-read
                              "From: "
                              (recase--completion
                               recase-case-list
                               :annotation-function annotate-from
                               :display-sort-function
                               (lambda (collection)
                                 (seq-sort-by (lambda (case) (recase-is (intern-soft case) s))
                                              #'>
                                              collection)))
                              nil t)))
          (annotate-to (lambda (case)
                         (format " %S"
                                 (recase-apply
                                  (intern-soft case)
                                  (recase-parse from
                                                s)))))
          (to (intern-soft (completing-read
                            "To: "
                            (recase--completion
                             recase-case-list
                             :annotation-function annotate-to)
                            nil t))))
     (list beginning end from to)))
  (replace-region-contents
   beginning end
   (lambda () (recase-apply to (recase-parse from s)))))

(cl-defun recase--completion (collection &key annotation-function display-sort-function)
  "Allow specifying completion metadata for completion functions on COLLECTION.

ANNOTATION-FUNCTION is passed as the annotation function for the completion.
DISPLAY-SORT-FUNCTION is passed as the sort function for the completion."
  (lambda (str pred flag)
    (pcase flag
      ;; This specifies a request for information about the state of the current
      ;; completion.  The return value should have the form ‘(metadata . ALIST)’, where
      ;; ALIST is an alist whose elements are described below.
      ('metadata
       (list 'metadata
             (cons 'annotation-function annotation-function)
             (cons 'display-sort-function display-sort-function)))
      ;; This specifies an ‘all-completions’ operation.  The function should return a list
      ;; of all possible completions of the specified string.
      ('t (all-completions str collection pred))
      ('lambda (test-completion str collection pred))
      ((pred null) (try-completion str collection pred))
      (`(boundaries . ,suffix) (completion-boundaries str collection pred suffix)))))

;; Helper methods for defining cases

(defun recase--score-on (s rules)
  "Calculate a score on S by applying RULES.

RULES is a list of (criteria . delta) where if criteria is found
then delta is added to the score.

Criteria can be either a string or a function callable on a character."
  (let ((score 50))
    (seq-do (lambda (rule)
              (pcase (car rule)
                ((pred stringp)
                 (when (string-search (car rule) s)
                   (cl-incf score (cdr rule))))
                ((pred functionp)
                 (when (seq-filter (car rule) s)
                   (cl-incf score (cdr rule))))))
            rules)
    score))

(defun recase--upercase-char-p (c)
  "If C is uppercase."
  (get-char-code-property c 'lowercase))

(defun recase--lowercase-char-p (c)
    "If C is lowercase."
  (get-char-code-property c 'uppercase))

;; kebab-case

(cl-defmethod recase-parse ((_ (eql 'recase-kebab)) s)
  "Parse a kebab-case string S."
  (seq-map #'downcase (string-split s "-" t)))

(cl-defmethod recase-apply ((_ (eql 'recase-kebab)) s)
  "Turn a parsed string S into a kebab-case string."
  (string-join s "-"))

(cl-defmethod recase-is ((_ (eql 'recase-kebab)) s)
  "Test if S is a kebab-case string."
  (recase--score-on
   s '(("-" . +30)
       ("_" . -20)
       (" " . -20)
       (#'recase--upercase-char-p . -20))))

;; Upper Case

(cl-defmethod recase-parse ((_ (eql 'recase-upper)) s)
  "Parse a upper-case string S."
  (seq-map #'downcase (string-split s " " t)))

(cl-defmethod recase-apply ((_ (eql 'recase-upper)) s)
  "Turn a parsed string S into a upper-case string."
  (string-join
   (seq-map (lambda (c)
              (concat (capitalize (substring c nil 1))
                      (substring c 1))
              )
            s)
   " "))

(cl-defmethod recase-is ((_ (eql 'recase-upper)) s)
  "Test if S is a kebab-case string."
  (let ((score
         (recase--score-on
          s '(("-" . -20)
              ("_" . -20)
              (" " . +20)))))

    (seq-do
     (lambda (part)
       (if (recase--upercase-char-p (seq-first part))
           (when (seq-filter #'recase--lowercase-char-p (seq-rest part))
             (cl-incf score +10))
         (cl-incf score -5)))
     (string-split s " "))
    score))

(provide 'recase)
;;; recase.el ends here
