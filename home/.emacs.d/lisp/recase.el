;;; recase.el -- Case conversions between kebab-case, snake_case and more -*- lexical-binding: t -*-

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
          (b (current-buffer))
          (annotate-from (lambda (case)
                           (mapconcat (apply-partially #'format " %S")
                                      (recase-parse (intern-soft case)
                                                    (with-current-buffer b
                                                      (buffer-substring-no-properties beginning end))))))
          (from (intern-soft (completing-read
                              "From: "
                              (recase--completion
                               recase-case-list
                               :annotation-function annotate-from)
                              nil t)))
          (annotate-to (lambda (case)
                         (format " %S"
                                 (recase-apply
                                  (intern-soft case)
                                  (recase-parse from
                                                (with-current-buffer b
                                                  (buffer-substring-no-properties beginning end)))))))
          (to (intern-soft (completing-read
                            "To: "
                            (recase--completion
                             recase-case-list
                             :annotation-function annotate-to)
                            nil t))))
     (list beginning end from to)))
  (replace-region-contents
   beginning end
   (lambda ()
     (recase-apply
      to
      (recase-parse from (buffer-substring-no-properties (point-min) (point-max)))))))

(defun recase--completion (collection &keys annotation-function)
  "Allow specifying completion metadata for basic completion functions on COLLECTION.

ANNOTATION-FUNCTION is passes as the annotation function for the completion."
  (lambda (str pred flag)
    (pcase flag
      ;; This specifies a request for information about the state of the current
      ;; completion.  The return value should have the form ‘(metadata . ALIST)’, where
      ;; ALIST is an alist whose elements are described below.
      ('metadata
       (list 'metadata
             (cons 'annotation-function annotation-function)))
      ;; This specifies an ‘all-completions’ operation.  The function should return a list
      ;; of all possible completions of the specified string.
      ('t (all-completions str collection pred))
      ('lambda (test-completion str collection pred))
      ((pred null) (try-completion str collection pred))
      (`(boundaries . ,suffix) (completion-boundaries str collection pred suffix)))))

;; kebab-case

(cl-defmethod recase-parse ((_ (eql 'recase-kebab)) s)
  "Parse a kebab-case string S."
  (seq-map #'downcase (string-split s "-" t)))

(cl-defmethod recase-apply ((_ (eql 'recase-kebab)) s)
  "Turn a parsed string S into a kebab-case string."
  (string-join s "-"))

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

(provide 'recase)
;;; recase.el ends here
