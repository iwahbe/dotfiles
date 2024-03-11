;;; pulumi-schema -- special interactions for Pulumi package schemas

;;; Commentary
;;
;; For an authoritative definition of Pulumi schemas, see
;; https://www.pulumi.com/docs/using-pulumi/pulumi-packages/schema/.

(require 'jsonian)

;;;###autoload
(define-derived-mode pulumi-schema-mode jsonian-mode "Pulumi Schema"
  "A JSON mode specialized for consuming a Pulumi package Schema."
  (add-hook 'xref-backend-functions #'pulumi-schema--xref-backend nil t))

(defun pulumi-schema--xref-backend ()
  "A backend to be used by `xref-backend-functions'."
  'pulumi-schema)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'pulumi-schema)))
  (when-let ((pos (jsonian--string-at-pos)))
    (buffer-substring-no-properties
     (1+ (car pos)) (1- (cdr pos)))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'pulumi-schema)) identifier)
    (save-excursion
      (when (pulumi-schema--follow-link identifier)
        (list
         (xref-make "Definition"
                    (xref-make-buffer-location (current-buffer) (point)))))))

(defun pulumi-schema--follow-link (ref)
  "Goto the definition of REF in the current buffer.

If no definition can be found, nil is returned."
  (cond
   ((string-prefix-p "#/types/" ref)
    (jsonian-find (concat ".types[\"" (string-remove-prefix "#/types/" ref) "\"]")))
   ((string-search "/" ref)
    (user-error "Unable to handle a foreign schema reference"))))

(provide 'pulumi-schema)
