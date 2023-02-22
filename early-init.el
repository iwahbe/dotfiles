;;; -*- lexical-binding: t; -*-

;;; Commentary:

;; early-init.el should be limited to actions that *must* occur early
;; in the initialization process.
;;
;; This includes:
;; - disabling package.el, Emacs's standard package manager.
;; - re-routing the eln-cache to its appropriate place.
;; - disabling the gc during startup.
;; - setting the initial size of the frame.

;;; Code:

(defun =slugify-to-file-name (name)
  "Transform NAME to a normal and ascetic file name."
  (when (string-empty-p name)
    (error "Cannot slugify empty string"))
  (string-replace
   "/" "-"
   (string-replace " " "-" name)))

(defvar =cache-directory (expand-file-name ".cache" user-emacs-directory)
  "The directory where a system local cache is stored.")

(defun =cache-subdirectory (domain)
  "A stable directory to cache files from DOMAIN in."
  (expand-file-name (concat (=slugify-to-file-name domain) "/") =cache-directory))

(defun =cache-file (domain)
  "A stable file name for DOMAIN."
  (expand-file-name (=slugify-to-file-name domain) =cache-directory))

(setq package-enable-at-startup nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (=cache-subdirectory "eln-cache")))

;; Disable the GC during start-up
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; Re-enable the GC after elpaca has loaded all its packages
(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))


;; Disable graphic elements so they are not displayed and then
;; reverted.
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq
 frame-resize-pixelwise t
 initial-frame-alist
      '((width . 0.5) (height . 1.0)
	(top . 0) (left . 1.0)))

;;; early-init.el ends here
