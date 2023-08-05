;;; early-init.el --- Tangled from init.org -*- lexical-binding: t; -*-

;;; Commentary:

;; This file runs before much of Emacs's baked-in code.

;;; Code:

;; Minimize the GC during startup. This makes startup faster in exchange for a single
;; "stop the world" collection at the end of startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; We don't want to permanently disable garbage collection, so we re-enable garbage
;; collection after all packages have been loaded.
;;
;; Worth noting: `elpaca' runs asynchronously, and kicks off after `after-init-hook'.
(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

;; By default, Emacs thinks in terms of text columns and lines. This isn't great when
;; computing frame sizes for graphical purposes. We want to think in pixels on GUIs.
(setq frame-resize-pixelwise t)

;; It is important to set graphical elements in early-init.el, since otherwise they take
;; effect after Emacs initializes graphics and the window viably flickers or resizes
;; during startup.

(tool-bar-mode -1)
(menu-bar-mode -1)
(setq frame-resize-pixelwise t
      ;; We set the font here to work around a bug that hides the echo area
      ;; when a font is set after the frame loads.
      default-frame-alist '((font . "USER_FONT")
		            (vertical-scroll-bars . nil)
		            (horizontal-scroll-bars . nil))
      initial-frame-alist
      '((width . 0.5) (height . 1.0)
        (top . 0) (left . 1.0)))

;; We need to disable package.el, Emacs's default package manager. Since package.el sets
;; up existing packages before init.el runs, we need to do this in early-init.el.
(setq package-enable-at-startup nil)

;;; early-init.el ends here
