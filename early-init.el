;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq frame-resize-pixelwise t)

(tool-bar-mode -1)
(setq
 frame-resize-pixelwise t
 ;; We set the font here to work around a bug that hides the echo area
 ;; when a font is set after the frame loads.
 default-frame-alist '((font . "Fira Code")
		       (vertical-scroll-bars . nil)
		       (horizontal-scroll-bars . nil))
 initial-frame-alist
      '((width . 0.5) (height . 1.0)
	(top . 0) (left . 1.0)))

  (setq package-enable-at-startup nil)
