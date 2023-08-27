;;; watt.el --- Windows Ain't Tiny Trees -*- lexical-binding: t; -*-

;; Copyright (C) 2023 USER_NAME

;; Author: USER_NAME
;; URL: https://github.com/iwahbe/watt
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `watt' provides an abstraction layer on top of Emacs windows.

;;; Code:

(defun watt-move-right ()
  "Move to the window of the right of the point."
  (interactive)
  ;; Note: `watt-right' is not related to `window-right', which uses the window tree as
  ;; opposed to actual positions.
  (let ((right-window (watt--right-of (point))))
    (if (not right-window)
        (message "No right window found")
      (select-window right-window))))

(defun watt-move-left ()
  "Move to the window of the left of the point."
  (interactive)
  (let ((left-window (watt--left-of (point))))
    (if (or (not left-window))
        (message "No left window found")
      (select-window left-window))))

(defun watt-move-up ()
  "Move to the window above the point."
  (interactive)
  (let ((above-window (watt--up-of (point))))
    (if (or (not above-window))
        (message "No window above")
      (select-window above-window))))

(defun watt-move-down ()
  "Move to the window below the point."
  (interactive)
  (let ((below-window (watt--down-of (point))))
    (if (or (not below-window))
        (message "No window below")
      (select-window below-window))))

(defun watt--right-of (point)
  "The window to the right of POINT."
  (watt--window-direction
   (posn-at-point (line-beginning-position))
   (lambda (window x y)
     (cons (+ x (window-pixel-width window) 2) y))))

(defun watt--left-of (point)
  "The window to the left of POINT."
  (watt--window-direction
   (posn-at-point (line-beginning-position))
   (lambda (window x y)
     (cons (- x (or (car-safe (window-fringes window)) 0) 1) y))))

(defun watt--up-of (point)
  "The window that is above POINT."
  (watt--window-direction
   (posn-at-point point)
   (lambda (window x y)
     (cons (if (eq point (line-beginning-position))
               (+ x (car-safe (window-fringes window)))
             x)
           (- 0
              (window-header-line-height window) 10)))))

(defun watt--down-of (point)
  "The window that is below POINT."
  (watt--window-direction
   (posn-at-point point)
   (lambda (window x y)
     (cons (if (eq point (line-beginning-position))
               (+ x (car-safe (window-fringes window)))
             x)
           (+ (window-pixel-height window)
              (window-header-line-height window)
              10)))))

(defun watt--escape ()
  "Exit an instance of `watt' without performing an action."
  (interactive)
  (throw 'watt--escape nil))

(defun watt--window-direction (posn direction)
  "Return the window pointed to by moving DIRECTION from POSN.

POSN should satisfy `posnp'.

DIRECTION should be a function callable on a WINDOW x-cord y-cord
and return a new (x-cord . y-cord)."
  (when-let* ((from-window (posn-window posn))
              (x-y (posn-x-y posn))
              (x (car x-y))
              (y (cdr x-y))
              (new-x-y (funcall direction from-window x y))
              (new-x (+ (window-pixel-left from-window)
                        (car new-x-y)))
              (new-y (+ (window-pixel-top from-window)
                        (cdr new-x-y)))
              (w (when (and (wholenump new-x) (wholenump new-y))
                   (posn-window
                    (posn-at-x-y new-x new-y (window-frame from-window) t)))))
    (when (window-live-p w) w)))

(defvar watt-prefix-map
  (define-keymap :name "Watt"
    :suppress t
    "p"   #'watt-move-up
    "n"   #'watt-move-down
    "f"   #'watt-move-right
    "b"   #'watt-move-left
    "ESC" #'watt--escape)
  "The keymap used when `watt' is invoked.")

(defun watt ()
  "Perform actions on windows."
  (let ((initial (selected-window))
        done)
    (while (not done)


(provide 'watt)
;;; watt.el ends here
