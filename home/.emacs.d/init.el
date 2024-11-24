;;; init.el --- Tangled from init.org -*- lexical-binding: t; -*-

;;; Commentary:

;; This file was tangled as part of iwahbe's Emacs config.

;;; Code:

;; we want lexical bindings even when developing interactively. This doesn't do anything
;; when a file is loaded, but it does effect what happens when =eval-last-sexp= is used.
(setq-default lexical-binding t)

;; Set the custom file as soon as possible, to prevent emacs from writing to this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; We do not load the custom file, since we want all customizations to occur here.
;;
;;    (when (file-exists-p custom-file) (load custom-file))


;; Add some helpers to the load path.
;;
;; We don't add immediately because it has a noticeable performance impact on load init
;; time: 0.015 seconds.
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))))

;; I declare a custom helper macro for adding hooks. It simplifies quoting, and allows
;; multiple hooks to be attached in a single sexp.
(defmacro i/add-hook (mode &rest hooks)
  "Attach multiple HOOKS to a MODE hook.
It is optional to quote MODE."
  (declare (indent defun))
  `(progn
     ,@(mapcar
	(lambda (hook) `(add-hook
			 ,(if (eq (car-safe mode) 'quote)
			      mode
			    `(quote ,mode))
			 ,hook))
	hooks)))

(defun i/advise-once (symbol where function &optional props)
  "`advise-add' a function that will only trigger once.

SYMBOL, WHERE, FUNCTION and PROPS are all treated the same as by `advise-add'."
  ;; Borrowed from https://emacs.stackexchange.com/questions/26251/one-time-advice
  (advice-add symbol :after (lambda (&rest _) (advice-remove symbol function)))
  (advice-add symbol where function props))

(require 'cl-lib)
(cl-defmacro i/define-keymap (name &rest keys &key global parent bind after &allow-other-keys)
  "Define a self-describing keymap called NAME consisting of KEYS.

To bind the keymap to a global value, pass a binding to GLOBAL.

To have the defined keymap inherit from a parent, set PARENT.

If AFTER is specified, it should be a feature.  Other variables
will only be evaluated once AFTER is loaded.

BIND takes a list (keymap key) pair, and calls `(keymap-set keymap key name)'

KEYS are (binding function) lists."
  (declare (indent defun))
  (let (description)
    ;; Grab the description if present
    (when-let (d (car-safe keys))
      (when (stringp d)
        (setq description d
              keys (cdr keys))))
    ;; Process keys, removing known keywords and error-ing on unknown keywords.
    (setq keys (let (args)
                 (while keys
                   (if (keywordp (car keys))
                       (progn
                         (pcase (car keys)
                           (:global (setq global (cadr keys)))
                           (:parent (setq parent (cadr keys)))
                           (:bind (setq bind (cadr keys)))
                           (:after (setq after (cadr keys)))
                           (other (error "Unknown property %s" other)))
                         (setq keys (cddr keys)))
                     (push (car keys) args)
                     (setq keys (cdr keys))))
                 (reverse args)))
    (let ((c `(let ((m (define-keymap
                         :parent ,parent
                         ,@keys)))
                (defvar ,name m ,description)
                (setq ,name m)
                ,(when global `(keymap-global-set ,global ,name))
                ,(when bind `(keymap-set ,(car bind) ,(cadr bind) ,name))
                ,name)))
      (if after
          `(with-eval-after-load ,after
             ,c)
        c))))

(defmacro i/dbg (form)
  "Print FORM => RES where res is what FORM evaluate to.
Return RES."
  ;; This is the kind of function that is helpful to have around, but shouldn't make it
  ;; into "production" code.
  `(let ((res ,form)) (message "dbg: %s => %s" '(,@form) res) res))

(defmacro i/profile (&rest body)
  "Run `profiler' on BODY, then report the result."
  `(let ((time (current-time)))
     (require 'profiler)
     (profiler-reset) ; Disables the profiler, if running. Resets logs.
     (profiler-start 'cpu+mem)
     ,@body
     (profiler-stop)
     (profiler-report)
     (message "Finished in %.06f seconds" (float-time (time-since time)))))


;;; Cache

(defvar i/cache-directory (expand-file-name ".cache" user-emacs-directory)
  "The directory where a system local cache is stored.")

(defun i/cache-subdirectory (domain &optional ensure)
  "A stable directory to cache files from DOMAIN in.

If ENSURE is non-nil, create the file if it does not exist."
  (let ((path (expand-file-name (concat domain "/") i/cache-directory)))
    (when ensure
      (make-directory path t))
    path))

(defun i/cache-file (file &optional domain)
  "A stable file name for FILE, located in DOMAIN if provided."
  (expand-file-name file
		    (if domain
			(let ((s (i/cache-subdirectory domain)))
			  (unless (file-executable-p s)
			    (mkdir s))
			  s)
		      i/cache-directory)))

;; Following good practice, we maintain an =assets= folder, where we store /heavy/ files.


(defvar i/assets-directory (expand-file-name "assets" user-emacs-directory)
  "The directory containing large runtime assets, such as images.")



;;; Package Management

;; I use https://github.com/progfolio/elpaca as my package manager for Emacs.

(defvar elpaca-directory (i/cache-subdirectory "elpaca"))

;; This is the install script straight from the elpaca repo:


(defvar elpaca-installer-version 0.7)
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;;; Fixup init from seq already being loaded.

;; https://github.com/progfolio/elpaca/issues/216#issuecomment-1868747372
(defun i/elpaca-unload-seq (e)
  "Unload seq before continuing the elpaca build, then continue to build the recipe E."
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun i/elpaca-unload-transient (e)
  "Unload seq before continuing the elpaca build, then continue to build the recipe E."
  (and (featurep 'transient) (unload-feature 'transient t))
  (elpaca--continue-build e))

(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                                          elpaca--pre-built-steps
                                        elpaca-build-steps))
                              (list 'i/elpaca-unload-seq 'elpaca--activate-package))))

(elpaca `(transient :build ,(append (butlast (if (file-exists-p (expand-file-name "transient" elpaca-builds-directory))
                                          elpaca--pre-built-steps
                                        elpaca-build-steps))
                             (list 'i/elpaca-unload-transient 'elpaca--activate-package))))

;; We redefine `display-startup-echo-area-message', since there is no built in way to
;; disable it. To make sure I am cognizant of start-up time, I have this set to display
;; the load time of Emacs.


(defun display-startup-echo-area-message ()
  "Override the default help message by redefining the called function."
  (message "Loaded %s packages in %f seconds"
	   (length (elpaca--queued))
	   (float-time
	    (time-subtract
	     elpaca-after-init-time
	     before-init-time))))



;;; Splash Screen

;; Half of the point customizing Emacs is making the splash screen look fancy. It's an
;; opportunity for pointless optimization and I couldn't resist.


;; I like the simplicity of a random Emacs-related image on screen. I'm not willing to
;; give up on supporting text only situations (such as in the terminal). To that end,
;; there is a fall-back option to display only text.

(defun i/splash-buffer (&optional window)
  "The splash screen.
It is assumed that the splash screen will occupy the whole frame
when it is created.
WINDOW is passed via `window-size-change-functions'.  It is ignored."
  (ignore window)
  (if (not (memq (current-buffer)
                 (list (get-buffer "*scratch*")
	               (get-buffer "*Splash Screen*"))))
      ;; If the current buffer is not *scratch*, then Emacs was opened
      ;; onto a file, so we should just display that file.
      (current-buffer)
    (with-current-buffer (get-buffer-create "*Splash Screen*")
      (special-mode)
      (make-local-variable 'window-size-change-functions)
      (add-to-list 'window-size-change-functions #'i/splash-buffer)
      (let ((inhibit-read-only t))
	(unless (eq (buffer-size) 0)
	  (erase-buffer))
	(if (and (display-graphic-p) (featurep 'image))
	    (i/splash-buffer--graphic)
	  (i/splash-buffer--text))
	(setq cursor-type nil)
	(goto-char (point-min))
	(setq mode-line-format nil)
	(current-buffer)))))

;; Emacs uses `initial-buffer-choice' to determine what buffer it should start in.
(setq initial-buffer-choice #'i/splash-buffer)

;; Here we want to insert a random image from our list of graphic banner images. Graphic
;; banner images are stored in the "assets" folder. We define our list of images.

(defvar i/emacs-graphic-banners
  (mapcar (lambda (x) (expand-file-name x i/assets-directory))
	  '("gnu-head.svg"
	    "emacs-icon.svg"))
  "A list of graphical banners to open Emacs with.
Each element is expected to be the path to a SVG file.")

(defvar i/emacs-graphic-banner
  (nth (random (length i/emacs-graphic-banners))
       i/emacs-graphic-banners)
  "The randomly chosen graphic banner to use for this session.
This is calculated once, so it doesn't change during redisplay.")

;; We then define what a graphic splash buffer will be: A centered image 1/3 down the
;; frame.

(defun i/splash-buffer--graphic ()
  "Display the splash screen with graphics."
  (let* ((img
          ;; Each image is expected to take up 3/4 of the smallest dimension of screen
          ;; space.
          (if (>= (frame-pixel-width) (frame-pixel-height))
              (create-image
	       i/emacs-graphic-banner
	       nil nil :height (round (* (frame-pixel-height) 0.75)))
	    (create-image
	     i/emacs-graphic-banner
	     nil nil :width (round (* (frame-pixel-width) 0.75)))))
	 (img-size (image-size img))
	 (img-width (round (car img-size)))
	 (img-height (round (cdr img-size))))
    ;; We want to center the image around 1/3 down the
    ;; screen. Since the image insert holds the top of the
    ;; image, we need to adjust the insert point by adding
    ;; newlines.
    (insert (make-string (max (- (/ (frame-height) 3) (/ img-height 2)) 0) ?\n))
    ;; Likewise, we want to insert the image in the center of
    ;; the screen but the image inserts from the left. We pad
    ;; our insert point with spaces.
    (insert (make-string (max (- (/ (frame-width) 2) (/ img-width 2)) 0) ? ))
    (insert-image img nil nil nil t)))


;; Text banners are stored inline with in init.el. They were generated from
;; https://patorjk.com/software/taag/#p=display&f=Graffiti&t=Emacs.


(defvar i/emacs-text-banners
  '(("███████╗███╗   ███╗ █████╗  ██████╗███████╗"
     "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
     "█████╗  ██╔████╔██║███████║██║     ███████╗"
     "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
     "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
     "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
    ("  _______  ___      ___       __       ______    ________  "
     " /\"     \"||\"  \\    /\"  |     /\"\"\\     /\" _  \"\\  /\"       ) "
     "(: ______) \\   \\  //   |    /    \\   (: ( \\___)(:   \\___/  "
     " \\/    |   /\\\\  \\/.    |   /' /\\  \\   \\/ \\      \\___  \\    "
     " // ___)_ |: \\.        |  //  __'  \\  //  \\ _    __/  \\\\   "
     "(:      \"||.  \\    /:  | /   /  \\\\  \\(:   _) \\  /\" \\   :)  "
     " \\_______)|___|\\__/|___|(___/    \\___)\\_______)(_______/   "))
  "A list of non-graphical banners.
Each banner is expected to be a list of text, where each text
element is a single line.")

(defvar i/emacs-text-banner
  (nth (random (length i/emacs-text-banners)) i/emacs-text-banners)
  "The text banner to use for this session.
This is calculated once so it doesn't change during redisplay")

;; The display function is similar to the graphic version, aiming to put the text centered
;; 1/3 down the frame.

(defun i/splash-buffer--text ()
  "Display the splash screen with only text."
  (let ((banner i/emacs-text-banner)
	(empty-line "\n"))
    (dotimes (_ (- (/ (frame-height) 3) (/ (length banner) 2) 2))
      (insert empty-line))
    (mapc (lambda (x) (insert x "\n")) banner))
  (let ((fill-column (frame-width)))
    (center-region (point-min) (point-max))))



;;; Miscellaneous Customizations

(add-to-list 'warning-suppress-types
             '(undo discard-info)) ;; Don't warn when a large change is made to a buffer

;; Automatically keep buffers up to date with the underlying file:
(auto-revert-mode +1)

;; I understand what the scratch buffer does, so the explanation is not necessary.
(setq initial-scratch-message nil)

;; I don't need the additional delay of typing "es" or "o". "y" or "n" is sufficient.
(fset #'yes-or-no-p #'y-or-n-p)

;; Text editors should not make sound.
(setq ring-bell-function #'ignore)

;; Emacs disables /dangerous/ commands by default in interactive contexts. I like to live
;; dangerously (with copious use of version control).
(setq disabled-command-function nil)

;; In general, Emacs should follow symlinks. If it has reason to believe this is a bad
;; idea, it should warn but not halt my workflow.
(setq vc-follow-symlinks t)

;; Spaces vs tabs is the forever war. I have no stake in this race, except that some
;; environments struggle to correctly display tabs. Fewer struggle with spaces. If this
;; needs to be different for specific major modes, they can override it.
(setq-default indent-tabs-mode nil)

;; Recursive minibuffers are often helpful.
(setq enable-recursive-minibuffers t)

;; I expect the cursor to be static, and I prefer a bar over a block.
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

;; I need to know the line and column number.
(line-number-mode +1)
(column-number-mode +1)

;; This isolates the DEL keys from the region. To delete the region, `kill-region' should
;; be used.
(setq delete-active-region nil)

;; Emacs has support for native compilation of elisp code. This feature leads to a
;; noticeable speedup in performance dependent packages, such as `eglot' and
;; `jsonian'. Emacs compiles elisp code asynchronous in the background when a package is
;; loaded.

;; We don't want to see compilation errors pop up for existing packages we have, since
;; they are generally not actionable except by the package author. We instead shunt them
;; into the "*Warnings*" buffer.
(setq native-comp-async-report-warnings-errors 'silent)

;; I use `fill-paragraph' often. It defaults to /70/ characters, which is too conservative
;; for me.
(setq-default fill-column 90)

;; `save-place-mode' is a built-in global minor mode to save the position of point in a
;; buffer, and to persist that between sessions. It does that by writing each buffer
;; position to a file, and then referring to the file when a buffer is revisited. This is
;; fine, but we want to redirect the file to our cache.
(setq save-place-file (i/cache-file "places"))
(save-place-mode +1)

(setq savehist-file (i/cache-file "savehist"))
(savehist-mode +1)

;; Add smooth scrolling to GUI Emacs.
(cond
 ;; Introduced in Emacs 29.1. This is much smoother.
 ((functionp #'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode +1))
 ;; Introduced in Emacs 26.1
 ((functionp #'pixel-scroll-mode)
  (pixel-scroll-mode +1)))

;; Make files with shebangs executable at the file system level.
;;
;; Taken from https://www.masteringemacs.org/article/script-files-executable-automatically
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(defun i/elint-check-file (&optional file)
  "Byte compile FILE for errors compiler errors and warnings only."
  (interactive)
  (let* ((dst (make-temp-file "elint-file"))
         (byte-compile-dest-file-function (lambda (_) dst))
         (byte-compile-log-buffer (generate-new-buffer "*Compile-Log*")))
    (byte-compile-file (or file (buffer-file-name)))
    (delete-file dst)
    (message "Checked")))

(with-eval-after-load 'byte-compile
  (define-key emacs-lisp-compilation-mode-map "q" #'kill-current-buffer))



;;; Themes

;; Emacs uses a concept called a "theme" to control system appearance. Each theme applies
;; a layer of "face" description to the loaded buffer. The ordered list of enabled themes
;; is defined in the variable `custom-enabled-themes'.


(defvar i/default-themes '((light . tsdh-light)
                          (dark  . tsdh-dark))
  "Themes that the system is aware of.")

;; When I load a theme, I generally only want that theme to apply. I don't want the
;; previous theme to effect the current experience. To solve this, I define a `load-theme'
;; wrapper called `i/load-theme'.
(defun i/load-theme (theme)
  "Load THEME.

Other currently loaded themes are disabled."
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (append (mapcar #'symbol-name
				                  (custom-available-themes))
                                          (mapcar #'car i/default-themes))
                                  nil t))))
  (load-theme (alist-get theme i/default-themes theme) t)
  ;; Disable previous themes
  (mapc #'disable-theme (cdr custom-enabled-themes)))

(elpaca moe-theme
  (setq i/default-themes '((light . moe-light)
                          (dark  . moe-dark)))
  ;; Mac has a concept of light and dark mode at the system level. Emacs can be built with
  ;; hooks to support system appearance change. I want use these hooks when available.
  (if (boundp 'ns-system-appearance)
      ;; This hook will be called later during the startup process, so we don't need to
      ;; manually call `i/load-theme'.
      (i/add-hook ns-system-appearance-change-functions #'i/load-theme)
    ;; When there isn't any system input for the theme, we will just load the ='light=
    ;; theme by default.
    (i/load-theme 'light)))



;;; 1Password

;; I use 1Password for secrets management. I would like to be able to inject 1Password
;; secrets (like API keys) into my Emacs config from 1Password during startup.

;; Each call to the 1Password's =op= CLI tool is relatively slow (and requires a
;; authentication step). To avoid making multiple calls, we will model this as a queue and
;; flush system. When a variable is set, we add it to `1Password--forms', which acts as
;; our queue. Any time we need prompt access to a variable, we flush the entire queue
;; using "op inject" command, and then `eval-buffer' the result.


(defvar i/1Password--forms nil
  "A list of forms to evaluate after templating through 1Password.

After forms are evaluated, they are removed from this var.")

(defmacro i/1Password-setq (name path &rest consuming-funcs)
  "Declare that NAME should be filled from 1Password.
PATH is the 1Password path to the item.
CONSUMING-FUNCS is a list of functions that are know to consume NAME.

Each consuming func is adviced to ensure that values set with
`i/1Password-setq' are actually set before their consuming
functions are run."
  `(progn
     (setq i/1Password--forms
           (cons (list 'setq ',name ,(concat "{{ op://Personal/" path " }}"))
                 i/1Password--forms))
     ,@(mapcar (lambda (f)
                 `(advice-add ,f :before #'i/1Password--ensure))
               consuming-funcs)))

(defun i/1Password--flush ()
  "Evaluate `i/1Password--forms' after passing it through the 1Password CLI."
  (if-let ((op (executable-find "op")))
      ;; Create a temporary file for `op' to consume. We do this instead of piping into
      ;; `op' directly because `op' (as of April, 2023) doesn't allow itself to be created
      ;; without any stdin or input file. Programmatic consumption in Emacs goes
      ;;
      ;; 1. Create process
      ;; 2. Pipe in input
      ;;
      ;; Unfortunately, `op' errors out between steps (1) and (2) with the error
      ;;
      ;;   [ERROR] 2023/04/09 16:35:12 expected data on stdin but none found
      ;;
      ;; Writing to a file avoid this, at the cost of extra cleanup later. There is a
      ;; minimal security penalty to writing to a temp file, since the file we write only
      ;; contains placeholders for our secret values, not the values themselves.
      (let ((tmp (make-temp-file "1password.template")))
        (with-temp-file tmp
          (insert (mapconcat #'pp-to-string i/1Password--forms "\n")))
        (with-current-buffer (process-buffer
                              (make-process :name "1Password Init"
                                            :buffer "1Password"
                                            :command `(,op "inject" "--in-file" ,tmp)
                                            :sentinel #'i/1Password--sentinel))
          (set (make-local-variable 'i/1Password-template-file) tmp)))
    (warn "Could not find 1Password CLI")))

(defun i/1Password--sentinel (proc event)
  "The sentinel that controls the 1Password injection process.
PROC is the process and EVENT is the event that triggered the sentinel."
  (when (string-match "finished\n" event)
    ;; According to the manual for `process-exit-status', 256 indicates that a process was
    ;; closed "abnormally". I take this to mean an error was returned.
    (if (eq (process-exit-status proc) 256)
        (warn "1Password did not exit cleanly: %s"
              (let ((b (process-buffer proc)))
                (if (buffer-live-p b)
                    (with-current-buffer b
                      (buffer-substring (point-min) (point-max)))
                  "dead buffer")))
      (let ((b (process-buffer proc)))
        (unless (buffer-live-p b)
          (error "1Password buffer died unexpectedly"))
        ;; Since we know the input file is not being used (because the consuming process
        ;; exited), we can clean up.
        (delete-file (buffer-local-value 'i/1Password-template-file b))
        ;; We mark ourselves as done, freeing functions like `i/1Password--ensure' to exit.
        (setq i/1Password--forms nil)
        ;; We then evaluate the buffer. This doesn't present a race condition, since Emacs
        ;; doesn't interrupt running code, even with Sentinels.
        (eval-buffer b)
        (kill-buffer b)))))

(defun i/1Password-init-async ()
  "Asyncronously evaluate all declared 1Password variables."
  (i/1Password--flush))

(defun i/1Password--ensure (&rest args)
  "Syncronously ensure that 1Password has loaded all declared values.
ARGS allows this function to be used in hooks.  ARGS is ignored."
  (ignore args)
  (when i/1Password--forms
    (let ((progress (make-progress-reporter "Loading 1Password variables")))
      (i/1Password--flush)
      (while i/1Password--forms
        (progress-reporter-update progress)
        (sit-for 0.05))
      (message "1Password variables loaded"))))



;;; Help Messages

;; Emacs is famously introspectable. This is facilitated by the `describe-*'
;; functions. The built in introspection is excellent, but it can be improved by showing
;; more information about the values variables hold. The main improvement available is
;; showing the source code where the inspected item is defined. This is what
;; [[https://github.com/Wilfred/helpful][Wilfred/helpful]] does.

(elpaca helpful
  (keymap-global-set "<remap> <describe-function>" #'helpful-callable)
  (keymap-global-set "<remap> <describe-key>" #'helpful-key)
  (keymap-global-set "<remap> <describe-variable>" #'helpful-variable)
  (keymap-global-set "<remap> <describe-command>" #'helpful-command))


;; Emacs includes the excellent `eldoc-mode', which displays information about the object
;; at point in the echo area.  For larger (or more stable) documentation, Eldoc has
;; `eldoc-doc-buffer', which can hold unabridged documentation.

;; By default, both the buffer and echo area are used when the buffer is displayed. This
;; is probably a safe default, but it is not ideal. Ideally, the existence of the doc
;; buffer obviates the need for displaying in the echo area.

;; Don't display in the echo area if the doc buffer is visable
(setq eldoc-echo-area-prefer-doc-buffer t)


(elpaca which-key
  (setq which-key-idle-secondary-delay 0.0)
  (which-key-mode))



;;; GoTo Quickly

;; One thing I miss from VIM is the ability to easily jump between and around
;; characters. I have written a small package to accomplish this, called /GoTo Quickly/,
;; and I load that now.
(load (expand-file-name "gtq.el" user-emacs-directory))

;; It defines `gtq-goto', which brings up a model interface for quickly navigating among
;; characters.
(keymap-global-set "C-'" #'gtq-goto)



;;; Whitespace

;; Trailing whitespace is generally wrong. However, I need to be careful that I don't have
;; lots of whitespace diffs on shared files. `ws-butler-mode' handles this nicely.

;; Since the package is unmaintained, I use hlissner's (of Doom Emacs fame) fork, on the
;; grounds that since it is used by a popular distribution, it will probably work.
(elpaca (ws-butler :host github :repo "hlissner/ws-butler")
  ;; It is enabled everywhere.
  (ws-butler-global-mode))



;; By default, Emacs scatters backup and auto-save files over the directory in use.
;;
;; This is annoying at best, but painful in a world where most projects are under version
;; control.

(custom-set-variables
 ;; Move auto-save files (#foo#) and backup files (foo~) into the cache.
 '(auto-save-file-name-transforms `((".*" ,(i/cache-subdirectory "auto-save/") t)))
 '(backup-directory-alist `(("." . ,(i/cache-subdirectory "backup")))))



;;; Project Management

;; As far as I know, Emacs has two project management solutions:
;; https://github.com/bbatsov/projectile and project.el. Because project.el is in-trunk, I
;; have decided to use it. It works out of the box, but I still needed a couple of tweaks.

;; project.el caches which projects have been accessed, which needed to be re-mapped into
;; the cache directory.
(setq project-list-file (i/cache-file "projects"))

(defun i/project-switch-to-most-recent ()
  "Switch to the most recently used buffer in a project."
  (interactive)
  (if-let* ((proj (project-current nil project-current-directory-override))
            (bufs (seq-filter
                   (lambda (b) (and (not (eq b (current-buffer)))
                                    (not (string-prefix-p " " (buffer-name b)))))
                   (project-buffers proj))))
      (switch-to-buffer (car bufs))
    (message "Could not find any buffers to switch to in '%s'"
             project-current-directory-override)
    (sit-for 1)
    (project-switch-project project-current-directory-override)))

(require 'project)

(defun i/project-switch-project (dir)
  "\"Switch\" to another project by running an Emacs command.
The available commands are presented as a dispatch menu
made from `i/project-prefix-map'.

When called in a program, it will use the project corresponding
to directory DIR."
  ;; This approach unifies `project-switch-commands' and the `project-prefix-map'.
  (interactive (list (funcall project-prompter)))
  (let ((project-current-directory-override dir)
        (default-directory dir))
    ;; We use `which-key-show-keymap' to provide a "dispatch menu", and to block on input.
    (which-key-show-keymap 'i/project-prefix-map)
    ;; Typing a command on the `which-key' menu just dismisses the menu, so we recover the
    ;; command from `recent-keys' and replay it on our key map.
    (call-interactively
     (keymap-lookup i/project-prefix-map
                    (key-description
                     (vector (let ((recent (recent-keys)))
                               (aref recent (1- (length recent))))))))))

(i/define-keymap i/project-prefix-map
  :global "C-x p"
  :parent project-prefix-map
  "C-f" #'consult-find
  "g" #'consult-ripgrep
  "v" #'magit
  "t" #'i/project-vterm
  "r" #'i/project-switch-to-most-recent
  "p" #'i/project-switch-project)



;;; Completion

;; For completion at point, there are two main options: https://github.com/minad/corfu and
;; https://company-mode.github.io. Company is by far the 800 pound gorilla in this fight
;; with a large number of custom backends.  I have decided to use `corfu' instead, since
;; it integrates with Emacs's existing `completion-at-point-functions'.

(elpaca corfu
  (setq corfu-auto t          ;; Complete when available
        corfu-auto-delay 0    ;; Without any delay
        corfu-auto-prefix 1)  ;; Wait only for the first character

  (global-corfu-mode)

  (with-eval-after-load 'corfu
    ;; Finally, I want completion to not interfere with my normal typing. By default, return
    ;; finalizes a completion. I find this super disruptive, since I often want to type RET,
    ;; even when a completion is prompted. The solution is to unbind RET and rebind a less
    ;; intrusive option. I use control-space.

    ;; This unbinds "RET" in the map `corfu' uses during completion. The trailing t ensures
    ;; that we are removing this binding, not just setting it to nil. This allows fallback
    ;; to other keymaps (such as the `self-insert-command' in the `global-mode-map').
    (keymap-unset corfu-map "RET" t)

    ;; I then apply the correct bindings for Ctrl-Space. Unfortunately, there doesn't seem
    ;; to be a binding that applies to both the terminal and the GUI, so I apply a separate
    ;; binding for both.
    (dolist (spc '("C-@" "C-SPC"))
      ;; C-@ works in the terminal, but not in GUI.
      ;; C-SPC works in GUI, but not in the terminal.
      (keymap-set corfu-map spc #'corfu-insert))))

;; `corfu' only works on a GUI. When I don't have access to a GUI, I load
;; https://codeberg.org/akib/emacs-corfu-terminal to get the graphics to stay consistent.

(unless (display-graphic-p)
  ;; Since we don't need the additional mode on GUI, only download it when on a TTY.
  (elpaca (corfu-terminal
	   :type git
	   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    (corfu-terminal-mode +1)))

;; A vertical completion framework, applying a nicer UX to default `completing-read' style
;; completion.
(elpaca vertico
  (setq vertico-cycle t)
  (vertico-mode))

;; Helpful information in the margin of `vertico' completions.
(elpaca marginalia (marginalia-mode))

;; Orderlies provides functions for sorting completion results.
(elpaca orderless
  (setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles basic partial-completion)))))




;; Copilot

(elpaca (copilot :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  (with-eval-after-load 'copilot
    (add-to-list 'copilot-major-mode-alist '("jsonian" . "json"))))

;;; Consult

;; https://github.com/minad/consult is a utility package that provides a boat load of
;; improved consulting commands. I rebind several existing built-in commands with their
;; =consult= equivalent.

(elpaca consult
  (keymap-global-set "<remap> <goto-line>" #'consult-goto-line)
  (keymap-global-set "<remap> <Info-search>" #'consult-info)
  (keymap-global-set "<remap> <yank-pop>" #'consult-yank-pop)
  (keymap-global-set "<remap> <imenu>" #'consult-imenu)

  ;; By default, consult applies the prefix ?# to all registers, which
  ;; is not necessary.
  (setq consult-register-prefix nil)

  (keymap-global-set "<remap> <jump-to-register>" #'consult-register-load)
  (keymap-global-set "<remap> <switch-to-buffer>" #'consult-buffer)
  (keymap-global-set "<remap> <switch-to-buffer-other-frame>" #'consult-buffer-other-frame)
  (keymap-global-set "<remap> <switch-to-buffer-other-window>" #'consult-buffer-other-window)
  (keymap-set isearch-mode-map "<remap> <isearch-edit-string>" #'consult-isearch-history))



;;; Anzu

;; Anzu provides counters when searching
(elpaca anzu
  (custom-set-variables
   '( anzu-mode-lighter ""))
  (global-anzu-mode +1))



;;; Jinx - Spellcheck

(elpaca (jinx
         :host github :repo "minad/jinx"
         :files (:defaults "*.c" "*.h")) ;; Copy over
  (global-jinx-mode)
  (keymap-global-set "M-$" #'jinx-correct))

(unless module-file-suffix
  (error "TODO: Support spellcheck without dynamic modules"))

;;; `abbrev-mode`

(defun i/abbrev-add-local (arg)
  "Define a buffer-local abbrev whose expansion is last word before point.
If there's an active region, use that as the expansion.

Prefix argument ARG says how many words before point to use for the expansion;
zero means the entire region is the expansion.

A negative ARG means to undefine the specified abbrev.

This command reads the abbreviation from the minibuffer."
  (interactive "P")
  (add-abbrev local-abbrev-table "Local" arg))

(keymap-set abbrev-map "l" #'i/abbrev-add-local)



;;; Major Modes: `text-mode'

;; Text mode is the parent mode for unstructured text.

;; We want spelling support for text all text modes, so we turn on `flyspell-mode' for
;; `text-mode'. This applies for all derived modes as well.

(i/add-hook 'text-mode-hook
  #'visual-line-mode #'abbrev-mode)



;;; Major Modes: `prog-mode'

;; `prog-mode' is for writing structured text for a computer to read (programs). All
;; programming language major modes and most data format major modes are ultimately
;; derived from `prog-mode'.

;; Programming languages introduce a new type of error: syntax errors. This is handled by
;; `flymake', which we enable for all programming languages.
(i/add-hook prog-mode-hook #'flymake-mode)




;;; Treesit

;; Emacs 29 includes built-in support for https://tree-sitter.github.io/tree-sitter/,
;; under the treesit prefix. Paradoxically, they enable some tree sitter modes by default,
;; but don't bundle the appropriate grammars into Emacs. This means that an unconfigured
;; Emacs errors when opening a *.ts file.

;; =emacs -Q bad-decision.ts=  fails with:
;;
;; 	■  Warning (treesit): Cannot activate tree-sitter, because language definition\
;; 	for typescript is unavailable (not-found): (libtree-sitter-typescript.so libtr\
;; 	e-sitter-typescript.dylib) No such file or directory

;; We need to define the set of valid language grammars and where we download them from.
(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	(tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"       "master" "src"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python"     "master" "src"))
        (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir"     "main"   "src"))
        (go         . ("https://github.com/tree-sitter/tree-sitter-go"         "master" "src"))
        (heex       . ("https://github.com/phoenixframework/tree-sitter-heex"  "main"   "src"))))

;; By default, treesit installs grammars in (expand-file-name "tree-sitter"
;; user-emacs-directory). We want to redirect this to a directory in
;; `i/cache-directory'/tree-sitter.
(defvar i/treesit-language-cache (i/cache-subdirectory "tree-sitter")
  "The directory to cache compiled tree-sitter language files.")

(add-to-list 'treesit-extra-load-path i/treesit-language-cache)

(advice-add #'treesit--install-language-grammar-1 :around
	    (lambda (fn out-dir &rest args)
	      (apply fn (or out-dir i/treesit-language-cache) args)))


;;; Emacs as a Server

;; Ensure that each instance of Emacs has a unique server name.
(with-eval-after-load 'server
  (when (equal server-name "server")
    (setq server-name (format "server-%s" (emacs-pid)))))

(defun i/server-ensure ()
  "Ensure that an instance of a `server' is running."
  (require 'server)
  (unless server-process
    (server-start)))



;;; Language Server Protocol (LSP)

;; Many major modes augment their functionality with a
;; https://microsoft.github.io/language-server-protocol/. `eglot' is the built in LSP
;; consumer for Emacs. It doesn't require much setup.

(defun i/lsp--ensure ()
  "Turn on the mode appropriate LSP mode."
  ;; `eglot-ensure' needs to be called via each `major-mode's startup hook. Because of the
  ;; blocking nature of `eglot-ensure', we provide wrapper function that will allow the
  ;; buffer to display before enabling the LSP server.
  (run-with-idle-timer 0 nil #'eglot-ensure))

;; Performance optimizations for eglot
;;
;; Borrowed from https://github.com/joaotavora/eglot/discussions/993.
(with-eval-after-load 'eglot
  (setq eglot-events-buffer-config '(:size 0 :format 'lisp)
        eglot-ignored-server-capabilities '(:hoverProvider
                                            :documentHighlightProvider)
        eglot-autoshutdown t))

(i/define-keymap i/lsp-map
  "Common functions for LSP."
  "a" #'eglot-code-actions
  "r" #'eglot-rename)

(cl-defmacro i/lsp-declare (mode &key require program)
  "Declare that MODE should launch a LSP server.

REQUIRE is the feature provided by the package.  It is assumed to
be the same as mode if not specified.

PROGRAM is the name or path of the LSP server, left to the
underlying LSP plugin if not specified."
  `(progn
     ,(when program
        `(with-eval-after-load 'eglot
           (add-to-list 'eglot-server-programs '((,mode) ,program))))
     (with-eval-after-load ',(or require mode)

       ;; Ensure that we have `eglot' up and running on new files.
       (add-hook ',(intern (concat (symbol-name mode) "-hook")) #'i/lsp--ensure)
       ;; Create a common binding to commonly used LSP functions.
       (keymap-set ,(intern (concat (symbol-name mode) "-map")) "M-p" i/lsp-map))))



;;; Major mode: `compilation-mode'

;; Emacs includes a built in mode for compilation output: `compilation-mode'.

;; We want `compilation-mode' to scroll to the first addressable error, not the last.
(setq compilation-scroll-output 'first-error
      compilation-save-buffers-predicate (lambda ()
                                           ;; If `compilation-directory' is in a project,
                                           ;; we save all files in that project.
                                           (if-let (p (project-current nil compilation-directory))
                                               (string-prefix-p (project-root p)
                                                                (file-truename (buffer-file-name)))
                                             ;; If not, we just save all files.
                                             t)))

;; By default, compilation buffers don't handle ansi color correctly.
;;
;; This is ridiculous, and fixable.
(defun i/compilation-filter-hook ()
  "Apply `ansi-color' to a compilation buffer."
  (require 'ansi-color)
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook #'i/compilation-filter-hook)



;;; Major Modes: `emacs-lisp-mode'

;; `emacs-lisp-mode' is the major mode used when editing Emacs lisp. Emacs is already
;; pretty good at editing lisps (kind of it's thing). It is pretty stingy on syntax
;; highlighting though, which is especially painful for a heavily dynamic language. I use
;; https://github.com/Fanael/highlight-defined to highlight symbols that are known to be
;; defined in the current session.

(elpaca highlight-defined
  ;; By default, `highlight-defined' uses its own set of faces. I don't want to spend the
  ;; effort to maintain a custom set of theme appropriate faces. Setting
  ;; `highligh-defined-face-use-itself' restores the default faces. This effectively sets
  ;; highlight-defined-${KIND}-name-face to font-lock-${KIND}-name-face.
  (setq highlight-defined-face-use-itself t) ;; Use standard faces when highlighting.
  (i/add-hook emacs-lisp-mode-hook #'highlight-defined-mode))



;;; Major Modes: `jsonian-mode'

;; I maintain my own major mode for json: `jsonian'. It has some cool features, but the
;; major win is working well in large buffers.

(elpaca jsonian
  (with-eval-after-load 'orderless
    (setq jsonian-find-filter-fn #'orderless-filter)))



;;; Git

;; https://magit.vc is everyone's favorite git client, and I'm no exception.

(defun i/magit-patch-yank (&optional arg)
  "Save the current patch to the kill ring.

ARG is passed directly to `magit-patch-save'."
  (interactive '(current-prefix-arg))
  (require 'magit-patch)
  (let ((fname (expand-file-name (make-temp-name "my-magit")
                                 (or small-temporary-file-directory
                                     temporary-file-directory))))
    (magit-patch-save fname arg)
    (with-temp-buffer
      (insert-file-contents-literally fname)
      (delete-file fname)
      (kill-new
       (buffer-substring (point-min) (point-max))))))

(elpaca magit
  (with-eval-after-load 'magit
    (setq magit-display-buffer-function
          #'magit-display-buffer-same-window-except-diff-v1
          magit-show-long-lines-warning nil))

  (with-eval-after-load 'magit-patch
    (require 'magit-patch)
    (transient-append-suffix 'magit-patch "r"
      '("y" "Yank diff as patch" i/magit-patch-yank))))

(elpaca git-modes)

(defun i/find-next-divergent-column ()
  "Move to the next column where the current line diverges from the next line.

This is useful when finding the place where a line diverges in a diff."
  (interactive)
  ;; It is prohibitively slow to perform an end of line check or move point each
  ;; iteration. Instead, we pre-compute our bound values and then just increment numbers
  ;; in our hot loop.
  (let* ((starting-column (current-column))
         (point (point))
         (max-point (line-end-position))
         (compare-point (save-excursion
                          (forward-line)
                          (let ((lineno (line-number-at-pos)))
                            (goto-char (+ (point) starting-column))
                            (unless (eq lineno (line-number-at-pos))
                              (user-error "There is no equivalent character on the line below"))
                            (point))))
         (max-compare-point (save-excursion
                              (goto-char compare-point)
                              (line-end-position))))
    (while (and
            (< point max-point)
            (< compare-point max-compare-point)
            (equal (char-after point)
                   (char-after compare-point)))
      (setq point (1+ point)
            compare-point (1+ compare-point)))
    (goto-char point)))

(defun i/gh-token ()
  "The current GitHub token, as provided by gh."
  (when-let (executable-find "gh")
    (with-temp-buffer
      (shell-command "gh auth token" (current-buffer))
      (string-trim
       (buffer-substring-no-properties
        (point-min) (point-max))))))

(defun auth-source-backends-parser-elisp (entry)
  "Parse ENTRY as a elisp auth source."
  (when (eq (car-safe entry) 'elisp-source)
    (auth-source-backend
     :type 'elisp
     :source "config"
     :search-function #'auth-source-elisp-search
     :data (cdr-safe entry))))

(cl-defun auth-source-elisp-search (&rest spec
                                          &key backend require
                                          type max host user port
                                          &allow-other-keys)
  "Given a property list SPEC, return search matches from the `:backend'.
See `auth-source-search' for details on SPEC."
  (cl-assert (or (null type) (eq type 'elisp))
             t "Invalid elisp search: %s %s")
  (cl-assert backend t "Cannot have nil backend on query %s" spec)
  (let ((matches (cl-flet ((check (entry field value)
                             (or (not value)
                                 (equal value (plist-get entry field)))))
                   (seq-filter (lambda (entry)
                                 (and
                                  ;; If a field is specified, then we match it
                                  (check entry :host host)
                                  (check entry :user user)
                                  (check entry :port port)))
                               (slot-value backend :data)))))
    (if max
        (seq-take matches max)
      matches)))


(with-eval-after-load 'auth-source
  (add-to-list 'auth-sources
               (list 'elisp-source
                     (list :host "api.github.com"
                           :user "iwahbe^forge"
                           :secret #'i/gh-token)))

  (add-hook 'auth-source-backend-parser-functions
            #'auth-source-backends-parser-elisp))

(elpaca forge)


;; I often share code snippets from GitHub repos. It is helpful to be able to link to
;; snippets without going to github.com, and GitHub maintains a stable and easily
;; compute-able link format.

;; GitHub formats repo links like so:

;;   github.com/${ORG}/${REPO}/blob/${COMMIT-SHA}/${FILE_PATH}#L${LINE_START}[-L${LINE_END}]

;; We can quickly and easily retrieve this information. We save the generated URL into the
;; `kill-ring', and print it to the screen.

(defun i/github-code-region (start end)
  "Copy the GitHub permalink of the highlighted region into the `kill-ring'.
Operate on the region defined by START to END."
  (interactive "r")
  (let ((line-start (line-number-at-pos start t))
        (line-end (line-number-at-pos end t))
        (commit (magit-git-string "rev-parse" "--verify" "HEAD"))
        (path (magit-current-file))
        (url (car-safe (magit-config-get-from-cached-list "remote.origin.url"))))
    (unless url
      (user-error "Could not get remote URL"))
    (kill-new
     (format "%s/blob/%s/%s#L%d%s"
             (string-trim-right url (regexp-quote ".git")) commit path line-start
             (if (= line-start line-end)
                 ""
	       (format "-L%d" line-end))))
    (message "Github link to region: %s" (car kill-ring))))



;; Magit handles it's lovely UX with a subsidiary package:
;; https://github.com/magit/transient, which caches its history locally. We need to remap
;; this into `i/cache-directory' to keep .emacs.d clean. We don't need to `require'
;; transient ourselves, since Magit depends on it.

;; Transient does not define it's own history dir, so we do it ourselves.
(defvar i/transient-cache-dir (i/cache-subdirectory "transient")
  "The directory where transient history files are stored.")
(setq
 transient-history-file (expand-file-name "history.el" i/transient-cache-dir)
 transient-values-file (expand-file-name "values.el" i/transient-cache-dir)
 transient-levels-file (expand-file-name "levels.el" i/transient-cache-dir))



;;; Major Modes: `org-mode'

(defvar i/org-babel-languages
  '(calc
    emacs-lisp
    shell)
  "Languages loaded by `org-babel-do-load-languages' before a org src block is executed.")

;; https://orgmode.org is a staple of Emacs, providing a todo list, calendar, literate
;; programming environment and much more. `org-mode' comes built-in to Emacs, but I think
;; it's worth opting into a more developed version.
(elpaca org
  ;; Loading just emacs-lisp and shell takes 0.08 seconds. We don't want to do this during
  ;; startup, so we defer loading until it is used.
  (i/advise-once
   #'org-babel-execute-src-block
   :before (lambda (&rest _)
             (org-babel-do-load-languages
              'org-babel-load-languages
              (mapcar (lambda (lang)
                        (cons lang t))
                      i/org-babel-languages))))

  (require 'ol)
  (org-link-set-parameters "gh"
                           :follow #'org-gh-follow
                           :insert-description #'org-gh-insert-description)
  ;; gh:org/repo#num is much cleaner then https://github.com/org/repo/issues/num, but the
  ;; internet tends to provide links in the less clean form.
  ;;
  ;; This fixes up urls before `org-insert-link' is called to add a description. Since
  ;; "gh:" style links know how to insert their own description, this makes it possible to
  ;; insert a GH description by simply `yank'ing in a GH link and calling
  ;; `org-insert-link'.
  (advice-add #'org-insert-link :before #'org-gh--fixup-http))

(defun org-gh--fixup-http (&rest _)
  "When `org-insert-link' is run, fixup http(s?) links into the gh: format.

A link gh: compatible when it is of the form

    http(s)://www.github.com/${ORG}/${REPO}/(pulls|issues)/${NUMBER}

The above link can be equivalently represented as

    gh:${ORG}/${REPO}#${NUMBER}

This function performs the fixup in place."

  (when (org-in-regexp org-link-plain-re)
    (let ((remove (list (match-beginning 0) (match-end 0)))
          (link (org-link-unescape (match-string-no-properties 0)))
          (gh-link-regexp
           "^https?://\\(www\\.\\)?github.com/\\([^/.]+\\)/\\([^/.]+\\)/\\(pull\\|issues\\)/\\([0-9]+\\)$")
          (case-fold-search nil))
      (when (string-match gh-link-regexp link)
        (apply #'delete-region remove)
        (insert (concat "gh:"
                        (match-string 2 link) "/"
                        (match-string 3 link) "#"
                        (match-string 5 link)))))))

(defun org-gh--parse (link)
  "Parse a gh: LINK of the form gh:org/repo#issue into (org/repo . issue)."
  (let ((parts (string-split (string-remove-prefix "gh:" link) "#" t)))
    (unless (length= parts 2)
      (error "Link had the wrong length: %s" link))
    (cons (car parts) (cadr parts))))

(defun org-gh-follow (link _)
  "Open a `gh' based LINK of the format gh:org/repo#number."
  (let ((parts (org-gh--parse link)))
    (if-let (gh (executable-find "gh"))
        (call-process gh nil nil nil "issue" "view" "--web" "--repo" (car parts) (cdr parts))
      (user-error "Could not find \"gh\" executable"))))

(with-eval-after-load 'ol
  (add-to-list 'org-link-abbrev-alist (cons "gh" #'org-gh-export)))

(defun org-gh-export (link)
  "Export the (LINK . DESC) pair according to BACKEND."
  (let ((p (org-gh--parse link)))
    (format "https://www.github.com/%s/issues/%s" (car p) (cdr p))))

(defun org-gh-insert-description (link description)
  "Find the name of a GH issue for display purposes.
LINK is a gh link of the form org/repo#number.
DESCRIPTION is the existing description."
  (or description
      (when-let ((parts (org-gh--parse link))
                 (gh (executable-find "gh")))
        (with-temp-buffer
	  (unless (equal 0
                         (call-process gh nil t nil
			               "issue" "view" (cdr parts)
			               "--repo" (car parts)
			               "--json" "title"))
	    (user-error "Failed to get title from GH: %s"
                        (progn (goto-char (point-min))
                               (buffer-string))))
	  (goto-char (point-min))
          (format "%s#%s: %s"
                  (car parts) (cdr parts)
                  (alist-get 'title (json-parse-buffer :object-type 'alist)))))))

;; `org-mode' is structured around putting all your =.org= files into a single
;; directory. It isn't required, but I generally do it anyway. The default value is
;; $HOME/org, but I prefer ~/Documents/org, since it is synced by iCloud. This makes my
;; *.org files accessible on my iPhone and iPad.
(setq org-directory "~/Documents/org"
      org-id-locations-file (i/cache-file "id-locations" "org"))

;; `org-mode' is primarily used for reading, so it's worth making it look as nice as
;; possible
(with-eval-after-load 'org
  (setq
   ;; Hide markup text such as =*=, =/= and ===.
   org-hide-emphasis-markers t

   ;; Similarly, we can render pretty equations like =(\alpha - \beta) \div \Omega=.
   org-pretty-entities t

   ;; We would prefer that org renders headings as =✿ Foo= then =***✿ Foo=.
   org-hide-leading-stars t))

;; I replace stand org bullets with graphical overlays.
(elpaca org-superstar (i/add-hook org-mode-hook #'org-superstar-mode))

;; I would prefer that org is read with variable width text, but I need source blocks and
;; tables to be rendered with fixed width text. This can be accomplished by overriding org
;; text properties.
;; 
;; This solution was inspired by https://zzamboni.org/post/beautifying-org-mode-in-emacs/.
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Iosevka Aile"))))
 '(fixed-pitch ((t ( :family "Iosevka SS01"))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(org-quote ((t (:inherit (shadow italic)))))
 '(org-date ((t (:inherit (shadow fixed-pitch))))))

;; Apply the face `org-quote' to quoted blocks.
(setq org-fontify-quote-and-verse-blocks t)

;; `engrave-faces' allows exporting to LaTeX using Emacs's `font-lock' to highlight SRC
;; blocks.
;;
;; This increases the number of tlmgr packages needed to compile the generated .tex file.
(elpaca engrave-faces
  (setq org-latex-src-block-backend 'engraved))

;; I can now safely enable variable pitch mode.
(i/add-hook org-mode-hook #'variable-pitch-mode)

(with-eval-after-load 'org
  (add-to-list 'org-export-backends 'md))

;; `org-mode' defines a "TODO" item as any header that begins with a todo keyword.  The
;; keywords are defines as so:
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")
	(type "PROJ(p)")
	(type "LOOP(l)")
        (type "MEETING(m)")
        (type "IDEA(i)")))

;; I want to leave a small note every time a "TODO" changes state.
(setq org-log-done 'note)

;; Set how `org-refile' works.
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      org-refile-targets `((nil :maxlevel . 3) ; The current file
                           (org-agenda-files :maxlevel . 2)
                           (,(expand-file-name "archive.org" org-directory) :maxlevel . 1)))

;; `org-agenda' is a component of `org-mode' that displays "TODO" elements as part of a
;; time view.

;; I scatter "TO DO" elements all over my org files, so I need to tell `org-mode' which
;; directories it should search through.
(with-eval-after-load 'org-agenda
  (require 'org-roam)
  (setq
   org-agenda-files (list
                     org-directory
                     (expand-file-name "pulumi" org-directory)
                     (expand-file-name "daily" org-roam-directory))
   org-agenda-window-setup 'current-window

   ;; I generally use it to discover what I need to do this week, so I tell it to work in
   ;; increments of a week.
   org-agenda-span 'week)
  (add-hook 'org-agenda-after-show-hook #'org-narrow-to-subtree))

;; Org allows embedded source blocks, framed by "#+BEGIN_SRC" and "#+END_SRC".
;;
;; By default, they are indented, but this is confusing since it doesn't match other text
;; (which is not).
(setq org-src-preserve-indentation t)

;; When calling `org-edit-special', Emacs defaults to putting the buffer in another
;; frame. Generally, we want the same frame.
(setq org-src-window-setup 'current-window)

;; Org-roam is a https://en.wikipedia.org/wiki/Zettelkasten based notes system. It is an
;; extension to `org-mode'.
(elpaca org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory)
	org-roam-db-location (i/cache-file "roam.db" "org"))

  (defvar i/org-default-bibliography (expand-file-name "global.bib" org-roam-directory)
    "A bibliography where org associated entries are stored.")

  ;; `org-roam-node-list' is called before a list of nodes is displayed to the user. We
  ;; use it as a prompt to turn on database syncing without slowing down startup.
  (i/advise-once #'org-roam-node-list :before (lambda (&rest _) (org-roam-db-autosync-mode +1)))
  (setq org-cite-global-bibliography (list i/org-default-bibliography)))

(autoload #'i/org-capture-article (expand-file-name "templates.el" org-directory) nil t)
(with-eval-after-load 'org
  (let ((f (expand-file-name "templates.el" org-directory)))
    (when (file-exists-p f)
      (load-file f))))

(i/define-keymap i/org-global-map
  "My globally accessible org map."
  :global "M-o"
  "f" #'org-roam-node-find
  "r" #'org-roam-capture
  "c" #'org-capture
  "d" #'org-roam-dailies-goto-today
  "a" (lambda () (interactive) (org-agenda nil "n"))
  "b" (lambda ()
        (interactive)
        (find-file i/org-default-bibliography)))

(with-eval-after-load 'org-roam
  ;; From https://jethrokuan.github.io/org-roam-guide/
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
        ;; Here `${type}' is referencing the `org-roam-node-type'.
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(autoload #'org-roam-buffer-toggle "org-roam" nil t)
(i/define-keymap i/org-roam-leader-map
  "Org specific keybindings for `org-roam'.

Unlike `i/org-global-map', these keys are only accessible in an
`org-mode' buffer."
  :bind (org-mode-map "C-c r")
  :after 'org
  "a" #'org-roam-alias-add
  "i" #'org-roam-node-insert ; This only makes sense where the links are rendered
                                        ; correctly (`org-mode')
  "b" #'org-roam-buffer-toggle)

;; Emacs has `sh-mode', but no `zsh-mode'. Unfortunately, `org-mode' expects a mode called
;; `zsh-mode' when activating `org-edit-special'. Since the built-in `zsh-mode' can handle
;; *.zsh files just fine, we fake it.
(defalias 'zsh-mode 'sh-mode)

;;; Major modes: sh-mode

(elpaca flymake-shellcheck
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))



;;; Terminal Emulation

;; There are quite a few different terminal emulators for Emacs, from the built in `term'
;; to the fully Emacs Lisp based shell `eshell'. I prefer
;; https://github.com/akermu/emacs-libvterm (`vterm'), an Emacs integration of the
;; https://launchpad.net/libvterm C99 library. It acts as a fully function unconstrained
;; terminal, just like Termnial.app or https://github.com/alacritty/alacritty.
(elpaca vterm
  (setq vterm-max-scrollback 10000)
  (i/advise-once #'vterm :before
                (lambda (&rest _)
                  ;; `vterm' is capable of running Emacs recursively, but exiting is
                  ;; hard. To solve this problem, I have aliased "emacs" to "emacsclient"
                  ;; in "home/.zshrc" when [[ "$INSIDE_EMACS" = "vterm" ]]:
                  ;;
                  ;;     alias emacs='emacsclient --quiet'
                  ;;
                  ;; To ensure that the client accesses the instance of Emacs that is
                  ;; hosting `vterm', we need to ensure that the server is running and
                  ;; that we set EMACS_SOCKET_NAME to the server name.
                  ;;
                  ;; See
                  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html
                  ;; for details.
                  (i/server-ensure)
                  (push (concat "EMACS_SOCKET_NAME=" server-name) vterm-environment))))

(defun i/project-vterm (&optional arg)
  "A project aware invocation of `vterm'.
ARG is passed to `vterm' without processing."
  (interactive)
  (defvar vterm-buffer-name)
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project))
	    (vterm-buffer-name (concat "*vterm<" (project-name project) ">*")))
	(vterm arg))
    (vterm arg)))

(with-eval-after-load 'project
  (defalias 'project-shell 'i/project-vterm)

  ;; We ensure that `vterm' buffers are cleaned up when they are a part of a project by
  ;; adding them to `project-kill-buffers'.
  (add-to-list 'project-kill-buffer-conditions
	       '(and
	         (derived-mode . vterm-mode)
	         "^\\*vterm<.*>\\*$")))



;;; Major Modes: `bibtex-mode'

;; This is a major mode built into Emacs, and I'm happy to used the version shipped with
;; the running Emacs instance.

;; Setup format on save.
(i/add-hook bibtex-mode-hook
  (lambda ()
    (add-hook 'before-save-hook #'bibtex-reformat nil t)))

;; Setup formatting
(setq bibtex-align-at-equal-sign t
      bibtex-comma-after-last-field t
      bibtex-unify-case-function #'downcase)
(with-eval-after-load 'bibtex
  (mapc (lambda (x) (push x bibtex-entry-format))
        (list 'last-comma 'sort-fields 'realign)))

;;; Major Modes: `go-mode'

(elpaca go-mode)
(i/lsp-declare go-mode)

(i/add-hook go-mode-hook
  (apply-partially #'add-hook 'before-save-hook #'gofmt-before-save nil t))

(with-eval-after-load 'go-mode
  (keymap-set go-mode-map "C-c s" #'i/go-invert-string))

(setq-default eglot-workspace-configuration
              '((:gopls . ((gofumpt . t)))))

(defun i/go-invert-string (start end)
  "Invert the string at point.

If the region is active, then the region is interpreted to define
the string at point.

START and END define the bounds of the region acted upon.

If the string in the region is an interpreted string literal, convert it to
a raw string literal and vice versa."
  (interactive (list
                (if (region-active-p) (region-beginning)
                  (car (bounds-of-thing-at-point 'string)))
                (if (region-active-p) (region-end)
                  (cdr (bounds-of-thing-at-point 'string)))))
  (pcase (char-after start)
    (?`
     (unless (eq (char-before end) ?`)
       (user-error "Expected ` at region end, found %c" (char-before end)))
     (i/go--string-raw-to-interpreted start end))
    (?\" (i/go--string-interpreted-to-raw start end))
    (_ (user-error "Expected the region to start with %c or %c, found %c"
                   ?\" ?` (char-after start)))))

(defun i/go--string-raw-to-interpreted (start end)
  "Convert a raw string literal to an interpreted string literal.

The opening ` should be after START and the closing ` should be before END."
  (atomic-change-group
    (save-excursion
      (goto-char start)
      (delete-char 1)
      (insert-char ?\")
      (dotimes (_ (- end start 2))
        (pcase (char-after)
          (?\n
           (delete-char 1)
           (insert "\\n"))
          (?\t
           (delete-char 1)
           (insert "\\t"))
          (?\"
           (delete-char 1)
           (insert "\\\""))
          (?\\
           (delete-char 1)
           (insert "\\\\"))
          (_ (forward-char))))
      (delete-char 1)
      (insert-char ?\"))))

(defun i/go--string-interpreted-to-raw (start end)
  "Convert a interpreted string literal to an raw string literal.

The opening \" should be after START and the closing \" should be before END."
  (atomic-change-group
    (save-excursion
      (goto-char start)
      (delete-char 1)
      (insert-char ?`)
      (let ((unprocessed (- end start 2)))
        (while (> unprocessed 0)
          (setq unprocessed (1- unprocessed))
          (if (not (eq (char-after) ?\\))
              (forward-char)
            (delete-char 1) ;; Delete the escaping \
            (setq unprocessed (1- unprocessed)) ;; mark the extra char as processed
            (pcase (char-after)
              (?n
               (delete-char 1)
               (insert "\n"))
              (?t
               (delete-char 1)
               (insert "\t"))
              (?\"
               (delete-char 1)
               (insert "\""))
              (?\\
               (delete-char 1)
               (insert "\\"))
              (_ (forward-char))))))
      (delete-char 1)
      (insert-char ?`))))

(elpaca (testrun :host github :repo "t0yv0/testrun.el"
                 :remotes ("iwahbe" :repo "iwahbe/testrun.el")))



;;; Major Modes: `markdown-mode'

(elpaca markdown-mode
  ;; This snippet automatically activates `markdown-mode' for files ending .markdown or .md.
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  ;; Because GitHub is so dominant, I assume that any README.md is going in GitHub, and
  ;; use GitHub Flavored Markdown (GFM). I'm not sure if I intend to keep this snippet,
  ;; but it was recommended by `markdown-mode's website.
  (autoload 'gfm-mode "markdown-mode"
    "Major mode for GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (setq-default markdown-hide-urls t
                markdown-ordered-list-enumeration nil))



;;; Major Modes: `yaml-mode'

;; A major mode for YAML. That's it..

(elpaca yaml-mode)



;;; Major Modes: `typescript-ts-mode'

;; Emacs 29 has built in support for typescript syntax highlighting with
;; `typescript-ts-mode'.

;; As of Emacs 30.0.50, typescript-ts-mode.el contains these lines:
(require 'treesit)
(if (treesit-ready-p 'typescript)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))
;; Unfortunately, it does not work in typescript-ts-mode.el because the check is not
;; auto-loaded, so its not loaded until after a function from [[help:featurep][feature]]
;; `typescript-ts-mode' is loaded. By including the above directly in our init file, we
;; get the desired behavior.

(i/lsp-declare typescript-ts-mode)



;;; Major Modes: `python-mode'

(if (treesit-ready-p 'python)
    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

(i/lsp-declare python-mode)



;;; Major Modes: `rust-mode'

;; Rust is pretty simple, we want `rust-mode' and then a LSP on top:
(elpaca rust-mode
  (setq rust-format-on-save t))

(i/lsp-declare rust-mode)

;;; Major Modes: `elixir-mode'

;; The major mode for elixir should be `elixir-mode', which isn't defined by Emacs... so
;; we define our own.

(define-derived-mode elixir-mode elixir-ts-mode "Elixir"
  "Major mode for editing Elixir files."
  :syntax-table nil ;; Use the same syntax table as `elixir-ts-mode'
  :abbrev-table nil ;; Use the same abbrev table as `elixir-ts-mode'
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

;; We want to triger `elixir-mode' for the same set of files we would trigger
;; `elixir-ts-mode' for.
(dolist (m '(("mix\\.lock" . elixir-mode) ("\\.exs\\'" . elixir-mode)
             ("\\.ex\\'" . elixir-mode) ("\\.elixir\\'" . elixir-mode)))
  (add-to-list 'auto-mode-alist m))

;; When `elixir-ts-mode' loads, it pollutes `auto-mode-alist', so we clean it out.
(with-eval-after-load 'elixir-ts-mode
  (setq auto-mode-alist
        (seq-filter
         (lambda (el)
           (not (eq (cdr el) 'elixir-ts-mode)))
         auto-mode-alist)))

(i/lsp-declare elixir-mode :program "elixir-ls")

(provide 'elixir-mode)



;;; Major Modes: `terraform-mode'


;; OPEN_SOURCE:
;;
;; * Currently, `terraform-format-on-save' is a destructive operation. We could replace
;;   the current op with `replace-buffer-contents' to make it less destructive. This
;;   should make the point jump around less.
(elpaca terraform-mode
  (setq terraform-format-on-save t))



;;; ChatGPT with OpenAI

;; I have written a package for interacting with ChatGPT called
;; https://github.com/iwahbe/chat.el. This provides basic functionality to interact with
;; OpenAI's API: https://platform.openai.com/docs/api-reference/chat.
(elpaca (chat.el :host github :repo "iwahbe/chat.el")
  (setq chat-model "gpt-4o")
  (i/1Password-setq chat-api-key "OpenAI/API Keys/Pulumi" #'chat-get-api-key)
  (keymap-global-set "C-c c" #'chat-query-dwim))



;;; Lisp

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'recase)

;;; Pulumi

;; I currently work for https://www.pulumi.com, and I define a set of special functions to
;; work specifically with Pulumi specific data formats.

;; The largest in terms of scope (but not code) is a special mode for Pulumi YAML:
(elpaca (pulumi-yaml :host github :repo "pulumi/pulumi-lsp"
                     :files ("editors/emacs/*"))
  (i/lsp-declare pulumi-yaml-mode :require pulumi-yaml)
  (with-eval-after-load 'pulumi-yaml
    (keymap-set pulumi-yaml-mode-map "C-M-i" #'completion-at-point))
  (add-to-list 'auto-mode-alist (cons (regexp-quote "Pulumi.yaml") 'pulumi-yaml-mode))
  (add-to-list 'auto-mode-alist (cons (regexp-quote "Pulumi.yml") 'pulumi-yaml-mode))
  (add-to-list 'auto-mode-alist (cons (regexp-quote "Main.yaml") 'pulumi-yaml-mode)))

;; Autoload the necessary functions to have this work out of the box.
(autoload 'pulumi-schema-mode "pulumi-schema.el" nil t)
(autoload 'pulumi-schema--is "pulumi-schema.el" nil t)
(add-to-list 'magic-mode-alist '(pulumi-schema--is . pulumi-schema-mode))

;; Pulumi has repos, so many repos. Often, working on a bug in one repository requires
;; linking in several others. These functions make adding go module
;; https://go.dev/ref/mod#go-mod-file-replace directives to other Pulumi repositories
;; easy.

(defun i/pulumi-go-src-root ()
  "The root of the pulumi go src."
  (expand-file-name
   "src/github.com/pulumi"
   (or
    (getenv "GOPATH")
    (expand-file-name
     "go"
     (expand-file-name
      user-login-name
      "/Users" )))))

(defun i/pulumi-go-projects ()
  "A list of go project paths under the pulumi org."
  (seq-map #'car
           (seq-filter (lambda (attr)
                         (and
                          (cadr attr) ;; A directory
                          (not (member (car attr) '("." ".." "templates")))))
                       (directory-files-and-attributes (i/pulumi-go-src-root)))))

(defun i/pulumi-go-modules (dir depth)
  "A list of go paths contained in DIR.
DEPTH specifies how many levels to search through."
  (when (and dir (>= depth 1) (file-directory-p dir))
    (let ((root (expand-file-name "go.mod" dir)))
      (if (file-exists-p root)
          (with-temp-buffer
            (insert-file-contents-literally root)
            (search-forward-regexp "^module \\(.+\\)$")
            (list
             dir
             (buffer-substring
              (match-beginning 1)
              (match-end 1))))
        (flatten-list
         (seq-filter #'identity
                     (seq-map
                      (lambda (x) (i/pulumi-go-modules (expand-file-name x dir) (1- depth)))
                      (seq-filter (lambda (x) (not (member x '("." ".."))))
                                  (directory-files dir)))))))))

(defun i/pulumi-module-path-map ()
  (let ((m (make-hash-table :test #'equal))
        (root (i/pulumi-go-src-root)))
    (mapc
     (lambda (dir)
       (let* ((p (expand-file-name dir root))
              (path-and-mods (i/pulumi-go-modules p 2)))
         (while path-and-mods
           (puthash (cadr path-and-mods) (car path-and-mods) m)
           (setq path-and-mods (cddr path-and-mods)))))
     (i/pulumi-go-projects))
    m))

(defun i/pulumi-replace (&optional arg)
  "Insert the appropriate `replace` directive for a pulumi project."
  (interactive
   (list (completing-read "Select replace target: "
                          (i/pulumi-module-path-map)
                          nil t)))
  (insert "replace " arg " => "
          (file-relative-name
           (gethash arg (i/pulumi-module-path-map)))
          "\n"))



;;; Path Sync

;; Ensure that `exec-path' matches the path as set up by the shell environment.
;;
;; This implementation relies on `exec-path' being good enough to discover a valid ZSH
;; implementation, which is then queried asynchronously to find the correct path.

(defun i/set-exec-path-from-shell ()
  "Attempt to set the variable `exec-path' from $SHELL's $PATH."
  (let* ((b (get-buffer-create "discover-exec-path" t))
         (p (start-process "discover-exec-path" b shell-file-name shell-command-switch "echo $PATH")))
    (set-process-sentinel p (lambda (_ event)
                              (when (equal event "finished\n")
                                ;; If we have discovered a path, set it.
                                (if-let ((found (string-split
                                                 (with-current-buffer b (buffer-string))
                                                 ":" t "\n")))
                                    (progn
                                      (setq exec-path (append found (list exec-directory)))
                                      ;; Now that we have discovered `$PATH' from the
                                      ;; shell, propagate it back to Emacs's PATH for
                                      ;; subsidiary processes (like LSP servers) to use.
                                      (setenv "PATH" (string-join exec-path ":")))
                                  (message "Failed to discover exec-path"))
                                ;; Regardless of if we have discovered a path, kill the
                                ;; buffer. We won't get another chance here.
                                (kill-buffer b))))
    (set-process-query-on-exit-flag p nil)))

(i/set-exec-path-from-shell)

;;; Alter
;;
;; A small and simple library for building mapping commands.

(defun i/alter-word-at-point (&optional f register)
  "Set REGISTER to a function F that operates on the current word."
  (interactive "a mapping function:
c the register to save to:")
  (set-register register (i/alter-word-register--make f)))

(cl-defstruct
    (i/alter-word-register (:constructor nil)
                          (:constructor i/alter-word-register--make (f)))
  (f nil :read-only t :documentation "The function used to operate on the word at point: (string) -> string."))

(cl-defmethod register-val-insert ((r i/alter-word-register))
  "Call f (accessed from R)."
  (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'word)))
    (replace-region-contents beg end
                             (lambda () (funcall (i/alter-word-register-f r) (buffer-string))))))

(cl-defmethod register-val-describe ((r i/alter-word-register) _verbose)
  "Describe the function to call (from R)."
  (princ "Alter word at point: ")
  (princ (i/alter-word-register-f r)))

(keymap-global-set "M-_" #'kmacro-call-macro)

(i/define-keymap i/alter-map
  :global "C-x r a"
  "w" #'i/alter-word-at-point)



(provide 'init)
;;; init.el ends here
