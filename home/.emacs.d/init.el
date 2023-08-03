;;; init.el --- Tangled from init.org -*- lexical-binding: t; -*-

;;; Commentary:

;; This file was tangled as part of iwahbe's Emacs config.

;;; Code:

;; we want lexical bindings even when developing interactively. This doesn't do anything
;; when a file is loaded, but it does effect what happens when =eval-last-sexp= is used.
(setq-default lexical-binding t)


;; I declare a custom helper macro for adding hooks. It simplifies quoting, and allows
;; multiple hooks to be attached in a single sexp.
(defmacro =add-hook (mode &rest hooks)
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

(defmacro =dbg (form)
  "Print FORM => RES where res is what FORM evaluate to.
Return RES."
  ;; This is the kind of function that is helpful to have around, but shouldn't make it
  ;; into "production" code.
  `(let ((res ,form)) (message "dbg: %s => %s" '(,@form) res) res))


;;; Cache

(defvar =cache-directory (expand-file-name ".cache" user-emacs-directory)
  "The directory where a system local cache is stored.")

(defun =cache-subdirectory (domain)
  "A stable directory to cache files from DOMAIN in."
  (expand-file-name (concat domain "/") =cache-directory))

(defun =cache-file (file &optional domain)
  "A stable file name for FILE, located in DOMAIN if provided."
  (expand-file-name file
		    (if domain
			(let ((s (=cache-subdirectory domain)))
			  (unless (file-executable-p s)
			    (mkdir s))
			  s)
		      =cache-directory)))

;; Following good practice, we maintain an =assets= folder, where we store /heavy/ files.


(defvar =assets-directory (expand-file-name "assets" user-emacs-directory)
  "The directory containing large runtime assets, such as images.")



;;; Package Management

;; I use https://github.com/progfolio/elpaca as my package manager for Emacs.

(defvar elpaca-directory (=cache-subdirectory "elpaca"))

;; This is the install script straight from the elpaca repo:

(defvar elpaca-installer-version 0.1)
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
			      :ref nil
			      :files (:defaults (:exclude "extensions"))
			      :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
	   (build (expand-file-name "elpaca/" elpaca-builds-directory))
	   (order (cdr elpaca-order))
	   ((add-to-list 'load-path (if (file-exists-p build) build repo)))
	   ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
	       ((zerop (call-process "git" nil buffer t "clone"
				     (plist-get order :repo) repo)))
	       (default-directory repo)
	       ((zerop (call-process "git" nil buffer t "checkout"
				     (or (plist-get order :ref) "--"))))
	       (emacs (concat invocation-directory invocation-name))
	       ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
				     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
	  (progn (require 'elpaca)
		 (elpaca-generate-autoloads "elpaca" repo)
		 (kill-buffer buffer))
	(error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; We redefine =display-startup-echo-area-message=, since there is no built in way to
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

(defun =splash-buffer (&optional window)
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
      (read-only-mode)
      (make-local-variable 'window-size-change-functions)
      (add-to-list 'window-size-change-functions #'=splash-buffer)
      (let ((inhibit-read-only t))
	(unless (eq (buffer-size) 0)
	  (erase-buffer))
	(if (and (display-graphic-p) (featurep 'image))
	    (=splash-buffer--graphic)
	  (=splash-buffer--text))
	(setq cursor-type nil)
	(goto-char (point-min))
	(setq mode-line-format nil)
	(current-buffer)))))

;; Emacs uses `initial-buffer-choice' to determine what buffer it should start in.
(setq initial-buffer-choice #'=splash-buffer)

;; Here we want to insert a random image from our list of graphic banner images. Graphic
;; banner images are stored in the "assets" folder. We define our list of images.

(defvar =emacs-graphic-banners
  (mapcar (lambda (x) (expand-file-name x =assets-directory))
	  '("gnu-head.svg"
	    "emacs-icon.svg"))
  "A list of graphical banners to open Emacs with.
Each element is expected to be the path to a SVG file.")

(defvar =emacs-graphic-banner
  (nth (random (length =emacs-graphic-banners))
       =emacs-graphic-banners)
  "The randomly chosen graphic banner to use for this session.
This is calculated once, so it doesn't change during redisplay.")

;; We then define what a graphic splash buffer will be: A centered image 1/3 down the
;; frame.

(defun =splash-buffer--graphic ()
  "Display the splash screen with graphics."
  (let* ((img
          ;; Each image is expected to take up 3/4 of the smallest dimension of screen
          ;; space.
          (if (>= (frame-pixel-width) (frame-pixel-height))
              (create-image
	       =emacs-graphic-banner
	       nil nil :height (round (* (frame-pixel-height) 0.75)))
	    (create-image
	     =emacs-graphic-banner
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


(defvar =emacs-text-banners
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

(defvar =emacs-text-banner
  (nth (random (length =emacs-text-banners)) =emacs-text-banners)
  "The text banner to use for this session.
This is calculated once so it doesn't change during redisplay")

;; The display function is similar to the graphic version, aiming to put the text centered
;; 1/3 down the frame.

(defun =splash-buffer--text ()
  "Display the splash screen with only text."
  (let ((banner =emacs-text-banner)
	(empty-line "\n"))
    (dotimes (_ (- (/ (frame-height) 3) (/ (length banner) 2) 2))
      (insert empty-line))
    (mapc (lambda (x) (insert x "\n")) banner))
  (let ((fill-column (frame-width)))
    (center-region (point-min) (point-max))))



;;; Miscellaneous Customizations

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

;; Emacs includes a built in mode for compilation output: `compilation-mode'.  We want
;; `compilation-mode' to scroll to the first addressable error, not the last.
(setq compilation-scroll-output 'first-error)

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
(setq save-place-file (=cache-file "places"))
(save-place-mode +1)

(setq savehist-file (=cache-file "savehist"))
(savehist-mode +1)



;;; Themes

;; Emacs uses a concept called a "theme" to control system appearance. Each theme applies
;; a layer of "face" description to the loaded buffer. The ordered list of enabled themes
;; is defined in the variable `custom-enabled-themes'.

;; When I load a theme, I generally only want that theme to apply. I don't want the
;; previous theme to effect the current experience. To solve this, I define a `load-theme'
;; wrapper called `=load-theme'.
(defun =load-theme (theme)
  "Load THEME.

Other currently loaded themes are disabled."
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar #'symbol-name
				          (custom-available-themes))
                                  nil t))))
  (load-theme (pcase theme
		('light 'spacemacs-light)
		('dark 'spacemacs-dark)
		(other other))
	      t)
  ;; Disable previous themes
  (mapc #'disable-theme (cdr custom-enabled-themes)))

;; I'm currently using [[https://github.com/nashamri/spacemacs-theme][spacemacs-theme]],
;; both light and dark as my goto-theme.
(elpaca spacemacs-theme
  ;; Mac has a concept of light and dark mode at the system level. Emacs can be built with
  ;; hooks to support system appearance change. I want use these hooks when available.
  (if (boundp 'ns-system-appearance)
      ;; This hook will be called later during the startup process, so we don't need to
      ;; manually call `=load-theme'.
      (=add-hook ns-system-appearance-change-functions #'=load-theme)
    ;; When there isn't any system input for the theme, we will just load the ='light=
    ;; theme by default.
    (=load-theme 'light)))



;;; 1Password

;; I use 1Password for secrets management. I would like to be able to inject 1Password
;; secrets (like API keys) into my Emacs config from 1Password during startup.

;; Each call to the 1Password's =op= CLI tool is relatively slow (and requires a
;; authentication step). To avoid making multiple calls, we will model this as a queue and
;; flush system. When a variable is set, we add it to `1Password--forms', which acts as
;; our queue. Any time we need prompt access to a variable, we flush the entire queue
;; using "op inject" command, and then `eval-buffer' the result.


(defvar =1Password--forms nil
  "A list of forms to evaluate after templating through 1Password.

After forms are evaluated, they are removed from this var.")

(defmacro =1Password-setq (name path &rest consuming-funcs)
  "Declare that NAME should be filled from 1Password.
PATH is the 1Password path to the item.
CONSUMING-FUNCS is a list of functions that are know to consume NAME.

Each consuming func is adviced to ensure that values set with
`=1Password-setq' are actually set before their consuming
functions are run."
  `(progn
     (setq =1Password--forms
           (cons (list 'setq ',name ,(concat "{{ op://Personal/" path " }}"))
                 =1Password--forms))
     ,@(mapcar (lambda (f)
                 `(advice-add ,f :before #'=1Password--ensure))
               consuming-funcs)))

(defun =1Password--flush ()
  "Evaluate `=1Password--forms' after passing it through the 1Password CLI."
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
          (insert (mapconcat #'pp-to-string =1Password--forms "\n")))
        (with-current-buffer (process-buffer
                              (make-process :name "1Password Init"
                                            :buffer "1Password"
                                            :command `(,op "inject" "--in-file" ,tmp)
                                            :connecton-type 'pipe
                                            :sentinel #'=1Password--sentinel))
          (set (make-local-variable '=1Password-template-file) tmp)))
    (warn "Could not find 1Password CLI")))

(defun =1Password--sentinel (proc event)
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
        (delete-file (buffer-local-value '=1Password-template-file b))
        ;; We mark ourselves as done, freeing functions like `=1Password--ensure' to exit.
        (setq =1Password--forms nil)
        ;; We then evaluate the buffer. This doesn't present a race condition, since Emacs
        ;; doesn't interrupt running code, even with Sentinels.
        (eval-buffer b)
        (kill-buffer b)))))

(defun =1Password-init-async ()
  "Asyncronously evaluate all declared 1Password variables."
  (=1Password--flush))

(defun =1Password--ensure (&rest args)
  "Syncronously ensure that 1Password has loaded all declared values.
ARGS allows this function to be used in hooks.  ARGS is ignored."
  (ignore args)
  (when =1Password--forms
    (message "Ensuring 1Password variables are loaded")
    (=1Password--flush)
    (while =1Password--forms
      (sit-for 0.05))))



;;; Help Messages

;; Emacs is famously introspectable. This is facilitated by the `describe-*'
;; functions. The built in introspection is excellent, but it can be improved by showing
;; more information about the values variables hold. The main improvement available is
;; showing the source code where the inspected item is defined. This is what
;; [[https://github.com/Wilfred/helpful][Wilfred/helpful]] does.

(elpaca helpful
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-command] #'helpful-command))



;; Emacs includes the excellent [[help:eldoc-mode][eldoc]], which displays information
;; about the object at point in the echo area.  For larger (or more stable) documentation,
;; Eldoc has `eldoc-doc-buffer', which can hold unabridged documentation.

;; By default, both the buffer and echo area are used when the buffer is displayed. This
;; is probably a safe default, but it is not ideal. Ideally, the existence of the doc
;; buffer obviates the need for displaying in the echo area.

;; Don't display in the echo area if the doc buffer is visable
(setq eldoc-echo-area-prefer-doc-buffer t)



;;; GoTo Quickly

;; One thing I miss from VIM is the ability to easily jump between and around
;; characters. I have written a small package to accomplish this, called /GoTo Quickly/,
;; and I load that now.
(load (expand-file-name "gtq.el" user-emacs-directory))

;; It defines `gtq-goto', which brings up a model interface for quickly navigating among
;; characters.
(global-set-key (kbd "C-'") #'gtq-goto)



;;; Whitespace

;; Trailing whitespace is generally wrong. However, I need to be careful that I don't have
;; lots of whitespace diffs on shared files. `ws-butler-mode' handles this nicely.

;; Since the package is unmaintained, I use hlissner's (of Doom Emacs fame) fork, on the
;; grounds that since it is used by a popular distribution, it will probably work.
(elpaca (ws-butler :host github :repo "hlissner/ws-butler")
  ;; It is enabled everywhere.
  (ws-butler-global-mode))



;; By default, Emacs scatters backup and auto-save files over the directory in use, but
;; does not remember useful information such as where I was last I edited the buffer. This
;; needs to be fixed.




;; I move all auto-saves into a centralized directory that I know is /not/ under source control.


(setq auto-save-list-file-prefix
      (concat (=cache-subdirectory "auto-save-list") ".saves-"))



;; Similarly, I move all backups to a cache directory.

;; The ="."= means that this is the backup location for files in all directories.


(setq backup-directory-alist `(("." . ,(=cache-subdirectory "backup"))))



;;; Project Management

;; As far as I know, Emacs has two project management solutions:
;; https://github.com/bbatsov/projectile and project.el. Because project.el is in-trunk, I
;; have decided to use it. It works out of the box, but I still needed a couple of tweaks.

;; project.el caches which projects have been accessed, which needed to be re-mapped into
;; the cache directory.
(setq project-list-file (=cache-file "projects"))

(defun =project-set-switch-commands (bound unbound)
  "Set `project-switch-commands' to the combined BOUND and UNBOUND.

BOUND items are run with `default-directory' set to the default
directory of the current project.

UNBOUND functions remain unchanged."
  ;; This enables commands like =magit= and =vterm= to kick off in the new project.
  (setq project-switch-commands
        (append
	 (mapcar
	  (lambda (x) (cons
		       (lambda ()
			 (interactive)
			 (let ((default-directory
			        (or project-current-directory-override
				    default-directory)))
			   (funcall-interactively (car x))))
		       (cdr x)))
	  bound)
         unbound)))

(defun =project-switch-to-most-recent ()
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


;; We now set the actual command pallet.
(=project-set-switch-commands
 '((project-find-file "Find file" "f")
   (consult-find "`find` file" "C-f")
   (consult-ripgrep "Find regexp" "g")
   (magit "Git" "v")
   (=project-vterm "Shell" "t"))
 '((=project-switch-to-most-recent "Most recent" "r")))

;; To avoid duplicating commands in the project map, we provide a mapping for quick access
;; to the project switcher against the current project.
;;
;; TODO: We should really merge `=project-set-switch-commands' to effect both
;; `project-switch-commands' and the C-x p "" command map.
(defun =project-command-pallet ()
  "Run the project switcher in the current project."
  (interactive)
  (project-switch-project (when-let ((p (project-current)))
                            (project-root p))))

(global-set-key (kbd "C-x P") #'=project-command-pallet)



;;; Completion

;; For completion at point, there are two main options: https://github.com/minad/corfu and
;; https://company-mode.github.io. Company is by far the 800 pound gorilla in this fight
;; with a large number of custom backends.  I have decided to use `corfu' instead, since
;; it integrates with Emacs's existing `completion-at-point-functions'.

(elpaca corfu
  (setq corfu-auto t          ;; Complete when available
        corfu-auto-delay 0    ;; Without any delay
        corfu-auto-prefix 1)  ;; Wait only for the first character

  ;; I want completion to be enabled everywhere.
  (global-corfu-mode)

  ;; Finally, I want completion to not interfere with my normal typing. By default, return
  ;; finalizes a completion. I find this super disruptive, since I often want to type RET,
  ;; even when a completion is prompted. The solution is to unbind RET and rebind a less
  ;; intrusive option. I use control-space.

  ;; This unbinds "RET" in the map `corfu' uses during completion. The trailing t ensures
  ;; that we are removing this binding, not just setting it to nil. This allows fallback
  ;; to other keymaps (such as the `self-insert-command' in the `global-mode-map').
  (define-key corfu-map (kbd "RET") nil t)

  ;; I then apply the correct bindings for Ctrl-Space. Unfortunately, there doesn't seem
  ;; to be a binding that applies to both the terminal and the GUI, so I apply a separate
  ;; binding for both.
  (dolist (spc '("C-@" "C-SPC"))
    ;; C-@ works in the terminal, but not in GUI.
    ;; C-SPC works in GUI, but not in the terminal.
    (define-key corfu-map (kbd spc) #'corfu-insert)))

;; `corfu' only works on a GUI. When I don't have access to a GUI, I load
;; https://codeberg.org/akib/emacs-corfu-terminal to get the graphics to stay consistent.

(unless (display-graphic-p)
  ;; Since we don't need the additional mode on GUI, only download it when on a TTY.
  (elpaca (corfu-terminal
	   :type git
	   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    (corfu-terminal-mode +1)))

;; A vertical completion framework, applying a nicer UX to default compleating-read style
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




;;; Consult

;; https://github.com/minad/consult is a utility package that provides a boat load of
;; improved consulting commands. I rebind several existing built-in commands with their
;; =consult= equivalent.


(elpaca consult
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap Info-search] #'consult-info)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  (global-set-key [remap imenu] #'consult-imenu)

  ;; By default, consult applies the prefix ?# to all registers, which
  ;; is not necessary.
  (setq consult-register-prefix nil)

  (global-set-key [remap jump-to-register] #'consult-register-load)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (define-key isearch-mode-map [remap isearch-edit-string] #'consult-isearch-history))



;;; Major Modes: `text-mode'

;; Text mode is the parent mode for unstructured text.

;; We want spelling support for text all text modes, so we turn on `flyspell-mode' for
;; `text-mode'. This applies for all derived modes as well.

(=add-hook 'text-mode-hook
  #'flyspell-mode
  #'visual-line-mode)



;;; Major Modes: `prog-mode'

;; `prog-mode' is for writing structured text for a computer to read (programs). All
;; programming language major modes and most data format major modes are ultimately
;; derived from `prog-mode'.

;; For programming, we want spellcheck for strings and comments, but not necessarily for
;; all text (such as variable names). Flyspell provides `flyspell-prog-mode' for this
;; purpose.
(=add-hook prog-mode-hook #'flyspell-prog-mode)

;; Programming languages introduce a new type of error: syntax errors. This is handled by
;; `flymake', which we enable for all programming languages.
(=add-hook prog-mode-hook #'flymake-mode)



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
        (json       . ("https://github.com/tree-sitter/tree-sitter-json"       "master" "src"))))

;; By default, treesit installs grammars in (expand-file-name "tree-sitter"
;; user-emacs-directory). We want to redirect this to a directory in
;; `=cache-directory'/tree-sitter.
(defvar =treesit-language-cache (=cache-subdirectory "tree-sitter")
  "The directory to cache compiled tree-sitter language files.")

(add-to-list 'treesit-extra-load-path =treesit-language-cache)

(advice-add #'treesit--install-language-grammar-1 :around
	    (lambda (fn out-dir &rest args)
	      (apply fn (or out-dir =treesit-language-cache) args)))


;;; Language Server Protocol (LSP)

;; Many major modes augment their functionality with a
;; https://microsoft.github.io/language-server-protocol/. `eglot' is the built in LSP
;; consumer for Emacs. It doesn't require much setup.

(defun =lsp-ensure ()
  "Turn on the mode appropriate LSP mode."
  ;; `eglot-ensure' needs to be called via each `major-mode's startup hook. Because of the
  ;; blocking nature of `eglot-ensure', we provide wrapper function that will allow the
  ;; buffer to display before enabling the LSP server.
  (run-with-idle-timer 0 nil #'eglot-ensure))



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
  (=add-hook emacs-lisp-mode-hook #'highlight-defined-mode))



;;; Major Modes: `jsonian-mode'

;; I maintain my own major mode for json: `jsonian'. It has some cool features, but the
;; major win is working well in large buffers.

(elpaca jsonian)



;;; Git

;; https://magit.vc is everyone's favorite git client, and I'm no exception.

(defun =magit-patch-yank (&optional arg)
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
          #'magit-display-buffer-same-window-except-diff-v1))

  (with-eval-after-load 'magit-patch
    (require 'magit-patch)
    (transient-append-suffix 'magit-patch "r"
      '("y" "Yank diff as patch" =magit-patch-yank))))


;; I often share code snippets from GitHub repos. It is helpful to be able to link to
;; snippets without going to github.com, and GitHub maintains a stable and easily
;; compute-able link format.

;; GitHub formats repo links like so:

;;   github.com/${ORG}/${REPO}/blob/${COMMIT-SHA}/${FILE_PATH}#L${LINE_START}[-L${LINE_END}]

;; We can quickly and easily retrieve this information. We save the generated URL into the
;; `kill-ring', and print it to the screen.

(defun =github-code-region (start end)
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
;; this into `=cache-directory' to keep .emacs.d clean. We don't need to `require'
;; transient ourselves, since Magit depends on it.

;; Transient does not define it's own history dir, so we do it ourselves.
(defvar =transient-cache-dir (=cache-subdirectory "transient")
  "The directory where transient history files are stored.")
(setq
 transient-history-file (expand-file-name "history.el" =transient-cache-dir)
 transient-values-file (expand-file-name "values.el" =transient-cache-dir)
 transient-levels-file (expand-file-name "levels.el" =transient-cache-dir))



;;; Major Modes: `org-mode'

;; https://orgmode.org is a staple of Emacs, providing a todo list, calendar, literate
;; programming environment and much more. `org-mode' comes built-in to Emacs, but I think
;; it's worth opting into a more developed version.
(elpaca org)

;; `org-mode' is structured around putting all your =.org= files into a single
;; directory. It isn't required, but I generally do it anyway. The default value is
;; $HOME/org, but I prefer ~/Documents/org, since it is synced by iCloud. This makes my
;; *.org files accessible on my iPhone and iPad.
(setq org-directory "~/Documents/org"
      org-id-locations-file (=cache-file "id-locations" "org"))

;; `org-mode' is primarily used for reading, so it's worth making it look as nice as
;; possible

;; I hide markup text such as =*=, =/= and ===.
(setq org-hide-emphasis-markers t)

;; Similarly, we can render pretty equations like =(\alpha - \beta) \div \Omega=.
(setq org-pretty-entities t)

;; We would prefer that org renders headings as =✿ Foo= then =***✿ Foo=.
(setq org-hide-leading-stars t)

;; I replace stand org bullets with graphical overlays.
(elpaca org-bullets (=add-hook org-mode-hook #'org-bullets-mode))

;; I would prefer that org is read with variable width text, but I need source blocks and
;; tables to be rendered with fixed width text. This can be accomplished by overriding org
;; text properties.
;; 
;; This solution was inspired by https://zzamboni.org/post/beautifying-org-mode-in-emacs/.
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Helvetica"))))
 '(fixed-pitch ((t ( :family "Fira Code Retina"))))
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
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; I can now safely enable variable pitch mode.
(=add-hook org-mode-hook #'variable-pitch-mode)


;; `org-mode' defines a "TODO" item as any header that begins with a todo keyword.  The
;; keywords are defines as so:
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "DONE(d)")
	(type "PROJ(p)")
	(type "KILL(k)")
	(type "LOOP(l)")))

;; I want to leave a small note every time a "TODO" changes state.
(setq org-log-done 'note)

;; TODO: Adjust how [[help:org-refile][org-refile]] works to allow refiling into the
;; hierarchy local buffer hierarchy.
(setq org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil)

;; `org-agenda' is a component of `org-mode' that displays "TODO" elements as part of a
;; time view.

;; I scatter "TODO" elements all over my org files, so I need to tell `org-mode' which
;; directories it should search through.
(setq org-agenda-files (list org-directory))

;; I generally use it to discover what I need to do this week, so I tell it to work in
;; increments of a week.
(setq org-agenda-span 'week)

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
	org-roam-db-location (=cache-file "roam.db" "org"))
  ;; `org-roam-node-list' is called before a list of nodes is displayed to the user. We
  ;; use it as a prompt to turn on database syncing without slowing down startup.
  (advice-add #'org-roam-node-list :before (lambda (&rest _) (org-roam-db-autosync-mode +1))))

;; This is a utility function to resolve GH links to their issue name.

;; TODO Combine `=org-describe-link' with `org-link-make-description-function' to get the
;; desired behavior by default.
(defun =org-describe-link ()
  "Heuristically add a description to the `org-mode' link at point."
  (interactive)
  (when-let* ((ctx (org-element-context))
              (type (org-element-type ctx))
	      (link (org-element-property :raw-link ctx))
	      (description (pcase link
			     ;; This is an https: link to a github issue, so we can use
			     ;; `gh` to get the issue title and display that as the
			     ;; description.
			     ((pred (string-match
				     "https://github.com/\\([-a-zA-Z0-9]+\\)/\\([-a-zA-Z0-9]+\\)/\\(pull\\|issues\\)/\\([0-9]+\\)"))
			      (with-temp-buffer
				(unless (equal 0
					       (call-process
						(executable-find "gh") nil t nil
						"issue" "view" (substring link (match-beginning 4) (match-end 4))
						(concat "--repo="
							(substring link (match-beginning 1) (match-end 1))
							"/"
							(substring link (match-beginning 2) (match-end 2)))
						"--json=title"))
				  (user-error "Failed to get title from GH"))
				(goto-char (point-min))
				(alist-get 'title (json-parse-buffer :object-type 'alist))))
			     ;; Unable to describe link, so let the user do it
			     (_
			      (message "No option matched to describe the link at point: %s" link)
			      nil))))
    (save-excursion
      (delete-region (org-element-property :begin ctx)
		     (org-element-property :end ctx))
      (org-insert-link link link description))))

;; Emacs has `sh-mode', but no `zsh-mode'. Unfortunately, `org-mode' expects a mode called
;; `zsh-mode' when activating `org-edit-special'. Since the built-in `zsh-mode' can handle
;; *.zsh files just fine, we fake it.
(defalias 'zsh-mode 'sh-mode)



;;; Terminal Emulation

;; There are quite a few different terminal emulators for Emacs, from the built in `term'
;; to the fully Emacs Lisp based shell `eshell'. I prefer
;; https://github.com/akermu/emacs-libvterm (`vterm'), an Emacs integration of the
;; https://launchpad.net/libvterm C99 library. It acts as a fully function unconstrained
;; terminal, just like Termnial.app or https://github.com/alacritty/alacritty.
(elpaca vterm
  (setq vterm-max-scrollback 10000))

(defun =project-vterm (&optional arg)
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
  (defalias 'project-shell '=project-vterm)

  ;; We ensure that `vterm' buffers are cleaned up when they are a part of a project by
  ;; adding them to `project-kill-buffers'.
  (add-to-list 'project-kill-buffer-conditions
	       '(and
	         (derived-mode . vterm-mode)
	         "^\\*vterm<.*>\\*$")))



;;; Major Modes: `go-mode'

(elpaca go-mode)

(=add-hook go-mode-hook
  #'=lsp-ensure
  (apply-partially #'add-hook 'before-save-hook #'gofmt-before-save nil t))

(defun =go-invert-string (start end)
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
     (=go--string-raw-to-interpreted start end))
    (?\" (=go--string-interpreted-to-raw start end))
    (_ (user-error "Expected the region to start with %c or %c, found %c"
                   ?\" ?` (char-after start)))))

(defun =go--string-raw-to-interpreted (start end)
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

(defun =go--string-interpreted-to-raw (start end)
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
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))



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

(=add-hook 'typescript-ts-mode-hook #'=lsp-ensure)



;;; Major Modes: `rust-mode'

;; Rust is pretty simple, we want `rust-mode' and then a LSP on top:
(elpaca rust-mode
  (setq rust-format-on-save t))
(=add-hook rust-mode-hook #'lsp-ensure)



;;; ChatGPT with OpenAI

;; I have written a package for interacting with ChatGPT called
;; https://github.com/iwahbe/chat.el. This provides basic functionality to interact with
;; OpenAI's API: https://platform.openai.com/docs/api-reference/chat.
(elpaca (chat.el :host github :repo "iwahbe/chat.el")
  (=1Password-setq chat-api-key "OpenAI/API Keys/Personal" #'chat-get-api-key))



;;; Pulumi

;; I currently work for https://www.pulumi.com, and I define a set of special functions to
;; work specifically with Pulumi specific data formats.

;; The largest in terms of scope (but not code) is a special mode for Pulumi YAML:
(elpaca (pulumi-yaml :host github :repo "pulumi/pulumi-lsp"
                     :files ("editors/emacs/*"))
  (require 'pulumi-yaml)
  (=add-hook pulumi-yaml-mode-hook #'=lsp-ensure))

;; Pulumi defines its providers with a
;; https://www.pulumi.com/docs/guides/pulumi-packages/schema/. This function follows
;; internal schema links by leveraging jsonian.

(defun =pulumi-follow-schema-link ()
  "Follow a link in the pulumi schema."
  (interactive)
  (unless (derived-mode-p 'jsonian-mode)
    (user-error "Requires `jsonian-mode'"))
  (if-let* ((pos (jsonian--string-at-pos))
            (s (buffer-substring-no-properties (1+ (car pos)) (1- (cdr pos))))
            (seperator (string-search "/" s 3))
            (path (concat "[\"" (substring s 2 seperator) "\"]" "[\"" (substring s (1+ seperator)) "\"]")))
      (jsonian-find path)
    (user-error "Something went wrong")))

;; Pulumi has repos, so many repos. Often, working on a bug in one repository requires
;; linking in several others. These functions make adding go module
;; https://go.dev/ref/mod#go-mod-file-replace directives to other Pulumi repositories
;; easy.

(defun =pulumi-go-src-root ()
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

(defun =pulumi-go-projects ()
  "A list of go project paths under the pulumi org."
  (seq-map #'car
           (seq-filter (lambda (attr)
                         (and
                          (cadr attr) ;; A directory
                          (not (member (car attr) '("." ".." "templates")))))
                       (directory-files-and-attributes (=pulumi-go-src-root)))))

(defun =pulumi-go-modules (dir depth)
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
                      (lambda (x) (=pulumi-go-modules (expand-file-name x dir) (1- depth)))
                      (seq-filter (lambda (x) (not (member x '("." ".."))))
                                  (directory-files dir)))))))))

(defun =pulumi-module-path-map ()
  (let ((m (make-hash-table :test #'equal))
        (root (=pulumi-go-src-root)))
    (mapc
     (lambda (dir)
       (let* ((p (expand-file-name dir root))
              (path-and-mods (=pulumi-go-modules p 2)))
         (while path-and-mods
           (puthash (cadr path-and-mods) (car path-and-mods) m)
           (setq path-and-mods (cddr path-and-mods)))))
     (=pulumi-go-projects))
    m))

(defun =pulumi-replace (&optional arg)
  "Insert the appropriate `replace` directive for a pulumi project."
  (interactive
   (list (completing-read "Select replace target: "
                          (=pulumi-module-path-map)
                          nil t)))
  (insert "replace " arg " => "
          (file-relative-name
           (gethash arg (=pulumi-module-path-map)))
          "\n"))



;;; Custom

;; In general, we want all customizations to occur in init.el. Since there is no obvious
;; way to non-destructively disable `custom', we set it to use an external file:
;; custom.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Customizations aren't loaded by default, so we also need to instruct Emacs to load
;; custom.el if it exists.
(when (file-exists-p custom-file)
  (load custom-file))



(provide '=)
;;; init.el ends here
