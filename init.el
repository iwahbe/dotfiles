;;; init.el --- Tangled from init.org -*- lexical-binding: t; -*-

;;; Commentary:

;; This file was tangled as part of iwahbe's Emacs config.

;;; Code:

(defun =org-babel-elisp-header-footer ()
  "Add appropriate headers and footers to elisp files."
  (when (and
	 (string-prefix-p (expand-file-name user-emacs-directory) (buffer-file-name))
	 (string-suffix-p ".el" (buffer-file-name)))
    (let ((feature (string-remove-suffix
		    ".el" (string-remove-prefix
			   (expand-file-name user-emacs-directory)
			   (buffer-file-name)))))
      ;; Insert the header
      (goto-char (point-min))
      (insert ";;; " feature ".el --- Tangled from init.org -*- lexical-binding: t; -*-\n"
	      "\n"
	      ";;; Commentary:\n"
	      "\n"
	      ";; This file was tangled as part of iwahbe's Emacs config.\n"
	      "\n"
	      ";;; Code:\n"
	      "\n")
      ;; Then insert the footer
      (goto-char (point-max))
      (insert "\n"
	      "(provide '" feature ")\n"
	      ";;; " feature ".el ends here\n"))
    (lisp-indent-region (point-min) (point-max))
    (let (before-save-hook) (save-buffer))))

(add-hook 'org-babel-post-tangle-hook #'=org-babel-elisp-header-footer)

(setq-default lexical-binding t)

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
  `(let ((res ,form)) (message "dbg: %s => %s" '(,@form) res) res))

(defmacro =one-of (el &rest forms)
  "Check if EL is one of FORMS.
The nth form in FORMS is evaluated only if no previous form matched EL.
Each form in FORMS is compared against EL with `eq'."
  (let ((name (gensym "el")))
    `(let ((,name ,el))
       (or ,@(mapcar
	      (lambda (form)
		`(eq ,name ,form))
	      forms)))))

(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

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

(defvar =assets-directory (expand-file-name "assets" user-emacs-directory)
  "The directory containing large runtime assets, such as images.")

(defvar elpaca-directory (=cache-subdirectory "elpaca"))

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

(defun display-startup-echo-area-message ()
  "Override the default help message by redefining the called function."
  (message "Loaded %s packages in %f seconds"
	   (length (elpaca--queued))
	   (float-time
	    (time-subtract
	     elpaca-after-init-time
	     before-init-time))))

(defun =splash-buffer (&optional window)
  "The splash screen.
It is assumed that the splash screen will occupy the whole frame
when it is created.
WINDOW is passed via `window-size-change-functions'.  It is ignored."
  (ignore window)
  (if (not (=one-of (current-buffer)
		    (get-buffer "*scratch*")
		    (get-buffer "*Splash Screen*")))
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

(setq initial-buffer-choice #'=splash-buffer)

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

(defun =splash-buffer--graphic ()
  "Display the splash screen with graphics."
  (let* ((img
	  (create-image
	   =emacs-graphic-banner
	   nil nil :width (* (/ (frame-pixel-width) 3) 2)))
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

(defun =splash-buffer--text ()
  "Display the splash screen with only text."
  (let ((banner =emacs-text-banner)
	(empty-line "\n"))
    (dotimes (_ (- (/ (frame-height) 3) (/ (length banner) 2) 2))
      (insert empty-line))
    (mapc (lambda (x) (insert x "\n")) banner))
  (let ((fill-column (frame-width)))
    (center-region (point-min) (point-max))))

(setq initial-scratch-message nil)

(fset #'yes-or-no-p #'y-or-n-p)

(setq ring-bell-function #'ignore)

(defun =load-theme (theme)
  "Load THEME without asking for permission."
  (load-theme (pcase theme
		('light 'spacemacs-light)
		('dark 'spacemacs-dark)
		(other other))
	      t)
  ;; Disable previous themes
  (mapc #'disable-theme (cdr custom-enabled-themes)))

(elpaca spacemacs-theme

  (if (boundp 'ns-system-appearance)
      (=add-hook ns-system-appearance-change-functions #'=load-theme)

    (=load-theme 'light)))

(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

(line-number-mode +1)
(column-number-mode +1)

(elpaca helpful
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-command] #'helpful-command))

(setq native-comp-async-report-warnings-errors 'silent)

(load (expand-file-name "gtq.el" user-emacs-directory))

(global-set-key (kbd "C-'") #'gtq-goto)

(elpaca (ws-butler :host github :repo "hlissner/ws-butler")

  (ws-butler-global-mode))

(setq-default fill-column 90)

(setq save-place-file (=cache-file "places"))
(save-place-mode +1)

(setq auto-save-list-file-prefix
      (concat (=cache-subdirectory "auto-save-list") ".saves-"))

(setq backup-directory-alist `(("." . ,(=cache-subdirectory "backup"))))

(setq project-list-file (=cache-file "projects"))

(defun =project-set-switch-commands (pallet)
  "Set `project-switch-commands'.

This function alters the commands passed in via PALLET to make
them aware of the new project."
  (setq project-switch-commands
	(mapcar
	 (lambda (x) (cons
		      (lambda ()
			(interactive)
			(let ((default-directory
			       (or project-current-directory-override
				   default-directory)))
			  (funcall-interactively (car x))))
		      (cdr x)))
	 pallet)))

(=project-set-switch-commands
 '((project-find-file "Find file" "f")
   (consult-find "`find` file" "C-f")
   (consult-ripgrep "Find regexp" "g")
   (magit "Git" "v")
   (vterm "Shell" "t")))

(elpaca corfu

  (setq corfu-auto t          ;; Complete when available
	corfu-auto-delay 0    ;; Without any delay
	corfu-auto-prefix 1)  ;; Wait only for the first character

  (global-corfu-mode)

  (define-key corfu-map (kbd "RET") nil t)

  (dolist (spc '("C-@" "C-SPC"))
    ;; C-@ works in the terminal, but not in GUI.
    ;; C-SPC works in GUI, but not in the terminal.
    (define-key corfu-map (kbd spc) #'corfu-insert)))

(unless (display-graphic-p)
  ;; Since we don't need the additional mode on GUI, only download it
  ;; when on a TTY.
  (elpaca (corfu-terminal
	   :type git
	   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    (corfu-terminal-mode +1)))

;; A vertical completion framework, applying a nicer UX to default
;; compleating-read style completion.
(elpaca vertico
  (setq vertico-cycle t)
  (vertico-mode))

(elpaca marginalia (marginalia-mode))

(elpaca orderless
  (setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles basic partial-completion)))))

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

(=add-hook 'text-mode-hook
  #'flyspell-mode
  #'visual-line-mode)

(=add-hook prog-mode-hook #'flyspell-prog-mode)

(=add-hook prog-mode-hook #'flymake-mode)

(elpaca highlight-defined
  (setq highlight-defined-face-use-itself t) ;; Use standard faces when highlighting.
  (=add-hook emacs-lisp-mode-hook #'highlight-defined-mode))

(elpaca jsonian
  (setq jsonian-ignore-font-lock (>= emacs-major-version 29)))

(elpaca magit)

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

;; Transient does not define it's own history dir, so we do it ourselves.
(defvar =transient-cache-dir (=cache-subdirectory "transient")
  "The directory where transient history files are stored.")
(setq
 transient-history-file (expand-file-name "history.el" =transient-cache-dir)
 transient-values-file (expand-file-name "values.el" =transient-cache-dir)
 transient-levels-file (expand-file-name "levels.el" =transient-cache-dir))

(elpaca org)

(setq org-directory "~/Documents/org"
      org-id-locations-file (=cache-file "id-locations" "org"))

(setq org-hide-emphasis-markers t)

(setq org-pretty-entities t)

(elpaca org-bullets (=add-hook org-mode-hook #'org-bullets-mode))

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

(=add-hook org-mode-hook #'variable-pitch-mode)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "DONE(d)")
	(type "PROJ(p)")
	(type "KILL(k)")
	(type "LOOP(l)")))

(setq org-log-done 'note)

(setq org-agenda-files (list org-directory))

(setq org-agenda-span 'week)

(setq org-src-preserve-indentation t)

(elpaca org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory)
	org-roam-db-location (=cache-file "roam.db" "org"))
  ;; `org-roam-node-list' is called before a list of nodes is displayed to the user. We
  ;; use it as a prompt to turn on database syncing without slowing down startup.
  (advice-add #'org-roam-node-list :before (lambda (&rest _) (org-roam-db-autosync-mode +1))))

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
				     "https://github.com/\\([-a-zA-Z0-9]+\\)/\\([-a-zA-Z0-9]+\\)/issues/\\([0-9]+\\)"))
			      (with-temp-buffer
				(unless (equal 0
					       (call-process
						(executable-find "gh") nil t nil
						"issue" "view" (substring link (match-beginning 3) (match-end 3))
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

(elpaca vterm
  (defun =advice--vterm (fn &rest args)
    "Advice for `vterm'.
  Redirect the `default-directory' of `vterm' to be project aware.
  Fix the naming of the resulting buffer to be project unique.
  
  FN is the original `vterm' function.
  ARGS are it's arguments."
    (if-let ((project (project-current)))
        (let ((default-directory (project-root project))
  	      (vterm-buffer-name (concat "*vterm<" (project-name project) ">*")))
  	  (apply fn args))
      (apply fn args)))
  (advice-add #'vterm :around #'=advice--vterm))

(defun =advice--vterm (fn &rest args)
  "Advice for `vterm'.
Redirect the `default-directory' of `vterm' to be project aware.
Fix the naming of the resulting buffer to be project unique.

FN is the original `vterm' function.
ARGS are it's arguments."
  (if-let ((project (project-current)))
      (let ((default-directory (project-root project))
	    (vterm-buffer-name (concat "*vterm<" (project-name project) ">*")))
	(apply fn args))
    (apply fn args)))
(advice-add #'vterm :around #'=advice--vterm)

(require 'project)
(add-to-list 'project-kill-buffer-conditions
	     '(and
	       (derived-mode . vterm-mode)
	       "^\\*vterm<.*>\\*$"))

(setq vterm-environment
      (list (concat "VTERM_DATA="
		    (expand-file-name "repos/emacs-libvterm/etc/emacs-vterm-zsh.sh"
				      elpaca-directory))))

(elpaca go-mode)

(=add-hook go-mode-hook
  #'eglot-ensure
  (lambda () (add-hook 'before-save-hook #'gofmt-before-save nil t)))

(elpaca markdown-mode

  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(elpaca yaml-mode)

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
