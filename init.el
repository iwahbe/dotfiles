;;; init.el --- Load my config for Emacs -*- lexical-binding: t; -*-

;;; Code:

;; Setup elpaca
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

;;; Splash screen

(defun =splash-buffer ()
  "The splash screen.
It is assumed that the splash screen will occupy the whole frame
when it is created."
  (if (not (eq (current-buffer) (get-buffer "*scratch*")))
      (current-buffer)
    (with-current-buffer (get-buffer-create "*Splash Screen*")
      (when buffer-read-only
	(read-only-mode -1))
      (unless (eq (buffer-size) 0)
	(erase-buffer))
      (if (and (display-graphic-p) (featurep 'image))
	  (let* ((img
		  (create-image
		   (nth (random (length =emacs-graphic-banners))
			=emacs-graphic-banners)
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
	    (insert-image img nil nil nil t))
	(let ((banner (nth (random (length =emacs-text-banners)) =emacs-text-banners))
	      (empty-line "\n"))
	  (dotimes (_ (- (/ (frame-height) 3) (/ (length banner) 2) 2))
	    (insert empty-line))
	  (mapc (lambda (x) (insert x "\n")) banner))
	(let ((fill-column (frame-width)))
	  (center-region (point-min) (point-max))))
      (setq cursor-type nil)
      (read-only-mode)
      (goto-char (point-min))
      (setq mode-line-format nil)
      (current-buffer))))

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

(defvar =assets-directory (expand-file-name "assets" user-emacs-directory)
  "The directory containing large runtime assets, such as images.")

(defvar =emacs-graphic-banners
  (mapcar (lambda (x) (expand-file-name x =assets-directory))
	  '("gnu-head.svg"
	    "emacs-icon.svg"))
  "A list of graphical banners to open Emacs with.
Each element is expected to be the path to a SVG file.")

(setq initial-scratch-message nil)
(setq initial-buffer-choice #'=splash-buffer)

(defun display-startup-echo-area-message ()
  "Override the default help message by redefining the called function."
  (message "Loaded %s packages in %f seconds"
	   (length (elpaca--queued))
	   (float-time
	    (time-subtract
	     elpaca-after-init-time
	     before-init-time))))

;;; UI

(setq use-short-answers t)
(setq ring-bell-function 'ignore)
(unless (display-graphic-p)
  (menu-bar-mode -1))

;; Cursor
(setq-default cursor-type 'bar)
(blink-cursor-mode -1)

(line-number-mode +1)
(column-number-mode +1)

;;; Miscellaneous

(load (expand-file-name "gtq.el" user-emacs-directory))
(global-set-key (kbd "C-'") #'gtq-goto)

;; Native compiling packages can emit warnings or errors which are not
;; actionable. We don't want these to pop up when compiling, so we
;; send them to the *Warnings* buffer.
(setq native-comp-async-report-warnings-errors 'silent)

;; Helpful is a package
(elpaca helpful
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-key] #'helpful-key)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-command] #'helpful-command))

(setq save-place-file (=cache-file "places"))
(save-place-mode +1)

(elpaca (ws-butler :host github :repo "hlissner/ws-butler")
  (ws-butler-global-mode))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Move auto-save to the cache dir
(setq auto-save-list-file-prefix
      (concat (=cache-subdirectory "auto-save-list") ".saves-"))

;; Move backups to their own directory
(setq backup-directory-alist
      (list (cons "." (=cache-subdirectory "backup"))))

;; Better consulting commands
(elpaca consult
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap Info-search] #'consult-info)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  (global-set-key [remap imenu] #'consult-imenu)

  ;; We don't want to type a prefix for all searches
  (setq consult-register-prefix nil)

  (global-set-key [remap jump-to-register] #'consult-register-load)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (define-key isearch-mode-map [remap isearch-edit-string] #'consult-isearch-history))

;;; Project Management

(setq project-list-file (=cache-file "projects")
      project-switch-commands '((project-find-file "Find file" "f")
				(consult-find "`find` file" "C-f")
				(consult-ripgrep "Find regexp" "g")
				(magit "Git" "v")
				(vterm "Shell" "t")))

;;; Completion

;; A vertical completion framework, applying a nicer UX to default
;; compleating-read style completion.
(elpaca vertico
  (setq vertico-cycle t)
  (vertico-mode))

(elpaca marginalia (marginalia-mode))

(elpaca orderless
  (setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles basic partial-completion)))))

(elpaca corfu
  (setq corfu-auto t
	corfu-auto-delay 0
	corfu-auto-prefix 1)
  (global-corfu-mode)
  (define-key corfu-map (kbd "RET") nil t)
  (dolist (d '("C-@" "C-SPC"))
    ;; C-@ works in the terminal, but not in GUI.
    ;; C-SPC works in GUI, but not in the terminal.
    (define-key corfu-map (kbd d) #'corfu-insert)))

;; The corfu display itself is restricted to GUI Emacs, so we enable
;; another mode when we are restricted to a TTY
(unless (display-graphic-p)
  ;; Since we don't need the additional mode on GUI, only download it
  ;; when on a TTY.
  (elpaca (corfu-terminal
	   :type git
	   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
    (corfu-terminal-mode +1)))

;;; Until

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(defmacro dbg(form)
  "Print FORM => RES where res is what FORM evaluate to. Return RES."
  `(let ((res ,form)) (message "dbg: %s => %s" '(,@form) res) res))

;;; Major modes

(defmacro =defun-init (mode &rest body)
  "Run BODY when MODE is initialized."
  (declare (indent defun))
  (let ((fn (intern (concat "=" (symbol-name mode) "-init"))))
    `(progn
       (defun ,fn ()
	 ,@body)
       (add-hook ',(intern (concat (symbol-name mode) "-hook")) #',fn)
       #',fn)))

(=defun-init text-mode
  (flyspell-mode))

(=defun-init prog-mode
  (flyspell-prog-mode)
  (flymake-mode))

(elpaca jsonian
  (setq jsonian-ignore-font-lock (>= emacs-major-version 29)))

(elpaca magit
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
      (message "Github link to region: %s" (car kill-ring)))))

;; Transient is used by magit, among other packages. We define it
;; separately because we want to customize it's history save files,
;; but don't actually want to depend on it ourselves.
(elpaca nil
  ;; Transient does not define it's own history dir, so we do it ourselves.
  (defvar =transient-cache-dir (=cache-subdirectory "transient")
    "The directory where transient history files are stored.")
  (setq
   transient-history-file (expand-file-name "history.el" =transient-cache-dir)
   transient-values-file (expand-file-name "values.el" =transient-cache-dir)
   transient-levels-file (expand-file-name "levels.el" =transient-cache-dir)))

(elpaca org
  ;; Set the root of the org directory. This is the expected location
  ;; for all stand-alone org documents.
  (setq org-directory "~/Documents/org"
	org-id-locations-file (=cache-file "id-locations" "org")
	org-log-done 'note
	org-agenda-span 'week
	org-todo-keywords
	'((sequence "TODO(t)" "DONE(d)")
	  (type "PROJ(p)")
	  (type "KILL(k)")
	  (type "LOOP(l)"))
	org-agenda-files (list org-directory)))

;; Because org is often used as a writing environment, we want it to
;; look nice.
(elpaca org-bullets
  (setq org-hide-emphasis-markers t
	org-startup-indented t
	org-pretty-entities t))

;; This snippet provides variable and fixed pitch fonts to different
;; parts of org-mode.  It was borrowed from
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/, and then
;; modified.
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

(=defun-init org-mode
  (org-bullets-mode)
  (variable-pitch-mode)
  (visual-line-mode))

(elpaca org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory)
	org-roam-db-location (=cache-file "roam.db" "org")))

(elpaca vterm
  (setq vterm-environment
	(list (concat "VTERM_DATA="
		      (expand-file-name "repos/emacs-libvterm/etc/emacs-vterm-zsh.sh"
					elpaca-directory)))))

(elpaca go-mode
  (=defun-init go-mode
    (eglot)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

(elpaca markdown-mode
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

;; We load user customizations at the end to make sure that the
;; init.el load succeeds.
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
