;;; init.el --- Load my config for emacs -*- lexical-binding: t; -*-

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

(defun =initial-buffer-choice ()
  "The splash screen."
  (get-buffer-create "*scratch*"))

(setq initial-scratch-message nil)
(setq initial-buffer-choice #'=initial-buffer-choice)

(defun display-startup-echo-area-message ()
  "Override the default help message by redefining the called function."
  (message "Loaded %s packages in %s seconds"
	   (length (elpaca--queued))
	   (emacs-init-time)))

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

;; Move auto-save to the cache dir
(setq auto-save-list-file-prefix
      (concat (=cache-subdirectory "auto-save-list") ".saves-"))

;; Move backups to their own directory
(setq backup-directory-alist
      (list (cons "." (=cache-subdirectory "backup"))))

(setq project-list-file
      (expand-file-name "projects" (=cache-subdirectory "project")))

;; Better consulting commands
(elpaca consult
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap Info-search] #'consult-info)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  (global-set-key [remap imenu] #'consult-imenu)
  (setq consult-register-prefix nil) ;; We don't want to type a prefix
				     ;; for all searches
  (global-set-key [remap jump-to-register] #'consult-register-load)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (define-key isearch-mode-map [remap isearch-edit-string] #'consult-isearch-history))

;;; Completion

;; A vertical completion framework, applying a nicer UX to default
;; compleating-read style completion.
(elpaca vertico (vertico-mode))

(elpaca marginalia (marginalia-mode))

(elpaca corfu
  (setq corfu-auto t
	corfu-auto-delay 0
	corfu-auto-prefix 1
	completion-styles '(basic))
  (global-corfu-mode)
  (define-key corfu-map (kbd "RET") nil t)
  (dolist (d '("C-@" "C-SPC"))
    ;; C-@ works in the terminal, but not in GUI.
    ;; C-SPC works in GUI, but not in the terminal.
    (define-key corfu-map (kbd d) #'corfu-insert)))

;; The corfu display itself is restricted to GUI emacs, so we enable
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
  (setq org-directory "~/Documents/org"
	org-log-done 'note
	org-agenda-span 'week
	org-todo-keywords
	'((sequence "TODO(t)" "DONE(d)")
	  (type "PROJ(p)")
	  (type "KILL(k)")
	  (type "LOOP(l)"))
	org-agenda-files (list org-directory)))

(elpaca org-roam
  (setq org-roam-directory (expand-file-name "roam" org-directory)))

(elpaca vterm)

(elpaca go-mode
  (=defun-init go-mode
    (eglot)
    (add-hook 'before-save-hook #'gofmt-before-save nil t)))

;;; init.el ends here
