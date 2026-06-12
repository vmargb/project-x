;;; project-x.el --- Extra convenience features for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur
;; Copyright (C) 2026  vmargb

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Maintainer: vmargb <https://github.com/vmargb>
;; URL: https://github.com/vmargb/project-x
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: project, convenience, session, tools

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; project-x provides some convenience features for project.el:
;; - Recognize any directory with a `.project' file as a project.
;; - Save and restore project files and window configurations across sessions.
;; - (Optional) `project-x-tabs-mode', with unique tab
;;   names that stay in sync with renamed sessions.
;;
;; COMMANDS:
;;
;; project-x-window-state-save : Save the window configuration of currently open project buffers
;; project-x-window-state-load : Load a previously saved project window configuration
;; project-x-add-local-project : Conveniently add project + root marker to any dir
;; project-x-rename-session    : Rename the current project's display label
;;
;; TAB COMMANDS (project-x-tabs-mode, requires Emacs 28+):
;; project-x-detach-buffer-to-tab  : Move the current buffer to its project's tab
;; project-x-change-tab-root-dir   : Manually attach the current tab to a project
;;
;; CUSTOMIZATION:
;; `project-x-window-list-file': File to store project window configurations
;; `project-x-local-identifier': String matched against file names to decide if a
;; directory is a project
;; `project-x-save-interval': Interval in seconds between autosaves of the
;; current project.
;;
;; TAB CUSTOMIZATION:
;; `project-x-tab-name-format'           : Format for disambiguating conflicting tab names
;; `project-x-default-tab-name'          : Name of the initial/fallback tab
;; `project-x-tab-bury-buffer'           : Bury instead of kill when buffer is in another tab
;; `project-x-tab-kill-buffers-on-close' : Kill project buffers when tab is closed
;; `project-x-tab-find-file-integration' : Auto-switch to a file's project tab on find-file
;; `project-x-tab-override-commands'     : Commands always run in the tab's root directory

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defvar project-prefix-map)
(defvar project-switch-commands)

(declare-function project-prompt-project-dir "project")
(declare-function project-buffers "project")

(defgroup project-x nil
  "Convenience features for the Project library."
  :group 'project)

;; Persistent project sessions
;; -------------------------------------
(defcustom project-x-window-list-file
  (locate-user-emacs-file "project-window-list")
  "File in which to save project window configurations by default."
  :type 'file
  :group 'project-x)

(defcustom project-x-auto-save-delay nil
  "Seconds of idle time before auto-saving project state.
Set to nil to disable auto-save.  Replaces the old timer-based interval."
  :type '(choice (const :tag "Disabled" nil)
                 number)
  :group 'project-x)

(defcustom project-x-save-extra-buffers nil
  "Whether to save and restore non-file buffers (Magit, Eshell, Compilation).
If changed to nil, existing saved extra buffers are ignored during restoration."
  :type 'boolean
  :group 'project-x)

(defvar project-x-window-alist nil
  "Alist of window configurations associated with known projects.")

(defvar project-x--auto-save-timer nil
  "Idle timer for debounced auto-saving.")

;; I/O
;; ===============

(defun project-x--window-state-write (&optional file)
  "Write project window states to `project-x-window-list-file'.
If FILE is specified, write to it instead."
  (when project-x-window-alist
    (require 'pp)
    (unless file (make-directory (file-name-directory project-x-window-list-file) t))
    (with-temp-file (or file project-x-window-list-file)
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-level nil) (print-length nil))
        (pp project-x-window-alist (current-buffer))))
    (message "Wrote project window state to %s" (or file project-x-window-list-file))))

(defun project-x--window-state-read (&optional file)
  "Read project window states from `project-x-window-list-file'.
If FILE is specified, read from it instead."
  (and (or file (file-exists-p project-x-window-list-file))
       (with-temp-buffer
         (insert-file-contents (or file project-x-window-list-file))
         (condition-case nil
             (if-let ((win-state-alist (read (current-buffer))))
                 (setq project-x-window-alist win-state-alist)
               (message "Could not read %s" project-x-window-list-file))
           (error (message "Could not read %s" project-x-window-list-file))))))

;; helper to normalize the path for a project consistently
(defun project-x--project-root-key (dir)
  "Return a normalized alist key for project DIR."
  (file-name-as-directory (expand-file-name dir)))

(defun project-x--ensure-window-state-loaded ()
  "Load the saved session file into memory when needed."
  (unless project-x-window-alist
    (project-x--window-state-read)))

(defun project-x--session-entry (dir)
  "Return the saved session entry for DIR, or nil."
  (project-x--ensure-window-state-loaded)
  (alist-get (project-x--project-root-key dir) project-x-window-alist nil nil #'equal))

(defun project-x--session-has-window-state-p (dir)
  "Return non-nil when DIR has a saved window state."
  (and-let* ((entry (project-x--session-entry dir)))
    (alist-get 'windows entry)))

(defun project-x--default-session-label (dir)
  "Return a sensible default label for DIR."
  (file-name-nondirectory
   (directory-file-name (project-x--project-root-key dir))))

(defun project-x--session-label (dir)
  "Return the friendly label for DIR, falling back to a default."
  (or (alist-get 'label (project-x--session-entry dir))
      (project-x--default-session-label dir)))

(defun project-x--set-session-entry (dir entry)
  "Store ENTRY for DIR and keep the in-memory alist normalized."
  (setf (alist-get (project-x--project-root-key dir)
                   project-x-window-alist nil nil #'equal)
        entry)
  entry)

(defun project-x--current-project-root ()
  "Return the current project root or nil."
  (when-let* ((project (project-current nil)))
    (project-root project)))

(defun project-x--current-project-root-or-error ()
  "Return the current project root, signaling an error when absent."
  (or (project-x--current-project-root)
      (user-error "No current project")))

(defun project-x--current-session-label ()
  "Return the label for the current project."
  (project-x--session-label (project-x--current-project-root-or-error)))

;; Commands
;; ==================

;;;###autoload
(defun project-x-rename-session (label)
  "Rename the current project's session to LABEL.
This changes only the display name, not the project directory.
When `project-x-tabs-mode' is active, the corresponding tab is also renamed."
  (interactive
   (list (read-string "Session label: "
                      (project-x--current-session-label))))
  (let* ((dir (project-x--current-project-root-or-error))
         (entry (copy-tree (or (project-x--session-entry dir) nil))))
    (setf (alist-get 'label entry) label)
    (project-x--set-session-entry dir entry)
    (project-x--window-state-write)
    ;; sync the tab name when tabs mode is active
    ;; forward-referenced via fboundp so Section 1 does not depend on Section 2
    (when (and (bound-and-true-p project-x-tabs-mode)
               (fboundp 'project-x--tab-sync-label))
      (project-x--tab-sync-label dir label))
    (message "Renamed session for %s to %s" dir label)))

;;;###autoload
(defun project-x-clear-session (&optional arg)
  "Delete the saved session for the current project.
With optional prefix argument ARG, query for a project instead."
  (interactive "P")
  (when-let* ((dir (cond (arg (project-prompt-project-dir))
                         ((project-current)
                          (project-root (project-current)))))
              (key (project-x--project-root-key dir)))
    (project-x--ensure-window-state-loaded)
    (if (assoc key project-x-window-alist #'equal)
        (when (yes-or-no-p (format "Delete saved session for %s? " key))
          (setq project-x-window-alist
                (cl-remove key project-x-window-alist :key #'car :test #'equal))
          (project-x--window-state-write)
          (message "Deleted session for %s" key))
      (message "No saved session for %s" key))))

;; gets all buffer data for the project/session including non-file buffers
;; extra buffers are now properly guarded behind `project-x-save-extra-buffers'
(defun project-x--buffer-session-data (buf)
  "Extract session data for BUF.
Returns (buffer-name . (:type TYPE :data ...)) or nil if buffer should be ignored."
  (let ((name (buffer-name buf))
        (mode (buffer-local-value 'major-mode buf))
        (dir  (buffer-local-value 'default-directory buf)))
    (cond
     ;; explicitly ignore special/ephemeral buffers
     ((string-match-p "^\\*\\(scratch\\|Messages\\|Warnings\\|Compile-Log\\|Backtrace\\|Help\\|info\\|Customize\\|Occur\\|grep\\|Flymake\\)\\*$" name) nil)
     ;; file buffers
     ((buffer-file-name buf)
      (cons name `(:type file :path ,(buffer-file-name buf))))
     ;; dired
     ((eq mode 'dired-mode)
      (cons name `(:type dired :path ,(with-current-buffer buf dired-directory))))
     ;; magit
     ((and project-x-save-extra-buffers
           (eq mode 'magit-status-mode) (fboundp 'magit-status-mode))
      (cons name `(:type magit :root ,dir)))
     ;; eshell
     ((and project-x-save-extra-buffers (eq mode 'eshell-mode))
      (cons name `(:type eshell :dir ,dir)))
     ;; compilation
     ((and project-x-save-extra-buffers (eq mode 'compilation-mode))
      (cons name `(:type compilation :dir ,dir)))
     ;; skip everything else by default
     (t nil))))

;;;###autoload
(defun project-x-window-state-save (&optional arg)
  "Save current window state of project.
With optional prefix argument ARG, query for a project.
When `project-x-tabs-mode' is active, saves the tab's bound project
rather than the visited buffers project."
  (interactive "P")
  (when-let*
      ((dir (cond
             (arg (project-prompt-project-dir))
             ;; when tabs mode is on, prefer the tabs project root so that
             ;; visiting a foreign file does not save to the wrong project
             ((and (bound-and-true-p project-x-tabs-mode)
                   (fboundp 'project-x--tab-get-root)
                   (project-x--tab-get-root)))
             ((project-current)
              (project-root (project-current)))))
       (dir (project-x--project-root-key dir))
       (default-directory dir))
    (project-x--ensure-window-state-loaded)
    (let ((buffer-data nil)
          (label (project-x--session-label dir)))
      ;; collect session data for all open project buffers
      (dolist (buf (project-buffers (project-current)))
        (when-let ((data (project-x--buffer-session-data buf)))
          (push data buffer-data)))
      (project-x--set-session-entry
       dir
       (list (cons 'label label)
             (cons 'buffers buffer-data)
             (cons 'windows (window-state-get nil t)))))
    (project-x--window-state-write) ;; save to disk
    (message (format "Saved project state for %s" dir))))

;; not every buffer is a file, for buffers that don't have file paths
;; (eshell-mode, magit-status-mode, compilation) they will need different init calls
;; it does this by reading the :type metadata (file, dired, magit, eshell, compilation)
;; Handles three formats for backwards compatibility:
;;   1. New: (\"buf-name\" . (:type TYPE :data ...))
;;   2. Old v1: (\"/path/to/file\" . \"buf-name\")
;;   3. Legacy: \"/path/to/file\" (plain string)
(defun project-x--restore-buffer (item)
  "Create/return buffer from session ITEM.
Returns (expected-name . buffer) or nil."
  (cond
   ;; format 3: plain string (legacy sessions)
   ((stringp item)
    (when (file-exists-p item)
      (let ((name (file-name-nondirectory (directory-file-name item))))
        (cons name (find-file-noselect item)))))
   ;; format 1: New plist-based format (name . (:type ...))
   ;; must come before format 2 because car is also a string
   ((and (consp item) (listp (cdr item)))
    (let* ((buf-name (car item))
           (props (cdr item))
           (type (plist-get props :type)))
      (cond
       ((eq type 'file)
        (let ((path (plist-get props :path)))
          (when (file-exists-p path)
            (cons buf-name (find-file-noselect path)))))
       ((eq type 'dired)
        (let ((path (plist-get props :path)))
          (when (file-exists-p path)
            (cons buf-name (dired-noselect path)))))
       ;; `generate-new-buffer' optimisation over `get-buffer-create'
       ;; now also properly guarded by `project-x-save-extra-buffers'
       ((eq type 'magit)
        (when (and project-x-save-extra-buffers
                   (file-exists-p (plist-get props :root)))
          (cons buf-name
                (with-current-buffer (generate-new-buffer buf-name)
                  (setq default-directory (plist-get props :root))
                  (when (fboundp 'magit-status-mode) (magit-status-mode))
                  (current-buffer)))))
       ((eq type 'eshell)
        (when project-x-save-extra-buffers
          (cons buf-name
                (with-current-buffer (generate-new-buffer buf-name)
                  (setq default-directory (plist-get props :dir))
                  (unless (eq major-mode 'eshell-mode) (eshell-mode))
                  (current-buffer)))))
       ((eq type 'compilation)
        (when project-x-save-extra-buffers
          (cons buf-name
                (with-current-buffer (generate-new-buffer buf-name)
                  (compilation-mode)
                  (setq default-directory (plist-get props :dir))
                  (insert ";; Compilation session restored.\n")
                  (current-buffer)))))
       (t nil))))
   ;; format 2: Cons cell with string car AND string cdr (old v1: path . name)
   ((and (consp item) (stringp (car item)) (stringp (cdr item)))
    (let ((path (car item)) (name (cdr item)))
      (when (file-exists-p path)
        (cons name (find-file-noselect path)))))
   (t nil)))

;; actual restore session happens here, once confirmed
;; that a session does exist for the selected project
;; -------------------------------------------------------
;; this has been updated to fix a major bug where identical
;; buffer-names before and after project-switch will prevent the
;; switch from happening. I call these files "squatter buffers"
(defun project-x--window-state-restore (dir)
  "Restore the saved window state for project directory DIR.
Returns `restored' on success, `stale' when session buffers are all gone,
or nil when no session was found."
  (project-x--ensure-window-state-loaded)
  (if-let* ((project-state (project-x--session-entry dir))
            (window-config (alist-get 'windows project-state))
            ;; support both new 'buffers key and legacy 'files key
            (buffer-items (or (alist-get 'buffers project-state)
                              (alist-get 'files project-state))))

      ;; prevent GC pauses and UI thrashing for large projects
      (let* ((gc-cons-threshold (max gc-cons-threshold (* 50 1024 1024)))
             (inhibit-redisplay t)
             (inhibit-message t)
             (name-buf-pairs (delq nil (mapcar #'project-x--restore-buffer buffer-items)))
             (squatter-renames nil))
        ;; if every saved buffer failed to restore (files deleted, remote gone,
        ;; extra-buffers turned off, etc.) report stale rather than applying a
        ;; window config that references no live buffers, which would silently
        ;; leave the user on whatever they had open before
        (if (null name-buf-pairs)
            'stale
          ;; handle uniquify/squatter buffer collisions
          (dolist (pair name-buf-pairs)
            (let ((expected (car pair))
                  (buf      (cdr pair)))
              (unless (string= (buffer-name buf) expected)
                (when-let ((squatter (get-buffer expected)))
                  (let ((tmp (generate-new-buffer-name
                              (concat " *px-tmp-" expected "*"))))
                    (push (cons squatter (buffer-name squatter)) squatter-renames)
                    (with-current-buffer squatter
                      ;; disable uniquify strictly during the temporary swap
                      (let ((uniquify-buffer-name-style nil))
                        (rename-buffer tmp)))))
                (with-current-buffer buf
                  (let ((uniquify-buffer-name-style nil))
                    (rename-buffer expected))))))

          ;; restore the window configuration
          (window-state-put window-config nil 'safe)

          ;; give squatter buffers their names back safely
          ;; uniquify is active here to safely handle any final collisions
          (dolist (pair squatter-renames)
            (when (buffer-live-p (car pair))
              (with-current-buffer (car pair)
                (rename-buffer (cdr pair) t))))
          'restored))
    nil))

;; midway helper that routes both project-switch-project
;; and project-x-window-state-load into project-x--window-state-restore
;; only if a session for the current project exists
(defun project-x--restore-session-command ()
  "Restore the saved window state for the current project.
Used as the direct command executed by `project-switch-project'.
Falls back to Dired at the project root when no session exists or
when all session files have been deleted."
  (interactive)
  (if-let* ((project (project-current nil))
            (dir (project-root project)))
      (pcase (project-x--window-state-restore dir)
        ('restored (message "Restored project state for %s" dir))
        ('stale    (dired dir)
                   (message "Session files no longer exist, opened project root in Dired instead."))
        (_         (dired dir)
                   (message "No saved session for this project, opened project root in Dired instead.")))
    (message "No current project")))

;; project-x-window-state-load -> project-switch-project -> project-x--restore-session
;; window-state-load routes to project-switch-project and
;; immediately restores session without prompting (avoids infinite recursion)
;;;###autoload
(defun project-x-window-state-load (dir)
  "Switch to DIR and restore its saved session.
When `project-x-tabs-mode' is active, creates or selects the projects
dedicated tab before restoring, so the session lands in the right tab."
  (interactive (list (funcall project-prompter)))
  (if (and (bound-and-true-p project-x-tabs-mode)
           (fboundp 'project-x--tab-select-or-create)
           (require 'tab-bar nil t))
      ;; directly handle tab + restore, bypassing `project-switch-project'
      ;; so it always restores even when switching to an already-open tab
      (let ((dir (expand-file-name dir)))
        (project-x--tab-select-or-create dir)
        (pcase (project-x--window-state-restore dir)
          ('restored (message "Restored project state for %s" dir))
          ('stale    (dired dir)
                     (message "Session files no longer exist, opened project root in Dired."))
          (_         (dired dir)
                     (message "No saved session, opened project root in Dired."))))
    ;; no tabs mode: original behaviour with project-switch-project
    (let ((project-switch-commands 'project-x--restore-session-command))
      (project-switch-project dir))))

;;;###autoload
(defun project-x-windows ()
  "Restore the last saved window state of the current project.
Falls back to Dired at the project root when no session exists."
  (interactive)
  (if-let* ((project (project-current nil))
            (dir (project-root project)))
      (pcase (project-x--window-state-restore dir)
        ('restored (message "Restored project state for %s" dir))
        ('stale    (dired dir)
                   (message "Session files no longer exist, opened project root in Dired instead."))
        (_         (dired dir)
                   (message "No saved session for this project, opened project root in Dired instead.")))
    (message "No current project")))


;; Tab for each project integration (requires Emacs 28+, tab-bar-mode)
;; =========================================================================
;;
;; each project is given a tab, with context isolation built in.
;; all commands in `project-x-tab-override-commands' are run in the current
;; tabs root directory automatically ti preserve context
;; tab names are derived from session labels `project-x-rename-session'
;; so renaming a session automatically syncs with the tab name
;; the unique naming engine (adapted from otpp, credit: Abdelhak Bougouffa) resolves
;; tab name conflicts between projects with identical names by appending the minimal
;; distinguishing path component, such as "backend[project1]" vs "backend[project2]"

(defgroup project-x-tabs nil
  "Tab-per-project integration for project-x."
  :group 'project-x)

;; Customisation
;; =======================

(defcustom project-x-tab-name-format "%s[%s]"
  "Format for disambiguating tab names that have the same base name.
The first %s is the base (session label), the second %s is the
distinguishing path suffix."
  :type 'string
  :group 'project-x-tabs)

(defcustom project-x-default-tab-name "*default*"
  "Name for the initial/fallback tab when no project is active.
Can also be a no-argument function that returns the name."
  :type '(choice string function)
  :group 'project-x-tabs)

(defcustom project-x-rename-initial-tab t
  "When non-nil, rename the single initial tab to `project-x-default-tab-name'."
  :type 'boolean
  :group 'project-x-tabs)

(defcustom project-x-tab-bury-buffer t
  "Bury a buffer instead of killing it if it's visible in another tab.
This only takes effect when `kill-buffer' is called non-interactively"
  :type 'boolean
  :group 'project-x-tabs)

(defcustom project-x-tab-kill-buffers-on-close nil
  "Whether to kill a projects buffers when its tab is closed."
  :type '(choice (const :tag "Don't kill" nil)
                 (const :tag "Kill without asking" t)
                 (const :tag "Ask for confirmation" "ask"))
  :group 'project-x-tabs)

(defcustom project-x-tab-find-file-integration nil
  "When non-nil, opening a file with `find-file' switches to its projects tab."
  :type 'boolean
  :group 'project-x-tabs
  :set (lambda (sym val)
         (let ((was-on (bound-and-true-p project-x-tabs-mode)))
           (when was-on (project-x-tabs-mode -1))
           (set-default sym val)
           (when was-on (project-x-tabs-mode 1)))))

(defcustom project-x-tab-override-commands
  '(project-find-file project-find-dir project-kill-buffers project-switch-to-buffer
    project-shell project-eshell project-dired project-compile
    project-find-regexp project-query-replace-regexp)
  "When `project-x-tabs-mode' is active, every command in this list is advised.
with `default-directory' bound to the tabs root, regardless of the file you're on."
  :type '(repeat function)
  :group 'project-x-tabs
  :set (lambda (sym val)
         (let ((was-on (bound-and-true-p project-x-tabs-mode)))
           (when was-on (project-x-tabs-mode -1))
           (set-default sym val)
           (when was-on (project-x-tabs-mode 1)))))

;; Internal state
;; ====================

(defvar project-x--tab-name-map (make-hash-table :test 'equal)
  "Expanded project root which gives you a name-info alist.
Each value is: ((dir-name . STR) (base-name . STR-OR-NIL) (unique-name . STR))")

(defconst project-x--tab-root-attr 'project-x-root-dir
  "The symbol stored as a tab plist key to hold the bound project root.")

;; for each project with a display name (base-name), compare its
;; path against every other project sharing that name. Walk both paths
;; from deepest to shallowest, dropping identical parts from the front
;; *until* they differ. The remaining elements of the conflict determines the
;; minimal suffix needed to disambiguate, the result is appended as "[suffix]"

(defun project-x--tab-path-elements (dir)
  "Return components of DIR ordered from deepest to shallowest."
  (let ((dir (directory-file-name (expand-file-name dir))))
    (butlast (reverse
              (if (fboundp 'file-name-split) ; emacs 28+
                  (file-name-split dir)
                (split-string dir "/"))))))

(defun project-x--tab-unique-path-elements (dir1 dir2 &optional base1 base2)
  "Return the differing tail elements of DIR1 and DIR2 as a cons (ELS1 . ELS2).
BASE1 / BASE2 are optional session labels.  When a custom label differs from the
physical directory name it is prepended so label-based conflicts are also resolved"
  (let ((els1 (project-x--tab-path-elements dir1))
        (els2 (project-x--tab-path-elements dir2)))
    (when (and base1 (not (equal base1 (car els1)))) (push base1 els1))
    (when (and base2 (not (equal base2 (car els2)))) (push base2 els2))
    (while (and (car els1) (car els2) (string= (car els1) (car els2)))
      (pop els1) (pop els2))
    (cons els1 els2)))

(defun project-x--tab-compute-unique-name (dir)
  "Compute the unique tab name for the project at DIR.
Reads current `project-x--tab-name-map' and return the minimal
unambiguous name string."
  (let* ((dir (expand-file-name dir))
         (entry (gethash dir project-x--tab-name-map))
         (dir-name (file-name-nondirectory (directory-file-name dir)))
         (base (or (alist-get 'base-name entry) dir-name))
         max-path
         (max-len most-negative-fixnum)
         (min-len most-positive-fixnum))
    (maphash
     (lambda (other-dir other-entry)
       (unless (string= dir other-dir)
         (let ((other-base (or (alist-get 'base-name other-entry)
                               (alist-get 'dir-name  other-entry))))
           (when (string= base other-base)
             (let* ((diff (project-x--tab-unique-path-elements
                           dir other-dir base other-base))
                    (els (car diff))
                    (len (length els)))
               (setq min-len (min min-len len))
               (when (> len max-len)
                 (setq max-len len max-path els)))))))
     project-x--tab-name-map)
    (if (null max-path)
        base  ; no conflicts means use base name as-is
      (let* ((take   (1+ (- max-len min-len)))
             (suffix (string-join
                      (reverse (butlast max-path (- (length max-path) take)))
                      "/")))
        (if (string-empty-p suffix)
            base
          (format project-x-tab-name-format base suffix))))))

(defun project-x--tab-name-register (dir)
  "Ensure DIR is registered in `project-x--tab-name-map'.
The base-name is taken from the current session label, which is
instead the directory name if no custom label has been set yet."
  (let* ((dir      (expand-file-name dir))
         (dir-name (file-name-nondirectory (directory-file-name dir)))
         (base     (project-x--session-label dir)))
    (unless (gethash dir project-x--tab-name-map)
      (puthash dir `((dir-name   . ,dir-name)
                     (base-name  . ,base)
                     (unique-name . ,base))
               project-x--tab-name-map))))

(defun project-x--tab-name-unregister (dir)
  "Unregister DIR from `project-x--tab-name-map'."
  (remhash (expand-file-name dir) project-x--tab-name-map))

(defun project-x--tab-name-map-cleanup ()
  "Remove map entries from projects that no longer have an open tab."
  (let ((live-dirs (delq nil
                         (mapcar (lambda (tab)
                                   (when-let ((r (project-x--tab-get-root tab)))
                                     (expand-file-name r)))
                                 (funcall tab-bar-tabs-function)))))
    (maphash (lambda (dir _)
               (unless (member dir live-dirs)
                 (remhash dir project-x--tab-name-map)))
             (copy-hash-table project-x--tab-name-map))))

;; Tab attribute accessors
;; =================================

(defun project-x--tab-get-root (&optional tab)
  "Return the project root stored in TAB (or the current tab if nil)."
  (alist-get project-x--tab-root-attr
             (or tab (tab-bar--current-tab))))

(defun project-x--tab-set-root (dir)
  "Store DIR as the project root for the current tab and register it."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (idx  (tab-bar--current-tab-index tabs))
         (tab  (nth idx tabs))
         (slot (assq project-x--tab-root-attr tab))
         (expanded (and dir (expand-file-name dir))))
    (if slot
        (setcdr slot expanded)
      (when expanded
        (nconc tab `((,project-x--tab-root-attr . ,expanded)))))
    (when expanded
      (project-x--tab-name-register expanded))))

(defun project-x--tab-find-by-root (dir)
  "Return a list of tabs where the root matches DIR."
  (let ((target (expand-file-name dir)))
    (seq-filter (lambda (tab)
                  (when-let ((root (project-x--tab-get-root tab)))
                    (string= (expand-file-name root) target)))
                (funcall tab-bar-tabs-function))))

;; Tab name update
;; =========================

(defun project-x--tab-update-names ()
  "Recompute unique names for all project tabs and rename them in the tab bar."
  (project-x--tab-name-map-cleanup)
  ;; first pass: update the unique-name field in the map for every entry
  (maphash (lambda (dir entry)
             (let ((uname (project-x--tab-compute-unique-name dir)))
               (setcdr (assq 'unique-name entry) uname)))
           project-x--tab-name-map)
  ;; second pass: write the names into the actual tab objects
  (dolist (tab (funcall tab-bar-tabs-function))
    (when-let* ((root  (project-x--tab-get-root tab))
                (entry (gethash (expand-file-name root) project-x--tab-name-map))
                ;; tabs the user has explicitly renamed with `tab-bar-rename-tab'
                ((not (eq (alist-get 'explicit-name tab) t))))
      (setcdr (assq 'name tab) (alist-get 'unique-name entry))
      (setcdr (assq 'explicit-name tab) 'project-x)))
  (force-mode-line-update t))

(defun project-x--tab-sync-label (dir label)
  "Update the tab name map for DIR to use LABEL as the new base-name.
Called from `project-x-rename-session' to keep tab names in sync."
  (let* ((expanded (expand-file-name dir))
         (entry    (gethash expanded project-x--tab-name-map)))
    (if entry
        (setcdr (assq 'base-name entry) label)
      ;; edge case: tabs mode was just enabled and map entry doesn't exist yet
      (project-x--tab-name-register expanded)
      (when-let ((e (gethash expanded project-x--tab-name-map)))
        (setcdr (assq 'base-name e) label)))
    (project-x--tab-update-names)))

;; Tab lifecycle
;; =================

(defun project-x--tab-select-or-create (dir)
  "Select the existing tab for project DIR or create a new one.
Returns t if a new tab was created, nil if an existing one was selected."
  (if-let ((tab (car (project-x--tab-find-by-root dir))))
      (prog1 nil
        (tab-bar-select-tab (1+ (tab-bar--tab-index tab))))
    (tab-bar-new-tab)
    ;; Reminder: `tab-bar-new-tab' duplicates the current tabs entire window layout,
    ;; which means the new tab initially shows the previous projects/tabs buffers
    ;; instead change to a single clean window immediately so that none of those
    ;; foreign buffers linger in the new tab, which would otherwise complain
    ;; "This window displayed the file..." messages when the project/tab is killed
    (delete-other-windows)
    ;; Reminder: do NOT set `default-directory' here. Because the current buffer may
    ;; belong to another project, so changing its default-directory would cause
    ;; project-kill-buffers to wrongly include it in the new projects buffer list
    ;; `project-switch-project' already sets project-current-directory-override
    ;; for subsequent commands, and project-x--tab-override-a handles command
    ;; isolation via the tab's root attribute
    (project-x--tab-set-root dir)
    (project-x--tab-update-names)
    t))

(defun project-x--tab-close-current ()
  "Close or reset the current tab after its projects buffers have been killed."
  (let* ((tabs     (funcall tab-bar-tabs-function))
         (curr-tab (assq 'current-tab tabs))
         (root     (project-x--tab-get-root curr-tab)))
    (if (length> tabs 1)
        ;; if there's more than one tab, close this one without re-triggering the hook
        (let ((tab-bar-tab-pre-close-functions
               (remq 'project-x--tab-pre-close-hook
                     tab-bar-tab-pre-close-functions)))
          (tab-bar-close-tab))
      ;; last tab, detach it from the project and restore the default name
      (let ((slot (assq project-x--tab-root-attr curr-tab)))
        (when slot (setcdr slot nil)))
      (let ((default-name (if (functionp project-x-default-tab-name)
                              (funcall project-x-default-tab-name)
                            project-x-default-tab-name)))
        (setcdr (assq 'name curr-tab) default-name)
        (setcdr (assq 'explicit-name curr-tab) 'project-x-def)))
    (when root
      (project-x--tab-name-unregister root)
      (project-x--tab-update-names))))

(defun project-x--tab-set-default-name ()
  "Rename the tab bar's initial tab to `project-x-default-tab-name'."
  (when-let* (project-x-rename-initial-tab
              (name (if (functionp project-x-default-tab-name)
                        (funcall project-x-default-tab-name)
                      project-x-default-tab-name))
              (tabs (funcall tab-bar-tabs-function))
              ((length= tabs 1))
              (tab (assq 'current-tab tabs))
              ((not (project-x--tab-get-root tab)))
              ((not (eq (alist-get 'explicit-name tab) t))))
    (setcdr (assq 'name tab) name)
    (setcdr (assq 'explicit-name tab) 'project-x-def)
    (force-mode-line-update t)))

;; Advices
;; ===============

(defun project-x--tab-switch-project-a (orig-fn dir &rest args)
  "Select/create tab for DIR, THEN restore session.
For existing tabs, the tab already holds its window configuration.
For new tabs ORIG-FN is called which triggers full session restore."
  (let ((dir (expand-file-name (or dir default-directory))))
    (if (project-x--tab-select-or-create dir)
        ;; new tab: run the full project switch (which triggers session restore)
        (apply orig-fn dir args)
      ;; existing tab selected: the tabs windows are already correct
      nil)))

(defun project-x--tab-kill-buffers-a (orig-fn &rest args)
  "Close the project tab if the user confirms `project-kill-buffers'.
If the user cancels it then the tab is left untouched.
All other bindings (tabs, curr-tab, root) are resolved AFTER the kill."
  (when-let* (((apply orig-fn args))
              (tabs     (funcall tab-bar-tabs-function))
              (curr-tab (assq 'current-tab tabs))
              (root     (project-x--tab-get-root curr-tab)))
    (if (length> tabs 1)
        ;; remove the pre-close hook so it doesn't try to kill buffers again
        (let ((tab-bar-tab-pre-close-functions
               (remq 'project-x--tab-pre-close-hook
                     tab-bar-tab-pre-close-functions)))
          (tab-bar-close-tab))
      ;; last remaining tab: detach from the project and reset its name
      (let ((default-name (if (functionp project-x-default-tab-name)
                              (funcall project-x-default-tab-name)
                            project-x-default-tab-name)))
        (when-let ((slot (assq project-x--tab-root-attr curr-tab)))
          (setcdr slot nil))
        (setcdr (assq 'name curr-tab) default-name)
        (setcdr (assq 'explicit-name curr-tab) 'project-x-def)))
    (project-x--tab-name-unregister root)
    (project-x--tab-update-names)))

(defun project-x--tab-project-current-a (orig-fn &rest args)
  "Switch tabs when a project is selected interactively.
When the user is prompted for a project and they choose one
that belongs to a different tab, switch to that tab."
  (let ((proj (apply orig-fn args)))
    (when-let* (proj
                ((car args)) ; maybe-prompt was non-nil
                (proj-dir (expand-file-name (project-root proj)))
                (curr-root (project-x--tab-get-root))
                ((not (and curr-root
                           (string= (expand-file-name curr-root) proj-dir)))))
      (project-x--tab-select-or-create proj-dir))
    proj))

(defun project-x--tab-bury-buffer-a (fn &optional buffer)
  "Advice for `kill-buffer': bury BUFFER when it is visible in another tab.
Only intercepts non-interactive calls (so explicit \\[kill-buffer] always kills).
Requires `tab-bar-get-buffer-tab' which is available in Emacs 29+."
  (if (and project-x-tab-bury-buffer
           (not (called-interactively-p 'any))
           (or (null buffer) (eq (get-buffer buffer) (current-buffer)))
           (fboundp 'tab-bar-get-buffer-tab)
           (tab-bar-get-buffer-tab buffer t t t))
      (progn
        (message "Buffer visible in another tab — burying instead of killing.")
        (bury-buffer buffer))
    (funcall fn buffer)))

(defun project-x--tab-find-file-a (orig-fn &rest args)
  "Advice for `find-file': switch to the opened file's project tab.
Only active when `project-x-tab-find-file-integration' is non-nil."
  (when-let* ((file (car args))
              (proj (project-current nil (file-name-directory
                                          (expand-file-name file))))
              (root (project-root proj)))
    (let ((project-switch-commands #'ignore))
      (project-switch-project root)))
  (apply orig-fn args))

(defun project-x--tab-pre-close-hook (tab _last-tab-p)
  "Hook for `tab-bar-tab-pre-close-functions' optionally kill project buffers.
Respects `project-x-tab-kill-buffers-on-close'."
  (when-let* ((killp project-x-tab-kill-buffers-on-close)
              (root  (project-x--tab-get-root tab))
              (proj  (project-current nil root))
              ((string= (expand-file-name (project-root proj))
                        (expand-file-name root))))
    (project-kill-buffers (not (equal killp "ask")) proj)))

(defun project-x--tab-override-a (orig-fn &rest args)
  "Run ORIG-FN with `default-directory' bound to the current tab's root."
  (let ((default-directory (or (project-x--tab-get-root) default-directory)))
    (apply orig-fn args)))

;; Commands
;; ==================

;;;###autoload
(defun project-x-detach-buffer-to-tab (buffer)
  "Switch to or create the tab for BUFFER's project, then display BUFFER there.
With a prefix argument, prompts for which buffer to detach."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "Buffer to detach: ")
           (current-buffer))))
  (with-current-buffer buffer
    (if-let* ((proj (project-current))
              (root (project-root proj)))
        (progn
          (bury-buffer)
          (project-x--tab-select-or-create root)
          (switch-to-buffer buffer))
      (user-error "Buffer %S is not part of any project" (buffer-name)))))

;;;###autoload
(defun project-x-change-tab-root-dir (dir)
  "Manually attach the current tab to the project at DIR.
Useful when you want two tabs pointing at the same project with different
window layouts, or when you created a tab independently of project switching."
  (interactive
   (list (completing-read
          "Project root for this tab (leave blank to detach): "
          (delete-dups
           (delq nil (mapcar #'project-x--tab-get-root
                             (funcall tab-bar-tabs-function)))))))
  (let* ((tabs  (funcall tab-bar-tabs-function))
         (idx   (tab-bar--current-tab-index tabs))
         (tab   (nth idx tabs))
         (slot  (assq project-x--tab-root-attr tab))
         (root  (and dir (not (string-empty-p dir)) (expand-file-name dir))))
    (if slot
        (setcdr slot root)
      (when root (nconc tab `((,project-x--tab-root-attr . ,root)))))
    (when root (project-x--tab-name-register root))
    (project-x--tab-update-names)))

;; Tabs-mode minor mode
;; =====================

;;;###autoload
(define-minor-mode project-x-tabs-mode
  "One dedicated tab per project with built-in context isolation.
`project-switch-project' creates or selects a per-project tab.
`project-kill-buffers' closes the project's tab."
  :global t
  :group 'project-x-tabs
  (if project-x-tabs-mode
      (progn
        (when (< emacs-major-version 28)
          (setq project-x-tabs-mode nil)
          (error "project-x-tabs-mode requires Emacs 28 or later"))
        (require 'tab-bar)
        (tab-bar-mode 1)
        (advice-add 'project-switch-project :around #'project-x--tab-switch-project-a)
        (advice-add 'project-kill-buffers   :around #'project-x--tab-kill-buffers-a)
        (advice-add 'project-current        :around #'project-x--tab-project-current-a)
        (advice-add 'kill-buffer            :around #'project-x--tab-bury-buffer-a)
        (when project-x-tab-find-file-integration
          (advice-add 'find-file :around #'project-x--tab-find-file-a))
        ;; run project commands in the tab's root dir
        (dolist (cmd project-x-tab-override-commands)
          (advice-add cmd :around #'project-x--tab-override-a))
        (add-hook 'tab-bar-tab-pre-close-functions #'project-x--tab-pre-close-hook)
        (add-hook 'server-after-make-frame-hook     #'project-x--tab-set-default-name)
        ;; seed the name map from any tabs that already have a root dir set
        (dolist (tab (funcall tab-bar-tabs-function))
          (when-let ((root (project-x--tab-get-root tab)))
            (project-x--tab-name-register root)))
        (project-x--tab-update-names)
        (project-x--tab-set-default-name))
    (advice-remove 'project-switch-project #'project-x--tab-switch-project-a)
    (advice-remove 'project-kill-buffers   #'project-x--tab-kill-buffers-a)
    (advice-remove 'project-current        #'project-x--tab-project-current-a)
    (advice-remove 'kill-buffer            #'project-x--tab-bury-buffer-a)
    (advice-remove 'find-file              #'project-x--tab-find-file-a)
    (dolist (cmd project-x-tab-override-commands)
      (advice-remove cmd #'project-x--tab-override-a))
    (remove-hook 'tab-bar-tab-pre-close-functions #'project-x--tab-pre-close-hook)
    (remove-hook 'server-after-make-frame-hook     #'project-x--tab-set-default-name)
    (clrhash project-x--tab-name-map)))


;; 'Local' project backend
;; ==================================================

(defcustom project-x-local-identifier ".project"
  "Filename(s) that identifies a directory as a project.
You can specify a single filename or a list of names."
  :type '(choice (string :tag "Single file")
                 (repeat (string :tag "Filename")))
  :group 'project-x)

(cl-defmethod project-root ((project (head local)))
  "Return root directory of current PROJECT."
  (cdr project))

(cl-defmethod project-name ((project (head local)))
  "Return a human-friendly name for PROJECT."
  (project-x--session-label (project-root project)))

;; now supports emacs 29+ project-vc-root-marker while keeping
;; backwards compatibility with project-x-local-identifier
(defun project-x-try-local (dir)
  "Determine if DIR is a local project.
Checks both `project-x-local-identifier' and Emacs 29's
`project-vc-extra-root-markers'.  Returns the nearest (deepest)
matching root as a `local' project."
  (let* ((local-markers (if (listp project-x-local-identifier)
                            project-x-local-identifier
                          (list project-x-local-identifier)))
         (vc-extra (and (boundp 'project-vc-extra-root-markers) ;; safely check
                        project-vc-extra-root-markers))
         (vc-extra-list (if (listp vc-extra) vc-extra (list vc-extra)))
         ;; combine, deduplicate, and filter nils
         (all-markers (seq-uniq (seq-filter #'stringp
                                            (append local-markers vc-extra-list))))
         ;; find all matching roots and pick the deepest one
         (roots (delq nil (mapcar (lambda (m) (locate-dominating-file dir m)) all-markers))))
    (when roots
      (cons 'local (car (sort roots (lambda (a b) (> (length a) (length b)))))))))

;; More reliably add local projects + root markers to project list in one go
;;;###autoload
(defun project-x-add-local-project (&optional dir)
  "Ensure DIR is recognized as a project and register it with `project.el'.
Creates a marker file from `project-x-local-identifier' if missing.
If the marker already exists, simply re-registers the project in memory."
  (interactive "DDirectory for project root: ")
  (let* ((dir (file-name-as-directory (expand-file-name (or dir default-directory))))
         (marker-name (if (listp project-x-local-identifier)
                          (car project-x-local-identifier)
                        project-x-local-identifier))
         (marker-file (expand-file-name marker-name dir)))
    ;; create marker only if it doesn't exist or we're not in a git repo
    (unless (or (file-exists-p marker-file)
                (file-exists-p (expand-file-name ".git" dir)))
      (with-temp-buffer (write-file marker-file))
      (message "Created project marker '%s' in %s" marker-name dir))
    ;; always attempt to register the project with project.el
    (if-let ((project (project-current nil dir)))
        (progn
          (project-remember-project project)
          (message "Registered project at %s" dir))
      (message "Could not recognize %s as a project. Ensure project-x-mode is active." dir))))

;; Context-aware restore session advice to project.el
;; -------------------------------------
(defun project-x--dynamic-switch-commands (orig-fun dir &rest args)
  "Dynamically include 'Restore windows' to ARGS in ORIG-FUN if a saved state exists for DIR."
  (project-x--ensure-window-state-loaded)
  (let* ((target-dir (file-name-as-directory (expand-file-name dir)))
         (has-session (project-x--session-has-window-state-p target-dir))
         (cmd-entry '(project-x-windows "Restore windows" ?j))
         (project-switch-commands ;; dynamically bind the command list
          (if (listp project-switch-commands)
              (if has-session ;; add it if its missing (to avoid duplicates)
                  (if (member cmd-entry project-switch-commands)
                      project-switch-commands
                    (append project-switch-commands (list cmd-entry)))
                (seq-remove (lambda (cmd) (eq (car-safe cmd) 'project-x-windows))
                            project-switch-commands))
            ;; else if project-switch-commands is a symbol, leave it alone
            project-switch-commands)))
    ;; execute project switch with the temporary environment
    (apply orig-fun dir args)))

;; Debounced auto-save system
;; -------------------------------------
(defun project-x--schedule-auto-save ()
  "Schedule a debounced auto-save of the current project state."
  (when (and project-x-auto-save-delay project-x-mode (project-current nil))
    (when project-x--auto-save-timer
      (cancel-timer project-x--auto-save-timer))
    (setq project-x--auto-save-timer
          (run-with-idle-timer project-x-auto-save-delay nil
                               #'project-x--auto-save-current))))

(defun project-x--auto-save-current ()
  "Auto-save the current project if it has open buffers."
  (when-let ((proj (project-current nil))
             (bufs (project-buffers proj)))
    (when bufs
      (project-x-window-state-save))))

;; updated:
;; `project--find-in-directory' would run over every root without protection
;; one bad root would abort the whole prompt. now each directory is tried
;; independently, and any error is dropped from the candidate list
;; so something like an unreachable host no longer breaks
(defun project-x--project-prompt ()
  "Use `project-prompter' to inject custom prompt to `project-switch-project'.
Silently skips projects where the directory cannot be resolved like TRAMP"
  (let* ((dirs (project-known-project-roots))
         (projects (delq nil ; remove failed directories from the project list
                         (mapcar (lambda (dir)
                                   ; catch all folders that cannot be resolved
                                   (condition-case nil
                                       (project--find-in-directory dir)
                                     (error nil)))
                                 dirs)))
         (choices (mapcar (lambda (proj)
                            (let* ((root  (project-root proj))
                                   (label (project-x--session-label root)))
                              (cons (format "%s" label) root)))
                          projects)))
    (cdr (assoc (completing-read "Switch to project: " choices nil t)
                choices))))

;; Sync with project-forget-project
;; -------------------------------------
(defun project-x--handle-forget-project (orig-fun project &rest args)
  "Remove session data when PROJECT is forgotten.
PROJECT can be either a project object or a directory string."
  (let* ((dir (if (stringp project) project (project-root project)))
         (key (project-x--project-root-key dir)))
    (project-x--ensure-window-state-loaded)
    (when (assoc key project-x-window-alist #'equal)
      (setq project-x-window-alist
            (cl-remove key project-x-window-alist :key #'car :test #'equal))
      (project-x--window-state-write))
    (apply orig-fun project args)))

;;;###autoload
(define-minor-mode project-x-mode
  "Minor mode adding session persistence and convenience features to project.el.
For tab-bar-project isolation, also enable `project-x-tabs-mode' (Emacs 28+)."
  :global t
  :lighter ""
  :group 'project-x
  (if project-x-mode
      ;; Turning the mode ON
      (progn
        (add-hook 'project-find-functions #'project-x-try-local 90)
        (add-hook 'kill-emacs-hook        #'project-x--window-state-write)
        (add-hook 'window-configuration-change-hook #'project-x--schedule-auto-save)
        (add-hook 'kill-buffer-hook                 #'project-x--schedule-auto-save)
        (project-x--window-state-read)
        ;; Keybindings (safe registration)
        (unless (lookup-key project-prefix-map (kbd "w"))
          (define-key project-prefix-map (kbd "w") #'project-x-window-state-save))
        (unless (lookup-key project-prefix-map (kbd "j"))
          (define-key project-prefix-map (kbd "j") #'project-x-window-state-load))
        (unless (lookup-key project-prefix-map (kbd "a"))
          (define-key project-prefix-map (kbd "a") #'project-x-add-local-project))
        (unless (lookup-key project-prefix-map (kbd "r"))
          (define-key project-prefix-map (kbd "r") #'project-x-rename-session))
        (unless (lookup-key project-prefix-map (kbd "d"))
          (define-key project-prefix-map (kbd "d") #'project-x-clear-session))
        (advice-add 'project-switch-project :around #'project-x--dynamic-switch-commands)
        (advice-add 'project-forget-project :around #'project-x--handle-forget-project))
    ;; Turning mode OFF
    (remove-hook 'project-find-functions #'project-x-try-local)
    (remove-hook 'kill-emacs-hook        #'project-x--window-state-write)
    (remove-hook 'window-configuration-change-hook #'project-x--schedule-auto-save)
    (remove-hook 'kill-buffer-hook                 #'project-x--schedule-auto-save)
    (define-key project-prefix-map (kbd "w") nil)
    (define-key project-prefix-map (kbd "j") nil)
    (define-key project-prefix-map (kbd "a") nil)
    (define-key project-prefix-map (kbd "r") nil)
    (define-key project-prefix-map (kbd "d") nil)
    ;; remove dynamic menu advice
    (advice-remove 'project-switch-project #'project-x--dynamic-switch-commands)
    (advice-remove 'project-forget-project #'project-x--handle-forget-project)
    (when (timerp project-x--auto-save-timer)
      (cancel-timer project-x--auto-save-timer))))

(provide 'project-x)
;;; project-x.el ends here
