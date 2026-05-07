;;; project-x.el --- Extra convenience features for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur
;; Copyright (C) 2026  vmargb

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Maintainer: vmargb <https://github.com/vmargb>
;; URL: https://github.com/vmargb/project-x
;; Version: 0.2.2
;; Package-Requires: ((emacs "27.1"))

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
;; - Save and restore project files and window configurations across sessions
;;
;; COMMANDS:
;;
;; project-x-window-state-save : Save the window configuration of currently open project buffers
;; project-x-window-state-load : Load a previously saved project window configuration
;; project-x-add-local-project : Conveniently add project + root marker to any dir
;; project-x-rename-session    : Rename the current project's display label
;;
;; CUSTOMIZATION:
;;
;; `project-x-window-list-file': File to store project window configurations
;; `project-x-local-identifier': String matched against file names to decide if a
;; directory is a project
;; `project-x-save-interval': Interval in seconds between autosaves of the
;; current project.
;;
;; As of 15/04/2026, this package is maintained by vmargb.
;; Original author: Karthik Chikmagalur.

;;; Code:

(require 'project)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(defvar project-prefix-map)
(defvar project-switch-commands)

(declare-function project-prompt-project-dir "project")
(declare-function project--buffer-list "project")
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

(defcustom project-x-save-interval nil
  "Saves the current project state with this interval.
When set to nil auto-save is disabled."
  :type '(choice (const :tag "Disabled" nil)
                 integer)
  :group 'project-x)

(defvar project-x-window-alist nil
  "Alist of window configurations associated with known projects.")

(defvar project-x-save-timer nil
  "Timer for auto-saving project state.")

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
  (and (or file
           (file-exists-p project-x-window-list-file))
       (with-temp-buffer
         (insert-file-contents (or file project-x-window-list-file))
         (condition-case nil
             (if-let ((win-state-alist (read (current-buffer))))
                 (setq project-x-window-alist win-state-alist)
               (message (format "Could not read %s" project-x-window-list-file)))
           (error (message (format "Could not read %s" project-x-window-list-file)))))))

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

(defun project-x-rename-session (label)
  "Rename the current project's session to LABEL.
This changes only the display name, not the project directory."
  (interactive
   (list (read-string "Session label: "
                      (project-x--current-session-label))))
  (let* ((dir (project-x--current-project-root-or-error))
         (entry (copy-tree (or (project-x--session-entry dir) nil))))
    (setf (alist-get 'label entry) label)
    (project-x--set-session-entry dir entry)
    (project-x--window-state-write)
    (message "Renamed session for %s to %s" dir label)))

(defun project-x-delete-session (&optional arg)
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

(defun project-x-window-state-save (&optional arg)
  "Save current window state of project.
With optional prefix argument ARG, query for project."
  (interactive "P")
  (when-let* ((dir (cond (arg (project-prompt-project-dir))
                         ((project-current)
                          (project-root (project-current)))))
              (dir (project-x--project-root-key dir))
              (default-directory dir))
    (unless project-x-window-alist (project-x--window-state-read))
    (let ((file-list)
          (label (project-x--session-label dir)))
      ;; Collect file-list of all the open project buffers
      (dolist (buf (project-buffers (project-current)) file-list)
        (if-let ((file-name (or (buffer-file-name buf)
                                (with-current-buffer buf
                                  (and (derived-mode-p 'dired-mode)
                                       dired-directory)))))
            (push file-name file-list)))
      (project-x--set-session-entry
       dir
       (list (cons 'label label)
             (cons 'files file-list)
             (cons 'windows (window-state-get nil t)))))
    (project-x--window-state-write) ;; save to disk
    (message (format "Saved project state for %s" dir))))

;; actual restore session happens here, once confirmed
;; that a session does exist for the selected project
;; -------------------------------------------------------
;; this has been updated to fix a major bug where identical
;; buffer-names before and after project-switch will prevent the
;; switch from happening. I call these files "squatter buffers"
(defun project-x--window-state-restore (dir)
  "Restore the saved window state for project directory DIR.
Return non-nil when a saved state was found."
  (unless project-x-window-alist (project-x--window-state-read))
  (if-let* ((project-state (project-x--session-entry dir))
            (window-config (alist-get 'windows project-state)))
      (let* ((file-list (alist-get 'files project-state))
             ;; open all project files and pair each with the bare name
             ;; that window-state-put will look up (what the buffer
             ;; was called when the session was saved).
             (name-buf-pairs
              (mapcar (lambda (file-name)
                        (cons (file-name-nondirectory file-name)
                              (find-file-noselect file-name)))
                      (seq-filter #'file-exists-p file-list)))
             ;; track squatter buffers that already own a bare name so we
             ;; can restore their names after window-state-put.
             (squatter-renames nil))
        ;; for every project buffer that ended up with a uniquified name
        ;; (like elline.el<2>) because a same-named buffer from another
        ;; project was already open, temporarily:
        ;;   1. Rename the squatter out of the way.
        ;;   2. Give the project buffer the bare name window-state-put expects.
        (dolist (pair name-buf-pairs)
          (let* ((expected (car pair))
                 (buf      (cdr pair)))
            (unless (string= (buffer-name buf) expected)
              (when-let ((squatter (get-buffer expected)))
                (let ((tmp (generate-new-buffer-name
                            (concat " *px-tmp-" expected "*"))))
                  (push (cons squatter (buffer-name squatter)) squatter-renames)
                  (with-current-buffer squatter (rename-buffer tmp))))
              (with-current-buffer buf (rename-buffer expected)))))
        ;; restore the window configuration.  Buffer names now match the
        ;; saved state, so every window will get the right buffer.
        (window-state-put window-config nil 'safe)
        ;; give squatter buffers their names back (uniquified if needed, so
        ;; the freshly restored project buffer keeps the bare name).
        (dolist (pair squatter-renames)
          (when (buffer-live-p (car pair))
            (with-current-buffer (car pair)
              (rename-buffer (cdr pair) t))))
        t)
    nil))

;; midway helper that routes both project-switch-project
;; and project-x-window-state-load into project-x--window-state-restore
;; only if a session for the current project exists
(defun project-x--restore-session-command ()
  "Restore the saved window state for the current project.
Used as the direct command executed by `project-switch-project'."
  (interactive)
  (if-let* ((project (project-current nil))
            (dir (project-root project)))
      (if (project-x--window-state-restore dir)
          (message (format "Restored project state for %s" dir))
        (message (format "No saved window state for project %s" dir)))
    (message "No current project")))

;; project-x-window-state-load -> project-switch-project -> project-x--restore-session
;; window-state-load routes to project-switch-project and
;; immediately restores session without prompting (avoids infinite recursion)
(defun project-x-window-state-load (dir)
  "Switch to DIR with `project-switch-project' and restore its saved session.
If DIR is unspecified query the user for a project instead."
  (interactive (list (funcall project-prompter)))
  (let ((project-switch-commands 'project-x--restore-session-command))
    (project-switch-project dir)))

(defun project-x-windows ()
  "Restore the last saved window state of the current project."
  (interactive)
  (if-let* ((project (project-current nil))
            (dir (project-root project)))
      (if (project-x--window-state-restore dir) ;; restore if in project
          (message (format "Restored project state for %s" dir))
        (message (format "No saved window state for project %s" dir)))
    (message "No current project")))

;; Recognize directories as projects by defining a new project backend `local'
;; -------------------------------------
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
(defun project-x-add-local-project (&optional dir)
  "Ensure DIR is recognized as a project and register it with `project.el'.
Creates a marker file from `project-x-local-identifier' if missing.
If the marker already exists (e.g., after `project-forget-project'),
simply re-register the project in memory."
  (interactive "DDirectory for project root: ")
  (let* ((dir (or dir default-directory))
         (dir (file-name-as-directory (expand-file-name dir)))
         ;; extract the marker name, handling both string and list format
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
  (unless project-x-window-alist (project-x--window-state-read))
  (let* ((target-dir (file-name-as-directory (expand-file-name dir))) ;; normalize path
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


(defun project-x--project-prompt ()
  "Use `project-prompter' to inject custom prompt to `project-switch-project'."
  (let* ((dirs (project-known-project-roots))
         (projects (delq nil (mapcar #'project--find-in-directory dirs)))
         (choices
          (mapcar
           (lambda (proj)
             (let* ((root (project-root proj))
                    (label (project-x--session-label root)))
               (cons (format "%s" label) root)))
           projects)))
    (cdr (assoc (completing-read "Switch to project: " choices nil t)
                choices))))

;;;###autoload
(define-minor-mode project-x-mode
  "Minor mode to enable extra convenience features for project.el.
When enabled, save and load project window states.
Recognize any directory that contains (or whose parent
contains) a special file as a project."
  :global t
  :lighter ""
  :group 'project-x
  (if project-x-mode
      ;; Turning the mode ON
      (progn
        (add-hook 'project-find-functions 'project-x-try-local 90)
        (add-hook 'kill-emacs-hook 'project-x--window-state-write)
        (project-x--window-state-read)
        (define-key project-prefix-map (kbd "w") #'project-x-window-state-save)
        (define-key project-prefix-map (kbd "j") #'project-x-window-state-load)
        (define-key project-prefix-map (kbd "a") #'project-x-add-local-project)
        (define-key project-prefix-map (kbd "r") #'project-x-rename-session)
        (define-key project-prefix-map (kbd "d") #'project-x-delete-session)
        (advice-add 'project-switch-project :around #'project-x--dynamic-switch-commands)

        (when project-x-save-interval
          (setq project-x-save-timer
                (run-with-timer 0 (max project-x-save-interval 5)
                                #'project-x-window-state-save))))

    ;; Turning the mode OFF
    (remove-hook 'project-find-functions #'project-x-try-local)
    (remove-hook 'kill-emacs-hook #'project-x--window-state-write)
    (define-key project-prefix-map (kbd "w") nil)
    (define-key project-prefix-map (kbd "j") nil)
    (define-key project-prefix-map (kbd "a") nil)
    (define-key project-prefix-map (kbd "r") nil)
    (define-key project-prefix-map (kbd "d") nil)
    ;; remove dynamic menu advice
    (advice-remove 'project-switch-project #'project-x--dynamic-switch-commands)

    (when (timerp project-x-save-timer)
      (cancel-timer project-x-save-timer))))

(provide 'project-x)
;;; project-x.el ends here
