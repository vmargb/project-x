;;; project-x.el --- Extra convenience features for project.el -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur
;; Copyright (C) 2026  vmargb

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; Maintainer: vmargb <https://github.com/vmargb>
;; URL: https://github.com/vmargb/project-x
;; Version: 0.2.0
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
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'seq))
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

(defun project-x-window-state-save (&optional arg)
  "Save current window state of project.
With optional prefix argument ARG, query for project."
  (interactive "P")
  (when-let* ((dir (cond (arg (project-prompt-project-dir))
                         ((project-current)
                          (project-root (project-current)))))
              (default-directory dir))
    (unless project-x-window-alist (project-x--window-state-read))
    (let ((file-list))
      ;; Collect file-list of all the open project buffers
      (dolist (buf (project-buffers (project-current)) file-list)
        (if-let ((file-name (or (buffer-file-name buf)
                                (with-current-buffer buf
                                  (and (derived-mode-p 'dired-mode)
                                       dired-directory)))))
            (push file-name file-list)))
      (setf (alist-get dir project-x-window-alist nil nil 'equal)
            (list (cons 'files file-list)
                  (cons 'windows (window-state-get nil t)))))
    (project-x--window-state-write) ;; save to disk
    (message (format "Saved project state for %s" dir))))

(defun project-x-window-state-load (dir)
  "Load the saved window state for project with directory DIR.
If DIR is unspecified query the user for a project instead."
  (interactive (list (project-prompt-project-dir)))
  (unless project-x-window-alist (project-x--window-state-read))
  (if-let* ((project-x-window-alist)
            (project-state (alist-get dir project-x-window-alist
                                      nil nil 'equal)))
      (let ((file-list (alist-get 'files project-state))
            (window-config (alist-get 'windows project-state)))
        (dolist (file-name file-list nil)
          (find-file file-name))
        (window-state-put window-config nil 'safe)
        (message (format "Restored project state for %s" dir)))
    (message (format "No saved window state for project %s" dir))))

(defun project-x-windows ()
  "Restore the last saved window state of the chosen project."
  (interactive)
  (project-x-window-state-load (project-root (project-current))))

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

(defun project-x-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
  (if-let ((root (if (listp project-x-local-identifier)
                     (seq-some (lambda (n)
                                 (locate-dominating-file dir n))
                               project-x-local-identifier)
                   (locate-dominating-file dir project-x-local-identifier))))
      (cons 'local root)))

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
    ;; create marker only if it doesn't exist
    (unless (file-exists-p marker-file)
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
  (unless project-x-window-alist (project-x--window-state-read)) ;; ensure latest state

  (let* (;; normalize the path (resolves ~/ & enforces trailing slash)
         (target-dir (file-name-as-directory (expand-file-name dir)))
         ;; checks normalized strings against the alist keys
         (has-session (catch 'found
                        (dolist (state project-x-window-alist)
                          (when (string= target-dir
                                         (file-name-as-directory (expand-file-name (car state))))
                            (throw 'found t)))))

         (cmd-entry '(project-x-windows "Restore windows" ?j))

         ;; dynamically bind the command list (only if it's a list)
         (project-switch-commands
          (if (listp project-switch-commands)
              (if has-session
                  ;; add it if it's missing (to avoid duplicates)
                  (if (member cmd-entry project-switch-commands)
                      project-switch-commands
                    (append project-switch-commands (list cmd-entry)))
                ;; else remove it if present
                (seq-remove (lambda (cmd) (eq (car-safe cmd) 'project-x-windows))
                            project-switch-commands))
            ;; else if project-switch-commands is a symbol, leave it alone
            project-switch-commands)))
    ;; execute project switch with this temporary environment
    (apply orig-fun dir args)))

;;;###autoload
(define-minor-mode project-x-mode
  "Minor mode to enable extra convenience features for project.el.
When enabled, save and load project window states.
Recognize any directory that contains (or whose parent
contains) a special file as a project."
  :global t
  :version "0.10"
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
    ;; remove dynamic menu advice
    (advice-remove 'project-switch-project #'project-x--dynamic-switch-commands)

    (when (timerp project-x-save-timer)
      (cancel-timer project-x-save-timer))))

(provide 'project-x)
;;; project-x.el ends here
