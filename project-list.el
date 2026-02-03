;;; project-list.el --- Project List               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: tools
;; URL: https://github.com/lizqwerscott/project-list

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use `project-list-projects' open project list buffer.

;;; Code:

(require 'project)

(defface project-list-marked-face '((t :inherit warning))
  "Face used for displaying marked buffers."
  :group 'project-list)

(defface project-list-project-root-face '((t (:inherit completions-annotations)))
  "Face used for project root."
  :group 'project-list)

(defvar project-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'project-list-visit-project)
    (define-key map (kbd "g") 'project-list-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "o") 'project-list-open-project)
    (define-key map (kbd "k") 'project-list-forget-project)
    (define-key map (kbd "A") 'project-list-add-project)
    (define-key map (kbd "n") 'project-list-forward-line)
    (define-key map (kbd "p") 'project-list-backward-line)
    (define-key map (kbd "/ /") 'project-list-clear-filter)
    (define-key map (kbd "/ t") 'project-list-by-used-type)
    (define-key map (kbd "/ n") 'project-list-by-used-name)
    (define-key map (kbd "/ p") 'project-list-by-used-path)
    map)
  "Keymap for `project-list-mode'.")

(defvar project-list-filter-type nil
  "Filter projects by type (regexp).")

(defvar project-list-filter-name nil
  "Filter projects by name (regexp).")

(defvar project-list-filter-path nil
  "Filter projects by path (regexp).")

(define-derived-mode project-list-mode special-mode "Project List"
  "Major mode for listing known projects.
\\{project-list-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;; This makes things less ugly for users with a non-nil
  ;; `show-trailing-whitespace'.
  (setq show-trailing-whitespace nil)
  ;; disable `show-paren-mode' buffer-locally
  (if (bound-and-true-p show-paren-mode)
      (setq-local show-paren-mode nil)))

(defun project-list-refresh ()
  "Refresh the project list."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (project-list-insert-projects)))

(defun project-list-insert-projects ()
  "Insert all known projects into the buffer."
  (let ((projects (project-known-project-roots))
        (first-line-point nil)
        (filtered-count 0)
        (total-count 0))
    ;; Set header-line with filter info
    (let ((header-line ""))
      (when (or project-list-filter-type project-list-filter-name project-list-filter-path)
        (let ((filter-info "Filter: "))
          (when project-list-filter-type
            (setq filter-info (concat filter-info "type=" project-list-filter-type " ")))
          (when project-list-filter-name
            (setq filter-info (concat filter-info "name=" project-list-filter-name " ")))
          (when project-list-filter-path
            (setq filter-info (concat filter-info "path=" project-list-filter-path " ")))
          (setq header-line (string-trim filter-info))))

      (setq header-line
            (unless (string-empty-p header-line)
              (concat header-line (format " (Showing %d/%d projects)" filtered-count total-count))))

      (setq header-line-format header-line))

    (if (null projects)
        (insert "No known projects.\n")
      (insert (propertize (format "%-8s %-30s %s\n" "Type" "Name" "Path")
                          'face 'bold
                          'project-list-title t))
      (insert (propertize (make-string 80 ?-)
                          'face 'shadow
                          'project-list-title t))
      (insert "\n")
      (setq first-line-point (point))
      (dolist (project-root projects)
        (let* ((pr (project--find-in-directory project-root))
               (type (cond
                      ((null pr) "Unknown")
                      ((eq (car pr) 'transient) "Transient")
                      (t (symbol-name (car pr)))))
               (name (file-name-nondirectory (directory-file-name project-root)))
               (type-face (cond
                           ((null pr) 'error)
                           ((eq (car pr) 'transient) 'warning)
                           ((eq (car pr) 'vc) 'success)
                           (t 'font-lock-type-face)))
               (name-face (if (file-directory-p project-root)
                              'font-lock-function-name-face
                            'shadow))
               (include t))
          (setq total-count (1+ total-count))
          ;; Apply filters
          (when project-list-filter-type
            (unless (string-match-p project-list-filter-type type)
              (setq include nil)))
          (when project-list-filter-name
            (unless (string-match-p project-list-filter-name name)
              (setq include nil)))
          (when project-list-filter-path
            (unless (string-match-p project-list-filter-path project-root)
              (setq include nil)))
          (when include
            (setq filtered-count (1+ filtered-count))
            (insert (propertize (format "%-8s " type) 'face type-face))
            (insert (propertize (format "%-30s " name) 'face name-face))
            (insert (propertize project-root
                                'face 'project-list-project-root-face
                                'project-root-path project-root)
                    "\n")))))

    ;; Update header-line with actual counts
    (let ((header-line ""))
      (when (or project-list-filter-type project-list-filter-name project-list-filter-path)
        (let ((filter-info "Filter: "))
          (when project-list-filter-type
            (setq filter-info (concat filter-info "type=" project-list-filter-type " ")))
          (when project-list-filter-name
            (setq filter-info (concat filter-info "name=" project-list-filter-name " ")))
          (when project-list-filter-path
            (setq filter-info (concat filter-info "path=" project-list-filter-path " ")))
          (setq header-line (string-trim filter-info))))

      (setq header-line
            (unless (string-empty-p header-line)
              (concat header-line (format " (Showing %d/%d projects)" filtered-count total-count))))

      (setq header-line-format header-line))
    (goto-char first-line-point)))

(defun project-list-open-project ()
  "Use `dired' open project root dir."
  (interactive)
  (when-let* ((project (project-list-get-project-at-point)))
    (dired-other-window project)))

(defun project-list-visit-project ()
  "Visit the project at point."
  (interactive)
  (when-let* ((project (project-list-get-project-at-point)))
    (project-switch-project project)))

(defun project-list-get-project-at-point ()
  "Get the project root at point."
  (save-excursion
    (when-let* ((end (line-end-position))
                (pos (next-single-property-change (point) 'project-root-path nil end)))
      (get-text-property pos 'project-root-path))))

(defun project-list-forget-project ()
  "Forget the project at point."
  (interactive)
  (when-let* ((project (project-list-get-project-at-point)))
    (project-forget-project project)
    (project-list-refresh)))

(defun project-list-skip-properties (props direction)
  "Skip lines that have any of the text properties in PROPS at point.
Move in DIRECTION (1 for forward, -1 for backward). This is a helper function
used by `project-list-forward-line' and `project-list-backward-line' to skip
over header lines."
  (while (and (not (eobp))
	          (let ((hit nil))
		        (dolist (prop props hit)
		          (when (get-text-property (point) prop)
		            (setq hit t)))))
    (forward-line direction)
    (beginning-of-line)))

(defun project-list-backward-line (&optional arg)
  "Move backwards ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (or arg (setq arg 1))
  (beginning-of-line)
  (while (> arg 0)
    (forward-line -1)
    (when (get-text-property (point) 'project-list-title)
      (goto-char (point-max))
      (beginning-of-line))
    ;; Handle the special case of no project list.
    (when (get-text-property (point) 'project-list-title)
      (forward-line 1)
      (setq arg 1))
    (decf arg)))

(defun project-list-forward-line (&optional arg)
  "Move forward ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (or arg (setq arg 1))
  (beginning-of-line)
  (when (eobp)
    (goto-char (point-min)))
  (when (get-text-property (point) 'project-list-title)
    (when (> arg 0)
      (decf arg))
    (project-list-skip-properties '(project-list-title) 1))
  (if (< arg 0)
      (project-list-backward-line (- arg))
    (while (> arg 0)
      (forward-line 1)
      (when (eobp)
	    (goto-char (point-min)))
      (decf arg)
      (project-list-skip-properties '(project-list-title) 1))))

(defun project-list-clear-filter ()
  "Clear all filters and refresh the project list."
  (interactive)
  (setq project-list-filter-type nil)
  (setq project-list-filter-name nil)
  (setq project-list-filter-path nil)
  (project-list-refresh))

(defun project-list-by-used-type (regexp)
  "Filter projects by project type using a regular expression.

REGEXP is a regular expression string used to match project types."
  (interactive (list (read-string "Filter by type (regexp, empty to clear): " project-list-filter-type)))
  (project-list-clear-filter)
  (setq project-list-filter-type regexp)
  (project-list-refresh))

(defun project-list-by-used-name (regexp)
  "Filter projects by project name using a regular expression.

REGEXP is a regular expression string used to match project names."
  (interactive (list (read-string "Filter by name (regexp, empty to clear): " project-list-filter-name)))
  (project-list-clear-filter)
  (setq project-list-filter-name regexp)
  (project-list-refresh))

(defun project-list-by-used-path (regexp)
  "Filter projects by project path using a regular expression.

REGEXP is a regular expression string used to match project root paths."
  (interactive (list (read-string "Filter by path (regexp, empty to clear): " project-list-filter-path)))
  (project-list-clear-filter)
  (setq project-list-filter-path regexp)
  (project-list-refresh))

;;;###autoload
(defun project-list-projects ()
  "Display a list of known projects."
  (interactive)
  (let ((buffer (get-buffer-create "*Project List*")))
    (with-current-buffer buffer
      (project-list-mode)
      (project-list-refresh))
    (switch-to-buffer buffer)))

(provide 'project-list)
;;; project-list.el ends here
