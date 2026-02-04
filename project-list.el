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

(defface project-list-marked-face '((t :inherit ibuffer-marked))
  "Face used for displaying marked buffers."
  :group 'project-list)

(defface project-list-marked-delete-face '((t :inherit ibuffer-deletion))
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
    (define-key map (kbd "/ r") 'project-list-by-used-root)
    (define-key map (kbd "/ p") 'project-list-pop-filter)

    (define-key map (kbd "m") 'project-list-mark-forward)
    (define-key map (kbd "u") 'project-list-unmark-forward)

    (define-key map (kbd "d") 'project-list-mark-for-delete)
    (define-key map (kbd "x") 'project-list-do-kill-on-deletion-marks)

    map)
  "Keymap for `project-list-mode'.")

(defvar-local project-list-filter-list nil
  "Filter projects list.")

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

(defun project-list-project-info (project-root)
  "Get information about a project at PROJECT-ROOT.

PROJECT-ROOT is the root directory of the project.
Return a list (PROJECT TYPE NAME ROOT) where:
- PROJECT is the project object from `project--find-in-directory'
- TYPE is a string describing the project type
- NAME is the base name of the project directory
- ROOT is the project root path"
  (let* ((pr (if (file-remote-p project-root) '(remote)
               (project--find-in-directory project-root)))
         (type (cond
                ((null pr) "Unknown")
                ((eq (car pr) 'remote) "Remote")
                ((eq (car pr) 'transient) "Transient")
                (t (symbol-name (car pr)))))
         (name (file-name-nondirectory (directory-file-name project-root))))
    (list pr :type type :name name :project-root project-root)))

(defun project-list--format-with-truncation (text width face &optional ellipsis)
  "Format TEXT with FACE, truncating to WIDTH if needed.
If TEXT is longer than WIDTH, truncate it and add ELLIPSIS (default \"...\").
Returns a propertized string with face applied."
  (let* ((ellipsis (or ellipsis "..."))
         (text-len (length text))
         (ellipsis-len (length ellipsis)))
    (if (<= text-len width)
        (concat (propertize text 'face face)
                (make-string (- width text-len) ?\s)
                " ")
      (let* ((truncated-len (- width ellipsis-len))
             (truncated-text (if (> truncated-len 0)
                                 (substring text 0 truncated-len)
                               "")))
        (concat (propertize (concat truncated-text ellipsis) 'face face)
                " ")))))

(defun project-list-insert-buffer-line (mark project)
  "Insert a line describing BUFFER and MARK for PROJECT."
  (let ((beg (point)))
    (pcase-let* ((`(,pr . (:type ,type :name ,name :project-root ,project-root)) project)
                 (type-face (cond
                             ((null pr) 'error)
                             ((eq (car pr) 'transient) 'warning)
                             ((eq (car pr) 'vc) 'success)
                             (t 'font-lock-type-face)))
                 (name-face (cond ((eq mark ?\>) 'project-list-marked-face)
                                  ((eq mark ?\d) 'project-list-marked-delete-face)
                                  (t 'font-lock-function-name-face))))
      (insert (project-list--format-with-truncation type 8 type-face))
      (insert (project-list--format-with-truncation name 30 name-face))
      (insert (propertize project-root
                          'face 'project-list-project-root-face
                          'project-root-path project-root))
      (put-text-property beg (point) 'project-list-properties mark)
      (insert "\n"))))

(defun project-list-filter-includep (project)
  "Check if PROJECT should be included based on current filters.

PROJECT is a list (PROJECT TYPE NAME ROOT) as returned by
`project-list-project-info'. Return non-nil if the project matches all active
filters."
  (pcase-let* ((`(,_ . ,lines) project)
               (include t))
    ;; Apply filters
    (dolist (filter project-list-filter-list)
      (let ((data (plist-get lines (car filter))))
        (unless (string-match-p (cdr filter) data)
          (setq include nil))))
    include))

(defun project-list-set-heade-line (filtered-count total-count)
  "Set the header line for the project list buffer.

  FILTERED-COUNT is the number of projects matching current filters.
  TOTAL-COUNT is the total number of known projects.
  Display filter information and project counts in the header line.
  Note: Function name contains a typo (heade instead of header)."
  (let ((header-line ""))
    (when project-list-filter-list
      (let ((filter-info "Filter: "))
        (dolist (filter project-list-filter-list)
          (setq filter-info
                (concat filter-info
                        (format "[%s: %s] "
                                (intern (substring (symbol-name (car filter)) 1))
                                (cdr filter)))))
        (setq header-line (string-trim filter-info))))

    (setq header-line
          (unless (string-empty-p header-line)
            (concat header-line (format " (Showing %d/%d projects)" filtered-count total-count))))

    (setq header-line-format header-line)))

(defun project-list-insert-projects ()
  "Insert all known projects into the buffer."
  (let ((projects (project-known-project-roots))
        (first-line-point nil)
        (filtered-count 0)
        (total-count 0))
    ;; Set header-line with filter info
    (project-list-set-heade-line filtered-count total-count)
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
        (let* ((project (project-list-project-info project-root)))
          (setq total-count (1+ total-count))
          (when (project-list-filter-includep project)
            (setq filtered-count (1+ filtered-count))
            (project-list-insert-buffer-line ?\s project)))))

    ;; Update header-line with actual counts
    (project-list-set-heade-line filtered-count total-count)

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

(defun project-list-pop-filter ()
  "Pop filter."
  (interactive)
  (when (null project-list-filter-list)
    (error "No filters in effect"))
  (pop project-list-filter-list)
  (project-list-refresh))

(defun project-list-clear-filter ()
  "Clear all filters and refresh the project list."
  (interactive)
  (setq project-list-filter-list nil)
  (project-list-refresh))

(defun project-list-by-used-type (regexp)
  "Filter projects by project type using a regular expression.

REGEXP is a regular expression string used to match project types."
  (interactive (list (read-string "Filter by type (regexp): ")))
  (push (cons :type regexp) project-list-filter-list)
  (project-list-refresh))

(defun project-list-by-used-name (regexp)
  "Filter projects by project name using a regular expression.

REGEXP is a regular expression string used to match project names."
  (interactive (list (read-string "Filter by name (regexp): ")))
  (push (cons :name regexp) project-list-filter-list)
  (project-list-refresh))

(defun project-list-by-used-root (regexp)
  "Filter projects by project path using a regular expression.

REGEXP is a regular expression string used to match project root paths."
  (interactive (list (read-string "Filter by project root (regexp): ")))
  (push (cons :project-root regexp) project-list-filter-list)
  (project-list-refresh))

;; mark
(defsubst project-list-current-mark ()
  "Get current project mark."
  (get-text-property (line-beginning-position)
			         'project-list-properties))

(defun project-list-redisplay-current ()
  "Redisplay current project."
  (when (eobp)
    (forward-line -1))
  (beginning-of-line)
  (when-let* ((project-root (project-list-get-project-at-point))
              (project (project-list-project-info project-root))
              (mark (project-list-current-mark)))
    (save-excursion
	  (delete-region (point) (1+ (line-end-position)))
      (project-list-insert-buffer-line mark project))))

(defun project-list-set-mark-1 (mark)
  "Set MARK in current project."
  (let ((beg (line-beginning-position))
	    (end (line-end-position)))
    (put-text-property beg end 'project-list-properties mark)))

(defun project-list-set-mark (mark)
  "Set MARK in current project."
  (let ((inhibit-read-only t))
    (project-list-set-mark-1 mark)
    (project-list-redisplay-current)
    (beginning-of-line)))

(defun project-list-mark-interactive (arg mark &optional movement)
  "Mark or unmark ARG projects starting from current line.

  ARG is the number of projects to mark (positive) or unmark (negative). MARK is
the character to set (?\> for marked, ?\s for unmarked). MOVEMENT is a
deprecated argument for backward compatibility."
  (or arg (setq arg 1))
  ;; deprecated movement argument
  (when (and movement (< movement 0))
    (setq arg (- arg)))
  (project-list-forward-line 0)

  (project-list-forward-line 0)
  (while (> arg 0)
    (project-list-set-mark mark)
    (project-list-forward-line 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (project-list-forward-line -1)
    (project-list-set-mark mark)
    (setq arg (1+ arg))))

(defun project-list--mark-region-or-n-with-char (start end arg mark-char)
  "Mark projects in region or ARG projects with MARK-CHAR.

  If region is active (between START and END), mark all projects in the region.
Otherwise, mark ARG projects from current position. MARK-CHAR is the character
to set as mark."
  (if (use-region-p)
      (let ((cur (point)) (line-count (count-lines start end)))
        (goto-char start)
        (project-list-mark-interactive line-count mark-char)
        (goto-char cur))
    (project-list-mark-interactive arg mark-char)))

(defsubst project-list--get-region-and-prefix ()
  "Get region boundaries and prefix argument for marking commands.

  Return a list (START END ARG) where: - START and END are region boundaries if
region is active, otherwise nil - ARG is the numeric value of the prefix
argument"
  (let ((arg (prefix-numeric-value current-prefix-arg)))
    (if (use-region-p) (list (region-beginning) (region-end) arg)
      (list nil nil arg))))

(defun project-list-mark-forward (start end arg)
  "Mark the project in the region, or ARG projects.
START and END is selected regoin."
  (interactive (project-list--get-region-and-prefix))
  (project-list--mark-region-or-n-with-char start end arg ?\>))

(defun project-list-unmark-forward (start end arg)
  "Unmark the projects in the region, or ARG buffers.
START and END is selected regoin."
  (interactive (project-list--get-region-and-prefix))
  (project-list--mark-region-or-n-with-char start end arg ?\s))

(defun project-list-mark-for-delete (start end arg)
  "Mark the projects in the region to delete, or ARG buffers.
START and END is selected regoin."
  (interactive (project-list--get-region-and-prefix))
  (project-list--mark-region-or-n-with-char start end arg ?\d))

(defun project-list-do-kill-on-deletion-marks ()
  "Forget projects marked for deletion.

If no projects are marked, mark the current project for deletion and then forget
it. Ask for confirmation before forgetting any projects. This command operates
on the projects displayed in the current `project-list-mode' buffer."
  (interactive)
  (let ((deletion-project))
    (save-excursion
      (goto-char (point-min))
      (project-list-skip-properties '(project-list-title) 1)
      (while (not (eobp))
        (let ((mark (project-list-current-mark)))
          (when (eq mark ?\d)
            (push (project-list-get-project-at-point) deletion-project)))
        (forward-line)))
    (unless deletion-project
      (project-list-mark-for-delete nil nil 1)
      (project-list-backward-line 1)
      (push (project-list-get-project-at-point) deletion-project))
    (when (and deletion-project
               (yes-or-no-p "Is really forget this projects?"))
      (dolist (project deletion-project)
        (project-forget-project project)
        (project-list-refresh)))))


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
