;;; orgfold.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Copyright (C) 2009-2022

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;
;; Proof of concept implementation of saving folding information in
;; org buffers.  This variant saves folding information in a separate
;; file.
;; 

;;; Code:


(defun make-fold-save-file-name (file)
  "Create backup FILE to store the folt state of an org file."
  (let* ((backup-directory-alist fold-directory-alist)
         (name (make-backup-file-name-1 file)))
    (concat (file-name-directory name) (file-name-nondirectory name) ".fold")))

(setq fold-directory-alist '(("." . "~/.emacs.d/fold")))
(defcustom text-mode-hook nil
  "Directory in which to save org fold state."
  :type 'string
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'orgfole)

(defcustom undo-tree-history-directory-alist nil
  "Alist of filename patterns and fold state directory names.
Each element looks like (REGEXP . DIRECTORY).  Undo history for
files with names matching REGEXP will be saved in DIRECTORY.
DIRECTORY may be relative or absolute.  If it is absolute, so
that all matching files are backed up into the same directory,
the file names in this directory will be the full name of the
file backed up with all directory separators changed to `!' to
prevent clashes.  This will not work correctly if your filesystem
truncates the resulting name.

For the common case of all backups going into one directory, the
alist should contain a single element pairing \".\" with the
appropriate directory name.

If this variable is nil, or it fails to match a filename, the
backup is made in the original file's directory.

On MS-DOS filesystems without long names this variable is always
ignored."
  :group 'orgfold
  :type
  '(repeat
    (cons
     (regexp :tag "Regexp matching filename")
     (directory :tag "Undo history directory name"))))


(defun orgfold-get-fold-info-file-name ()
  "TODO: docs."
  (concat (buffer-file-name) ".fold"))

(defun orgfold-save ()
  "TODO: docs."
  (save-excursion
    (goto-char (point-min))

    (let (foldstates)
      (unless (looking-at outline-regexp)
        (outline-next-visible-heading 1))

      (while (not (eobp))
        (push (when (seq-some
                     (lambda (o) (overlay-get o 'invisible))
                     (overlays-at (line-end-position)))
                t)
              foldstates)
        (outline-next-visible-heading 1))

      (with-temp-file (orgfold-get-fold-info-file-name)
        (prin1 (nreverse foldstates) (current-buffer))))))

(defun orgfold-restore ()
  "TODO: docs."
  (save-excursion
    (goto-char (point-min))
    (let* ((foldfile (orgfold-get-fold-info-file-name))
           (foldstates
            (when (file-readable-p foldfile)
              (with-temp-buffer
                (insert-file-contents foldfile)
                (when (> (buffer-size) 0)
                  (read (current-buffer)))))))

      (when foldstates
        (show-all)
        (goto-char (point-min))

        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))

        (while (and foldstates (not (eobp)))
          (when (pop foldstates)
            (hide-subtree))

          (outline-next-visible-heading 1))

        (message "restored saved folding")))))


(add-hook 'org-mode-hook 'orgfold-activate)

(defun orgfold-activate ()
  "TODO: docs."
  (orgfold-restore)
  (add-hook 'kill-buffer-hook 'orgfold-kill-buffer nil t))

(defun orgfold-kill-buffer ()
  "Don't save folding info for unsaved buffers."
  (unless (buffer-modified-p)
    (orgfold-save)))

(provide 'orgfold)
;;; orgfold.el ends here
