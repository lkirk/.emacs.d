;;; orgfold.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; This package restores folding state from org mode files.  The one caveat is
;; that if these files are changed at any point, all of the folding restore is
;; out of sync.  There is no validation.  So, to combat this, I'll be version
;; controlling my fold files so that the fold state is preserved across all of
;; the machines I use Emacs on.  This was lifted from the internet... not sure
;; where?  I'll find out at some point.  I modified it to use the undo-tree temp
;; file naming scheme.

;;
;; Proof of concept implementation of saving folding information in
;; org buffers.  This variant saves folding information in a separate
;; file.
;; 

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

;;; Code:


(defcustom fold-directory-alist nil
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

(defun make-fold-save-file-name (file)
  "Create backup FILE to store the folt state of an org file."
  ;; If we've set the fold directory alist, use a relative path so that the
  ;; external dir where we store storage paths can be version controlled
  ;; without an explicit dependency on the absolute path.
  ;; TODO: make this a config option?
  (if fold-directory-alist
      ;; use the same machinery as files.el for examining the fold dir alist
      (let ((alist fold-directory-alist)
            elt
            backup-directory
            abs-backup-directory)
        (while alist
          (setq elt (pop alist))
          (if (string-match (car elt) file)
              (setq
               backup-directory (cdr elt)
               alist nil)))
        (when backup-directory
          (let* ((backup-directory-alist fold-directory-alist)
                 (name
                  (make-backup-file-name-1
                   (file-relative-name file backup-directory))))
            (concat
             (file-name-directory name)
             "."
             (file-name-nondirectory name)
             ".fold"))))
    (let* ((backup-directory-alist fold-directory-alist)
           (name (make-backup-file-name-1 file)))
      (concat
       (file-name-directory name) "." (file-name-nondirectory name) ".fold"))))

(defun orgfold-get-fold-info-file-name ()
  "Get the filename of fold file, which only exists if the buffer has a filename."
  (when buffer-file-name
    (make-fold-save-file-name buffer-file-name)))

(defun orgfold-save ()
  "Save the fold state to a fold file."
  (let ((foldfile (orgfold-get-fold-info-file-name)))
    (when foldfile
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

          (with-temp-file foldfile
            (prin1 (nreverse foldstates) (current-buffer))))))))

(defun orgfold-restore ()
  "Restore fold states from the fold file if the fold file exists."
  (save-excursion
    (goto-char (point-min))
    (let* ((foldfile (orgfold-get-fold-info-file-name))
           (foldstates
            (when foldfile
              (if (file-readable-p foldfile)
                  (with-temp-buffer
                    (insert-file-contents foldfile)
                    (when (> (buffer-size) 0)
                      (read (current-buffer))))))))
      ;; (message (format "could not read fold file: %s" foldfile))))))

      (when foldstates
        (show-all)
        (goto-char (point-min))

        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))

        (while (and foldstates (not (eobp)))
          (when (pop foldstates)
            (hide-subtree))

          (outline-next-visible-heading 1))

        (message "restored folding")))))


(defun orgfold-activate ()
  "Mode hook for orgfold saving."
  (orgfold-restore)
  (add-hook 'kill-buffer-hook 'orgfold-kill-buffer nil t)
  (add-hook 'after-save-hook 'orgfold-save nil t))

(defun orgfold-kill-buffer ()
  "Don't save folding info for unsaved buffers."
  (unless (buffer-modified-p)
    (orgfold-save)))

(provide 'orgfold)
;;; orgfold.el ends here
