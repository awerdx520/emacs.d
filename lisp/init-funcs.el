;;; init-funcs.el 自定以函数 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 26, 2024
;; Modified: April 26, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-funcs
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


;;
;;; search

;;;###autoload
(defun +default/search-emacs (regexp)
  "Conduct a text search in files under `thomas-emacs-dir'."
  (interactive (list (project--read-regexp)))
  (let ((default-directory thomas-emacs-dir))
    (project-find-regexp regexp)))

;;;###autoload
(defun +default/search-other-project (regexp)
  "Conduct a text search in a known project."
  (interactive (list (project--read-regexp)))
  ;; TODO 需要调换一下 `regexp' 和 `roject-root' 的顺序
  (let ((project-root nil)
        (default-directory
         (if-let (projects (project-known-project-roots))
             (completing-read "Search project: " projects nil t)
           (user-error "There are no known projects")))))
  (project-find-regexp regexp))

;;;###autoload
(defun +default/search-buffer ()
  "Conduct a text search on the current buffer.

If a selection is active and multi-line, perform a search restricted to that
region.

If a selection is active and not multi-line, use the selection as the initial
input and search the whole buffer for it."
  (interactive)
  (let (start end multiline-p)
    (save-restriction
      (when (region-active-p)
        (setq start (region-beginning)
              end   (region-end)
              multiline-p (/= (line-number-at-pos start)
                              (line-number-at-pos end)))
        (deactivate-mark)
        (when multiline-p
          (narrow-to-region start end)))

      (if (and start end (not multiline-p))
          (consult-line
           (replace-regexp-in-string
            " " "\\\\ "
            (rxt-quote-pcre
             (buffer-substring-no-properties start end))))
        (call-interactively #'consult-line)))))

;;;###autoload
(defun +default/search-all-buffer (arg &rest _)
  "Conduct a text search on the all Opened buffer."
  (interactive "P")
  (let ((current-prefix-arg
         (or 'all-buffers arg)))
    (call-interactively #'consult-line-multi)))

;;
;;; File

;;;###autoload
(defun thomas-glob (&rest segments)
  "Return file list matching the glob created by joining SEGMENTS.

The returned file paths will be relative to `default-directory', unless SEGMENTS
concatenate into an absolute path.

Returns nil if no matches exist.
Ignores `nil' elements in SEGMENTS.
If the glob ends in a slash, only returns matching directories."
  (declare (side-effect-free t))
  (let* (case-fold-search
         file-name-handler-alist
         (path (apply #'file-name-concat segments)))
    (if (string-suffix-p "/" path)
        (cl-delete-if-not #'file-directory-p (file-expand-wildcards (substring path 0 -1)))
      (file-expand-wildcards path))))

;;; ]t / [t
;;;###autoload
(defun +evil/next-frame (count)
  "Focus next frame."
  (interactive "p")
  (dotimes (_ (abs count))
    (let ((frame (if (> count 0) (next-frame) (previous-frame))))
      (if (eq frame (selected-frame))
          (user-error "No other frame")
        (select-frame-set-input-focus frame)))))

;;;###autoload
(defun +evil/previous-frame (count)
  "Focus previous frame."
  (interactive "p")
  (+evil/next-frame (- count)))

;;; ]f / [f
(defun +evil--next-file (n)
  (unless buffer-file-name
    (user-error "Must be called from a file-visiting buffer"))
  (let* ((directory (file-name-directory buffer-file-name))
         (filename (file-name-nondirectory buffer-file-name))
         (files (cl-remove-if-not #'file-regular-p (thomas-glob (file-name-directory buffer-file-name) "[!.]*")))
         (index (cl-position filename files :test #'file-equal-p)))
    (when (null index)
      (user-error "Couldn't find this file in current directory"))
    (let ((index (+ index n)))
      (cond ((>= index (length files))
             (user-error "No files after this one"))
            ((< index 0)
             (user-error "No files before this one"))
            ((expand-file-name (nth index files) directory))))))

;;;###autoload
(defun +evil/next-file (count)
  "Open file following this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+evil--next-file count)))

;;;###autoload
(defun +evil/previous-file (count)
  "Open file preceding this one, alphabetically, in the same directory."
  (interactive "p")
  (find-file (+evil--next-file (- count))))

;;;###autoload
(defun +default/find-file-in-emacs (&optional include-all)
  "Search for a file in `thomas-emacs-dir'."
  (interactive "P")
  (let ((default-directory thomas-emacs-dir))
    (project-find-file include-all)))

(provide 'init-funcs)
;;; init-funcs.el ends here
