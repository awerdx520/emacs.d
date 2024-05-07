;;; search.el --- autoload functions used in buffer. -*- lexical-binding: t -*-;
;;
;; Copyright (C) 2017-2022 kevin.scnu@gmail.com
;;
;; Author: Kevin Leung <kevin.scnu@gmail.com>
;; URL: https://github.com/lkzz/emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;; Code:
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

