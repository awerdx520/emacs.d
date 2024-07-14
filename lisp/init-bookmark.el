;;; init-bookmark.el Emacs 书签 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-bookmark
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Bookmark
(use-package bookmark
  :custom
  (bookmark-default-file (concat thomas-data-dir "bookmarks"))
  :config
  (with-no-warnings
    ;; Display icons for bookmarks
    (defun +bookmark-bmenu--revert ()
      "Re-populate `tabulated-list-entries'."
      (let (entries)
        (dolist (full-record (bookmark-maybe-sort-alist))
          (let* ((name       (bookmark-name-from-full-record full-record))
                 (annotation (bookmark-get-annotation full-record))
                 (location   (bookmark-location full-record))
                 (file       (file-name-nondirectory location))
                 (type       (let ((fmt "%-8.8s"))
                               (cond ((null location)
                                      (propertize (format fmt "NOFILE") 'face 'warning))
                                     ((file-remote-p location)
                                      (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                     ((not (file-exists-p location))
                                      (propertize (format fmt "NOTFOUND") 'face 'error))
                                     ((file-directory-p location)
                                      (propertize (format fmt "DIRED") 'face 'warning))
                                     (t (propertize (format fmt "FILE") 'face 'success)))))
                 (icon       (if (icons-displayable-p)
                                 (cond
                                  ((file-remote-p location)
                                   (nerd-icons-codicon "nf-cod-radio_tower"))
                                  ((file-directory-p location)
                                   (nerd-icons-icon-for-dir location))
                                  ((not (string-empty-p file))
                                   (nerd-icons-icon-for-file file)))
                               "")))
            (push (list
                   full-record
                   `[,(if (and annotation (not (string-equal annotation "")))
                          "*" "")
                     ,icon
                     ,(if (display-mouse-p)
                          (propertize name
                                      'font-lock-face 'bookmark-menu-bookmark
                                      'mouse-face 'highlight
                                      'follow-link t
                                      'help-echo "mouse-2: go to this bookmark in other window")
                        name)
                     ,type
                     ,@(if bookmark-bmenu-toggle-filenames
                           (list (propertize location 'face 'completions-annotations)))])
                  entries)))
        (tabulated-list-init-header)
        (setq tabulated-list-entries entries))
      (tabulated-list-print t))
    (advice-add #'bookmark-bmenu--revert :override #'+bookmark-bmenu--revert)

    (defun +bookmark-bmenu-list ()
      "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
      (interactive)
      (bookmark-maybe-load-default-file)
      (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
        (if (called-interactively-p 'interactive)
            (pop-to-buffer buf)
          (set-buffer buf)))
      (bookmark-bmenu-mode)
      (bookmark-bmenu--revert))
    (advice-add #'bookmark-bmenu-list :override #'+bookmark-bmenu-list)

    (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (setq tabulated-list-format
            `[("" 1) ;; Space to add "*" for bookmark with annotation
              ("" ,(if (icons-displayable-p) 2 0)) ;; Icons
              ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
              ("Type" 9)
              ,@(if bookmark-bmenu-toggle-filenames
                    '(("File" 0 bookmark-bmenu--file-predicate)))])
      (setq tabulated-list-padding bookmark-bmenu-marks-width)
      (setq tabulated-list-sort-key '("Bookmark" . nil))
      (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
      (setq revert-buffer-function #'bookmark-bmenu--revert)
      (tabulated-list-init-header))))

(provide 'init-bookmark)
;;; init-bookmark.el ends here
