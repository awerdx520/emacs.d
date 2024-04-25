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
;;; Buffer
(defun thomas/search-buffer ()
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

(defun thomas/search-all-buffer (arg &rest _)
  "Conduct a text search on the all Opened buffer."
  (interactive "P")
  (let ((current-prefix-arg
         (or 'all-buffers arg)))
    (call-interactively #'consult-line-multi)))


(provide 'init-funcs)
;;; init-funcs.el ends here
