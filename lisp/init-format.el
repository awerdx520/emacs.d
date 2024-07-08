;;; init-format.el Emacs 格式化配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-format
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defcustom +format-on-save-disabled-modes
  '(sql-mode           ; sqlformat is currently broken
    tex-mode           ; latexindent is broken
    latex-mode
    org-msg-edit-mode) ; doesn't need a formatter
  "A list of major modes in which to not reformat the buffer upon saving.

If it is t, it is disabled in all modes, the same as if the +onsave flag wasn't
  used at all.
If nil, formatting is enabled in all modes."
  :type '(list symbol))

(defvaralias '+format-with 'apheleia-formatter)
(defvaralias '+format-inhibit 'apheleia-inhibit)

(use-package apheleia
  :init
  (add-hook 'thomas-first-file-hook #'apheleia-global-mode)
  ;; apheleia autoloads `apheleia-inhibit-functions' so it will be immediately
  ;; available to mutate early.
  (defun +format-maybe-inhibit-h ()
    "Check if formatting should be disabled for current buffer.
This is controlled by `+format-on-save-disabled-modes'."
    (or (eq major-mode 'fundamental-mode)
        (string-blank-p (buffer-name))
        (eq +format-on-save-disabled-modes t)
        (not (null (memq major-mode +format-on-save-disabled-modes)))))

  (add-hook 'apheleia-inhibit-functions #'+format-maybe-inhibit-h)
  :config
  (setq +format-with-lsp nil)
  ;; 设置 Formatter
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

  (setq apheleia-remote-algorithm 'local)
  (with-eval-after-load 'lsp-bridge
    (add-hook 'apheleia-post-format-hook #'lsp-bridge-update-tramp-docker-file-mod-time)))

(provide 'init-format)
;;; init-format.el ends here
