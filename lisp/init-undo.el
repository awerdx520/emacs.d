;;; init-undo.el 撤销系统 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: May 08, 2024
;; Modified: May 08, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-undo
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
;;; Undo
(use-package undo-fu
  :hook (after-init . undo-fu-mode)
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))


(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory (expand-file-name "undo-fu-session" thomas-cache-dir))
  :config
  (setq undo-fu-session-incompatible-files
        '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))

  (defun undo-fu-make-hashed-session-file-name-a (file)
    "HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
        filenames to prevent file paths that are too long, so we force
        `undo-fu-session--make-file-name' to use it instead of its own
        home-grown overly-long-filename generator.
        TODO PR this upstream; should be a universal issue"
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file)
              (undo-fu-session--file-name-ext)))
    (advice-add  'undo-fu-session--make-file-name
                 :override #'undo-fu-make-hashed-session-file-name-a)))

(use-package vundo
  :when (> emacs-major-version 27)
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t))


(provide 'init-undo)
;;; init-undo.el ends here
