;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(if (string= (getenv "LOCATION") "work-dell-64")
    (setq user-full-name "tangxin1"
          user-mail-address "***REMOVED***"
          user-gpg-key "92A648F04F129A7A")

  (setq user-full-name "tangxin"
        user-mail-address "1164074502@qq.com"
        user-gpg-key "92A648F04F129A7A"))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-journal-dir (concat user-emacs-directory "gtd/")
;;       org-roam-directory (concat user-emacs-directory "roam/")
;;       org-archive-location (concat org-directory "archive/%s_archive::"))

;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Load customize config file
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory  dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))
(setq custom-file (locate-user-emacs-file "custom.el"))

;;
(require 'init-const)
(require 'init-macro)
(require 'init-package)
(require 'init-base)
(require 'init-wsl)

;; ui
(require 'init-ui)
(require 'init-dashboard)
(require 'init-modeline)
;;
(require 'init-editor)
(require 'init-buffer)
(require 'init-minibuffer)
(require 'init-window)
(require 'init-dired)
(require 'init-help)
(require 'init-motion)
(require 'init-remote)
;;
(require 'init-chinese)
(require 'init-spell)
;;
(require 'init-project)
(require 'init-git)
(require 'init-snippets)
(require 'init-syntax)
(require 'init-format)
(require 'init-develop)
;; Language
;;(require 'init-cpp)
(require 'init-rust)
;;(require 'init-bazel)
(require 'init-python)
;;(require 'init-elisp)
(require 'init-sh)

;;
(require 'init-shell)
(require 'init-text)
(require 'init-bookmark)
(require 'init-browser)

(require 'init-keybinding)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
