;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;
;; --debug-init implies `debug-on-error'.
(setq debug-on-error init-file-debug)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Load customize config file
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory  dir))
  (add-to-list 'load-path (file-name-as-directory (expand-file-name "lang" dir))))

;; 依赖
(require 'init-const)
(require 'init-custom)
(require 'init-funcs)
(require 'init-macro)

;; packages
(require 'init-package)

;; preferences
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
(require 'init-treesit)
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
;;;  init.el ends here
