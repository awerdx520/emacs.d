;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;
(when (version< emacs-version "29.1")
  (error "Your Emacs is too old -- this config requires 29.1 or higher"))

;;
;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent flashing of unstyled modeline at startup
(setq-default mode-line-format nil)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Optimize loading performance
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-file-name-handler-alist)))))))

;; 将 core 目录添加到 `load-path' 中
(add-to-list 'load-path (file-name-as-directory (locate-user-emacs-file "core")))

;;; 将配置文件添加到 `load-path' 中
(add-to-list 'load-path (file-name-as-directory (locate-user-emacs-file "lisp")))

(require 'core)
(require 'init-evil)
(require 'init-ui)
(require 'init-completion)
(require 'init-editor)

;; msic
(require 'init-project)
(require 'init-chinese)
(require 'init-window)
(require 'init-treesit)

;; tools
(require 'init-snippets)
(require 'init-dired)
(require 'init-spell)
(require 'init-undo)
(require 'init-direnv)
(require 'init-git)
(require 'init-bookmark)
(require 'init-browser)
(require 'init-format)

;; Language
(require 'init-shell)
(require 'init-text)
;;(require 'init-cpp)
(require 'init-rust)
;;(require 'init-bazel)
(require 'init-python)
(require 'init-go)
;;(require 'init-elisp)
(require 'init-sh)
(require 'init-lsp)

;;
(require 'init-keybinding)

;;
(when (file-exists-p custom-file)
  (load custom-file))
;;;  init.el ends here
