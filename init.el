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

;;; 将配置文件添加到 `load-path' 中
(add-to-list 'load-path (file-name-as-directory (locate-user-emacs-file "lisp")))

;; 设置 Custom 文件路径
(setq custom-file (expand-file-name "custom.el"  user-emacs-directory))

;; Packages
(require 'init-package)

;; 添加性能测试
(use-package benchmark-init
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config (require 'benchmark-init-modes))

;; Core
(require 'init-base)
(require 'init-completion)
(require 'init-evil)
(require 'init-editor)
(require 'init-ui)
(require 'init-window)

;; tools
(require 'init-help)
(require 'init-dired)
(require 'init-trans)
(require 'init-browse)
(require 'init-format)
(require 'init-snippets)
(require 'init-shell)
(require 'init-vcs)
(require 'init-project)
(require 'init-lsp)
(require 'init-org)
;;(require 'init-llm)

;; Programming
;;(require 'init-text)
;;(require 'init-rust)
;;(require 'init-scala)
(require 'init-python)
(require 'init-go)
;;(require 'init-sh)

;; 加载 custom.el 文件
(when (file-exists-p custom-file)
  (load custom-file))
;;;  init.el ends here
