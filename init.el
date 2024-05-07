;;; init.el --- The main entry for emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;

(when (version< emacs-version "29.1")
  (error "Your Emacs is too old -- this config requires 28.1 or higher"))

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil
      site-run-file nil)

;; Optimize loading performance
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-file-name-handler-alist)))))))

;; Optimize emacs garbage collect.
(setq gc-cons-threshold most-positive-fixnum)

;; Hook run after loading init files
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 800000)))

;; Suppress flashing at startup
(setq-default inhibit-message t
              inhibit-redisplay t)

(defun +window-inhibit-setting-fn ()
  "Inhibit some message and redisplay."
  (setq-default inhibit-message nil
                inhibit-redisplay nil)
  (redisplay))
(add-hook 'window-setup-hook #'+window-inhibit-setting-fn)

;; 加载 thomas-emacs 核心配置
(load (expand-file-name "core/core" (file-truename user-emacs-directory)) nil 'nomessage)
;; 初始化 thomas-emacs
(thomas-initialize-core)

;; Load custom
(setq custom-file (expand-file-name "custom.el" thomas-emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; Be quiet at startup; don't load or display anything unnecessary
(advice-add #'display-startup-echo-area-message :override #'ignore)

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
;;(require 'init-elisp)
(require 'init-sh)
(require 'init-lsp)

(require 'init-keybinding)
;;;  init.el ends here
