;;; core.el 核心配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: 五月 07, 2024
;; Modified: 五月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/core
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(when (version<= emacs-version "28.2")
  (user-error "不满足最小 Emacs 版本: %s" emacs-version))

;; 第一次输入事件定义
(defvar thomas-first-input-hook nil
  "Emacs 第一次输入钩子.")

;; 第一次运行命令时执行 `thomas-first-input-hook' 钩子然后清空钩子内容
(add-hook 'pre-command-hook #'(lambda ()
                                (when thomas-first-input-hook
                                  (run-hooks 'thomas-first-input-hook))))
(defvar thomas-first-file-hook nil
  "Emacs 第一次启动文件钩子.")

(defun run-thomas-first-file-hooks ()
  "执行 `doom-first-file-hook' 中钩子函数."
  (when thomas-first-file-hook
    (run-hooks 'thomas-first-file-hook)
    (setq thomas-first-file-hook nil)))

(add-hook 'find-file-hook #'run-thomas-first-file-hooks)
(add-hook 'dired-initial-position-hook #'run-thomas-first-file-hooks)

;; 主题加载事件定义
(defvar thomas-load-theme-hook nil
  "Emacs 加载主题后执行配置钩子.")

(defun run-thomas-load-theme-hooks(&rest _)
  "执行 `thomas-load-theme-hook' 钩子函数."
  (run-hooks 'thomas-load-theme-hook))

;; 为 load-theme 添加后置通知
(advice-add #'load-theme :after #'run-thomas-load-theme-hooks)
(add-hook 'thomas-load-theme-hook #'window-divider-mode)

;;; 将配置文件添加到 `load-path' 中
;; 将 core 目录添加到 `load-path' 中
(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path (file-name-as-directory
                         (locate-user-emacs-file "lisp")))


;;
(defun thomas-initialize-core()
  "为 Emacs 加载核心配置文件."
  (require 'core-const) ; 常量配置
  (require 'core-macro) ; 自定义宏
  (require 'core-wsl)
  (require 'core-basic) ; 基础配置
  (require 'core-package))

(provide 'core)
;;; core.el ends here
