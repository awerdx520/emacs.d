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

(require 'core-const) ; 常量配置
(require 'core-macro) ; 自定义宏

;; 第一次输入事件定义
(defvar thomas-first-input-hook nil "Emacs 第一次输入钩子.")
;; 第一次运行命令时执行 `thomas-first-input-hook' 钩子然后清空钩子内容
(add-hook 'pre-command-hook #'(lambda () (when thomas-first-input-hook)
                                (run-hooks 'thomas-first-input-hook)))


;; 第一次访问文件时需要执行的钩子
(defvar thomas-first-file-hook nil "Emacs 第一次启动文件钩子.")
(defun run-thomas-first-file-hooks ()
  "执行 `thomas-first-file-hook' 中钩子函数.

执行完钩子后需要清空钩子中的配置。"
  (when thomas-first-file-hook (run-hooks 'thomas-first-file-hook)
        (setq thomas-first-file-hook nil)))

(add-hook 'find-file-hook #'run-thomas-first-file-hooks)
(add-hook 'dired-initial-position-hook #'run-thomas-first-file-hooks)

;; 主题加载事件定义
(defvar thomas-load-theme-hook nil "Emacs 加载主题后执行配置钩子.")
(defun run-thomas-load-theme-hooks(&rest _)
  "执行 `thomas-load-theme-hook' 钩子函数."
  (run-hooks 'thomas-load-theme-hook))

;; 为 load-theme 添加后置通知
(advice-add #'load-theme :after #'run-thomas-load-theme-hooks)
(add-hook 'thomas-load-theme-hook #'window-divider-mode)

;; 设置 Custom 文件路径
(setq custom-file
      (expand-file-name "custom.el" thomas-emacs-dir))

;;
(require 'core-autoload)
(require 'core-basic)
(require 'core-package)
(require 'core-wsl)

;; 添加字体
(thomas-setup-font)
(add-hook 'window-setup-hook #'thomas-setup-font)
(add-hook 'server-after-make-frame-hook #'thomas-setup-font)

(provide 'core)
;;; core.el ends here
