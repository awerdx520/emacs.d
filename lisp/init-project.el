;;; init-project.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-project
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
(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  ;; Ensure babel's execution environment matches the host buffer's.
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment))

(defvar +project-ignore-project-list '("~" "/home/thomas" "/")
  "忽略指定目录作为项目目录.")

(use-package project
  :straight (:type built-in)
  :init
  (setq project-list-file (concat thomas-data-dir "projects"))
  :general
  (thomas-leader
    "p!" 'project-shell-command ;; 在项目根目录下执行命令
    "p&" 'project-async-shell-command
    "pa" 'project-remember-projects-under ;; 添加新项目
    "pc" 'project-compile ;; 执行编译命令
    "pd" 'project-find-dir ;; 在 dired 中打开选定的文件夹
    "pD" 'project-dired ;; 在项目根目录打开 dired
    "pf" 'project-find-file ;; 在项目中搜索文件
    "pF" 'project-or-external-find-file
    "pp" 'project-switch-project ;; 切换项目
    "pv" 'project-vc-dir
    "px" 'project-execute-extended-command
    "pr" 'project-query-replace-regexp
    "pk" 'project-kill-buffers)
  :config
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun thomas/project-try-flag (dir)
    "Determine if DIR is a non-Git project."
    (unless (member (expand-file-name "" dir)
                    +project-ignore-project-list)
      (catch 'ret
        (let ((pr-flags '((".project")
                          ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                          ("Makefile" "README.org" "README.md"))))
          (dolist (current-level pr-flags)
            (dolist (f current-level)
              (when-let ((root (locate-dominating-file dir f)))
                (throw 'ret (cons 'local root)))))))))

  (setq project-find-functions '(project-try-vc thomas/project-try-flag)))

(provide 'init-project)
;;; init-project.el ends here
