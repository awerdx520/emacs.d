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


(use-package project
  :straight (:type built-in)
  :init
  (setq project-list-file (concat thomas-data-dir "projects"))
  :general
  (thomas-leader-project
   "!" 'project-shell-command ;; 在项目根目录下执行命令
   "&" 'project-async-shell-command
   "a" 'project-remember-projects-under ;; 添加新项目
   "b" 'consult-project-buffer ;; 切换到项目中已经打开的 Buffer
   "c" 'project-compile ;; 执行编译命令
   "t" 'magit-todos-list ;; 列出项目 TODO
   "d" 'project-find-dir ;; 在 dired 中打开选定的文件夹
   "D" 'project-dired ;; 在项目根目录打开 dired
   "f" 'project-find-file ;; 在项目中搜索文件
   "F" 'project-or-external-find-file
   "p" 'project-switch-project ;; 切换项目
   "v" 'project-vc-dir
   "x" 'project-execute-extended-command
   "r" 'project-query-replace-regexp
   "k" 'project-kill-buffers)
  :config
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun thomas/project-try-flag (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '((".project")
                        ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let ((root (locate-dominating-file dir f)))
              (throw 'ret (cons 'local root))))))))

  (setq project-find-functions '(project-try-vc thomas/project-try-flag)))

(provide 'init-project)
;;; init-project.el ends here
