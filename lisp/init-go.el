;;; init-go.el Golang -*- lexical-binding: t; -*-
(use-package go-mode
  :mode (("\\.go\\'" . go-ts-mode))
  :custom (go-ts-mode-indent-offset 4))

;; 回答有关 Go 源代码问题的工具, 集到 go-mode 中
(use-package go-guru
  :after go-mode)

;; https://github.com/brantou/emacs-go-tag/tree/33f2059551d5298ca228d90f525b99d1a8d70364
;; 基于 gomodifytags 编辑 golang 结构字段的字段标签。
(use-package go-tag
  :after go-mode)

;; go-gen-test 是一个用于从 emacs 生成 go 代码测试的软件包。它是对 gotests 的简单封装。
(use-package go-gen-test)

;; 在 eldoc 中显示变量，函数类型信息以及函数当前参数位置
;; TODO 没有起作用
(use-package go-eldoc
  :hook (go-ts-mode . go-eldoc-setup))

(use-package gotest)

;;
;; (use-package gorepl-mode
;;   :commands gorepl-run-load-current-file)

(provide 'init-go)
