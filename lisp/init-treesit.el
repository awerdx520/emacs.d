;;; init-treesit.el Emacs Tree-sitter 配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 24, 2024
;; Modified: April 24, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-treesit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(use-package treesit-auto
  :config
  (setq treesit-font-lock-level 4
        treesit-auto-install 'prompt)

  ;; 自动注册 `treesit-auto-langs' 中所有的拓展到 auto-mode-alist 中
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

(provide 'init-treesit)
;;; init-treesit.el ends here
