;;; init-snippents.el Emacs 代码片段支持 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-snippents
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
(use-package yasnippet
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :hook (prog-mode . yas-minor-mode)
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (setq yas-verbosity 2)
  :config
  ;; HACK In case `+snippets-dir' and `doom-snippets-dir' are the same, or
  ;;      duplicates exist in `yas-snippet-dirs'.
  (advice-add #'yas-snippet-dirs :filter-return #'delete-dups)
  ;; 加载所有 snippets 菜单
  (yas-reload-all))

;; 添加常用 snippets 片段
(use-package yasnippet-snippets
  :after yasnippt)

;; 创建代码片段
(use-package auto-yasnippet
  :after yasnippet)

;; yassnippet
(use-package consult-yasnippet
  :after (yasnippet consult)
  :defer t)

(provide 'init-snippets)
;;; init-snippets.el ends here
