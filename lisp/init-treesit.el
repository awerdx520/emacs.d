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

;; https://github.com/nvim-treesitter/nvim-treesitter-textobjects#built-in-textobjects
(use-package evil-textobj-tree-sitter
  :after (evil treesit)
  :general
  ;; Mapping textobjects
  (:keymaps 'evil-inner-text-objects-map
            "a" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.inner"))
            ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
            "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
            "F" (evil-textobj-tree-sitter-get-textobj "call.inner")
            "C" (evil-textobj-tree-sitter-get-textobj "class.inner")
            "v" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
            "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))

  (:keymaps 'evil-outer-text-objects-map
            "a" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer"))
            ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
            "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
            "F" (evil-textobj-tree-sitter-get-textobj "call.outer")
            "C" (evil-textobj-tree-sitter-get-textobj "class.outer")
            "c" (evil-textobj-tree-sitter-get-textobj "comment.outer")
            "v" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
            "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")
            ;; Not Working
            "m" (evil-textobj-tree-sitter-get-textobj "import"
                  '((python-mode . [(import_statement) @import])
                    (rust-mode . [(use_declaration) @import]))))
  ;; Goto
  (:states '(normal) ; Previous
           "[ga" (+tree-sitter-goto-textobj "parameter.outer" t)
           "[gf" (+tree-sitter-goto-textobj "function.outer" t)
           "[gF" (+tree-sitter-goto-textobj "call.outer" t)
           "[gC" (+tree-sitter-goto-textobj "class.outer" t)
           "[gc" (+tree-sitter-goto-textobj "comment.outer" t)
           "[gv" (+tree-sitter-goto-textobj "conditional.outer" t)
           "[gl" (+tree-sitter-goto-textobj "loop.outer" t)
           )

  (:states '(normal) ; Next
           "]ga" (+tree-sitter-goto-textobj "parameter.outer")
           "]gf" (+tree-sitter-goto-textobj "function.outer")
           "]gF" (+tree-sitter-goto-textobj "call.outer")
           "]gC" (+tree-sitter-goto-textobj "class.outer")
           "]gc" (+tree-sitter-goto-textobj "comment.outer")
           "]gv" (+tree-sitter-goto-textobj "conditional.outer")
           "]gl" (+tree-sitter-goto-textobj "loop.outer")))

(with-eval-after-load 'which-key
  (setq which-key-allow-multiple-replacements t)
  (push '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1"))
        which-key-replacement-alist))

(provide 'init-treesit)
;;; init-treesit.el ends here
