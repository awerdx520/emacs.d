;;; init-help.el Emacs 帮助系统配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-help
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Browse devdocs.io
(use-package devdocs
  :bind ("C-c b" . devdocs-lookup)
  :config
  (setq devdocs-data-dir (concat thomas-data-dir "devdocs"))
  (add-to-list 'completion-category-overrides
               '(devdocs (styles . (flex)))))

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :general
  (general-def :keymaps 'override
    [remap describe-function] #'helpful-callable
    [remap describe-command]  #'helpful-command
    [remap describe-variable] #'helpful-variable
    [remap describe-key]      #'helpful-key
    [remap describe-symbol]   #'helpful-symbol)
  :init
  ;; Make `apropos' et co search more extensively. They're more useful this way.
  (setq apropos-do-all t)
  (with-eval-after-load 'apropos
    ;; patch apropos buttons to call helpful instead of help
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol)))))))

(use-package help
  :straight (:type built-in)
  :general
  (thomas-leader-help
   "" '(:ignore t :wk "help")
   "RET" 'info-emacs-manual
   "'" 'describe-char
   "." 'display-local-help
   "?" 'help-for-help
   "a" 'apropos
   "A" 'apropos-documentation
   "bb" 'describe-bindings
   "bf" 'which-key-show-full-keymap
   "bi" 'which-key-show-minor-mode-keymap
   "bk" 'which-key-show-keymap
   "bm" 'which-key-show-major-mode
   "bt" 'which-key-show-top-level
   "c" 'describe-key-briefly
   "C" 'describe-coding-system
   "e" 'view-echo-area-messages
   "f" 'describe-function
   "F" 'describe-face
   "g" 'describe-gnu-project
   "i" 'info
   "I" 'describe-input-method
   "k" 'describe-key
   "K" 'Info-goto-emacs-key-command-node
   "l" 'view-lossage
   "L" 'describe-language-environment
   "m" 'describe-mode
   "o" 'describe-symbol
   "P" 'find-library
   "q" 'help-quit
   "R" 'info-display-manual
   "s" 'describe-syntax
   "S" 'info-lookup-symbol
   "t" 'consult-theme
   "v" 'describe-variable
   "w" 'where-is
   "W" 'woman
   "R" 'info-display-manual
   "x" 'describe-command
   "C-d" 'view-emacs-debugging
   "C-f" 'view-emacs-FAQ
   "C-n" 'view-emacs-news
   "C-p" 'view-emacs-problems
   "C-t" 'view-emacs-todo
   "C-w" 'describe-no-warranty))

(provide 'init-help)
;;; init-help.el ends here
