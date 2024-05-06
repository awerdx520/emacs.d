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

;;
;;; built-in help mode
(use-package help
  :straight (:type built-in)
  :general
  (thomas-leader
   "h RET" 'info-emacs-manual
   "h'" 'describe-char
   "h." 'display-local-help
   "h?" 'help-for-help
   "ha" 'apropos
   "hA" 'apropos-documentation
   "hbb" 'describe-bindings
   ;;
   "hc" 'describe-key-briefly
   "hC" 'describe-coding-system
   "he" 'view-echo-area-messages
   "hf" 'describe-function
   "hF" 'describe-face
   "hg" 'describe-gnu-project
   "hi" 'info
   "hI" 'describe-input-method
   "hk" 'describe-key
   "hK" 'Info-goto-emacs-key-command-node
   "hl" 'view-lossage
   "hL" 'describe-language-environment
   "hm" 'describe-mode
   "ho" 'describe-symbol
   "hP" 'find-library
   "hq" 'help-quit
   "hR" 'info-display-manual
   "hs" 'describe-syntax
   "hS" 'info-lookup-symbol
   "hv" 'describe-variable
   "hw" 'where-is
   "hW" 'woman
   "hR" 'info-display-manual
   "hx" 'describe-command
   ;;
   "h C-d" 'view-emacs-debugging
   "h C-f" 'view-emacs-FAQ
   "h C-n" 'view-emacs-news
   "h C-p" 'view-emacs-problems
   "h C-t" 'view-emacs-todo
   "h C-w" 'describe-no-warranty))
;; Browse devdocs.io
(use-package devdocs
  :general
  (thomas-leader
   "pl" 'devdocs-lookup)
  :config
  (setq devdocs-data-dir (concat thomas-data-dir "devdocs"))
  (add-to-list 'completion-category-overrides
               '(devdocs (styles . (flex)))))

;; Quick editing in `describe-variable'
(with-eval-after-load 'help-fns
  (put 'help-fns-edit-variable 'disabled nil))

;; colorful help mode
(use-package helpful
  :general
  (general-def :keymaps 'override
    [remap describe-function] #'helpful-callable
    [remap describe-command]  #'helpful-command
    [remap describe-variable] #'helpful-variable
    [remap describe-key]      #'helpful-key
    [remap describe-symbol]   #'helpful-symbol)
  (general-def :keymaps '(help-mode-map helpful-mode-map)
    :states 'normal
    "q" 'quit-window
    [escape] 'quit-window
    "Q" 'kill-current-buffer)
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


(provide 'init-help)
;;; init-help.el ends here
