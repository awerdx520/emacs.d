;;; init-spell.el 检查相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: May 08, 2024
;; Modified: May 08, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-check
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
;; Interactive spell checker
;;
;; z= `ispell-word'
(use-package ispell
  :straight (:type built-in)
  :general
  (general-def :states 'normal
    "z=" 'ispell-word)
  :config
  ;; MacOS is broken
  (when (eq system-type 'darwin)
    (setenv "DICTIONARY" "en_US"))

  ;; no spell checking for org special blocks
  (appendq! ispell-skip-region-alist '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                                       ("#\\+begin_src" . "#\\+end_src")
                                       ("#\\+begin_example" . "#\\+end_example")))
  :config
  (setq ispell-really-hunspell t
        ispell-program-name "hunspell"
        ispell-dictionary "en_US"
        ispell-following-word t
        ispell-personal-dictionary (expand-file-name thomas-cache-dir "hunspell_dict.txt")))

;; Spell check on-the-fly
(use-package flyspell
  :straight (:type built-in)
  :config
  ;; Use M-C-i instead if M-TAB is shadowed by your window manager
  (setq flyspell-use-meta-tab t
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(provide 'init-spell)
;;; init-spell.el ends here
