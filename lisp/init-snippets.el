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

(defvar +snippets-dir (expand-file-name "snippets/" thomas-emacs-dir)
  "Directory where `yasnippet' will search for your private snippets.")
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
  :diminish yas-minor-mode
  :init
  ;; Reduce default verbosity. 3 is too chatty about initializing yasnippet. 2
  ;; is just right (only shows errors).
  (defvar yas-verbosity 2)
  ;; Remove default ~/.emacs.d/snippets
  (defvar yas-snippet-dirs nil)
  :config
  (add-to-list 'yas-snippet-dirs '+snippets-dir)
  ;; default snippets library, if available
  (add-to-list 'load-path +snippets-dir)
  ;; HACK In case `+snippets-dir' and `doom-snippets-dir' are the same, or
  ;;      duplicates exist in `yas-snippet-dirs'.
  (advice-add #'yas-snippet-dirs :filter-return #'delete-dups)
  ;; If in a daemon session, front-load this expensive work:
  (yas-global-mode +1))

(use-package yasnippet-snippets
  :after yasnippt)

(use-package auto-yasnippet
  :after yasnippet
  :config
  (setq aya-persist-snippets-dir +snippets-dir))


(provide 'init-snippets)
;;; init-snippets.el ends here
