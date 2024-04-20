;;; init-browser.el 浏览器相关设置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-browser
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Needed by `webpaste'
(use-package browse-url
  :straight (:type built-in)
  :custom
  (when IS-WSL
    (setq browse-url-chrome-program
          "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"))
  (setq browse-url-browser-function #'browse-url-chrome)
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Pastebin service
(use-package webpaste
  :commands webpaste-paste-buffer-or-region
  :custom
  (webpaste-open-in-browser t)
  (webpaste-paste-confirmation t)
  (webpaste-add-to-killring nil)
  (webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "ix.io")))


;; Web search
(use-package webjump
  :straight (:type built-in)
  ;; C-c / will be shadowed by `org-sparse-tree' in org-mode
  :bind ("C-c C-/" . webjump)
  :config
  (defconst webjump-weather-default-cities '("º¼ÖÝ" "ÉîÛÚ" "±±¾©" "ÉÏº£"))
  (defconst webjump-weather-url-template "https://weathernew.pae.baidu.com/weathernew/pc?query=%sÌìÆø&srcid=4982")

  (defun webjump-weather (_name)
    (let ((city (completing-read "City: " webjump-weather-default-cities)))
      (format webjump-weather-url-template city)))

  (add-to-list 'browse-url-handlers '("weathernew.pae.baidu.com" . xwidget-webkit-browse-url))
  :custom
  (webjump-sites '(;; Internet search engines.
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                   ("Ludwig Guru" .
                    [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
                   ("Stack Overflow" .
                    [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                   ("Man Search" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
                   ("Man Go" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])

                   ;; Code search
                   ("Code Search" .
                    [simple-query "sourcegraph.com" "sourcegraph.com/search?q=context:global+" "&patternType=literal"])

                   ;; Life
                   ("Weather" . webjump-weather)

                   ;; Language specific engines.
                   ("x86 Instructions Reference" .
                    [simple-query "www.felixcloutier.com"
                                  "www.felixcloutier.com/x86/" ""]))))

(provide 'init-browser)
;;; init-browser.el ends here
