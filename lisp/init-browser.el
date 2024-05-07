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
  :config
  ;; 当使用 WSL2 环境时可以使用 wslview 作为默认打开浏览器打开连接。
  ;; wlsview 是 wslu 工具集中的命令，可是根据传递的参数选择合适的
  ;; Windows 软件查看他。就是有点慢，不知道是 wslview 自己的问题
  ;; 还是 Windws 的配置匹配问题。
  (if IS-WSL
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "wslview")
    (setq browse-url-browser-function #'browse-url-chrome))

  ;; TODO 需要配置怎么通过默认 browser 打开文件
  (setq browse-url-handlers '(("\\`file:'" . browse-url-default-browser))))

;; web search
(use-package webjump
  :straight (:type built-in)
  :config
  (setq webjump-sites '(;; Internet search engines.
                        ("Google" .
                         [simple-query "www.google.com"
                                       "www.google.com/search?q=" ""])
                        ("Wikipedia" .
                         [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                        ("Ludwig Guru" .
                         [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
                        ("Stack Overflow" .
                         [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                        ("Archlinux Wiki" .
                         [simple-query "archlinuxcn.org" "wiki.archlinuxcn.org/wzh/index.php?search=" "&title=Special%3A%E6%90%9C%E7%B4%A2&profile=advanced&fulltext=1&ns0=1"])

                        ("AUR" .
                         [simple-query "archlinux.org" "aur.archlinux.org/packages?O=0&K=" ""])
                        ("Man Search" .
                         [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
                        ("Man Go" .
                         [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])
                        ("StackOverflow" .
                         [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                        ("Github"         .
                         [smple-query "github.com"     "github.com/search?ref=simplesearch&q=" ""])
                        ("Wolfram alpha"    .
                         [smple-query "wolframalpha.com"   "wolframalpha.com/input/?i=" ""])
                        ("MDN"  .
                         [smple-query "mozilla.org"               "developer.mozilla.org/en-US/search?q=" ""])
                        ("Rust Docs" .
                         [smple-query "rust-lang.org"  "doc.rust-lang.org/std/?search=" ""])
                        ;; Code search
                        ("Code Search" .
                         [simple-query "sourcegraph.com" "sourcegraph.com/search?q=context:global+" "&patternType=literal"])

                        ;; Language specific engines.
                        ("x86 Instructions Reference" .
                         [simple-query "www.felixcloutier.com"
                                       "www.felixcloutier.com/x86/" ""]))))
;; Pastebin service
(use-package webpaste
  :commands webpaste-paste-buffer-or-region
  :config
  (setq webpaste-open-in-browser t ;; After a successful paste, the returned URL from the provider will be sent to the killring.
        ;; Require confirmation before doing paste
        webpaste-paste-confirmation t
        ;; After a successful paste, the returned URL from the provider will be sent to the killring.
        webpaste-add-to-killring t
        webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "gist.github.com")))

(provide 'init-browser)
;;; init-browser.el ends here
