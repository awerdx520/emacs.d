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
  :general
  (thomas-leader-open
   "b" 'browse-url
   "o" 'browse-url-at-point
   "s" 'browse-url-of-file)
  :config
  (when IS-WSL
    (setq browse-url-chrome-program
          "/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"))

  ;; 默认使用 Google Chrome
  (setq browse-url-browser-function #'browse-url-chrome
        browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

;; Pastebin service
(use-package webpaste
  :commands webpaste-paste-buffer-or-region
  :general
  (thomas-leader-remote
   "p b" 'webpaste-paste-buffer
   "p r" 'webpaste-paste-region
   "p p" 'webpaste-paste-buffer-or-region)
  :config
  (setq webpaste-open-in-browser t ;; After a successful paste, the returned URL from the provider will be sent to the killring.
        ;; Require confirmation before doing paste
        webpaste-paste-confirmation t
        ;; After a successful paste, the returned URL from the provider will be sent to the killring.
        webpaste-add-to-killring t
        webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "gist.github.com")))


;; web search
(use-package webjump
  :straight (:type built-in)
  :general
  (thomas-leader-search
   "o" 'webjump)
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
                        ("DevDocs.io" .
                         [simple-query "devdocs.io" "devdocs.io/#q=" ""])
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

(provide 'init-browser)
;;; init-browser.el ends here
