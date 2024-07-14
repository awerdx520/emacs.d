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
  (thomas-leader-define
    "oo" 'browse-url-at-point
    "obi" 'browse-url
    "obd" 'browse-url-of-dired-file
    "obs" 'browse-url-of-file)
  :config
  ;; 当使用 WSL2 环境时可以使用 wslview 作为默认打开浏览器打开连接。
  ;; wlsview 是 wslu 工具集中的命令，可是根据传递的参数选择合适的
  ;; Windows 软件查看他。就是有点慢，不知道是 wslview 自己的问题
  ;; 还是 Windws 的配置匹配问题。
  (if IS-WSL
      (setq browse-url-generic-program "wslview"
            browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program browse-url-chrome-program))

  ;; TODO 需要配置怎么通过默认 browser 打开文件
  (setq browse-url-handlers '(("\\`file:'" . browse-url-default-browser))))


(use-package webjump
  :straight (:type built-in)
  :general
  (thomas-leader-define "so" 'webjump)
  :config
  (setq webjump-sites '(;; Internet search engines.
                        ("Google -- 全球搜索网站" .
                         [simple-query "www.google.com"
                                       "www.google.com/search?q=" ""])
                        ("Baidu -- 中文搜索网站" .
                         [simple-query "www.baidu.com"
                                       "www.baidu.com/s?wd=" ""])
                        ("DuckDuckGo -- 隐私搜索网站" .
                         [simple-query "www.duckduckgo.com"
                                       "www.duckduckgo.com/search?q=" ""])

                        ;; Emacs
                        ("Emacs China -- Emacs 中文论坛" .
                         [simple-query "emacs-china.org" "emacs-china.org/search?q=" ""])
                        ("Melpa -- Milkypostman 的 Emacs Lisp 软件包存档" .
                         [simple-query "melpa.org" "melpa.org/#/?q=vs" ""])

                        ;; Linux
                        ("Archlinux Wiki -- Archlinux wiki帮助页面" .
                         [simple-query "archlinuxcn.org" "wiki.archlinuxcn.org/wzh/index.php?search=" "&title=Special%3A%E6%90%9C%E7%B4%A2&profile=advanced&fulltext=1&ns0=1"])
                        ("AUR -- Arch 用户软件仓库" .
                         [simple-query "archlinux.org" "aur.archlinux.org/packages?O=0&K=" ""])
                        ("Man Search -- Arch Linux 软件包的手册页面" .
                         [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
                        ("Man Go -- Arch Linux 手册精准搜索" .
                         [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])

                        ;; Develop
                        ("Stack Overflow -- 程序设计领域的问答网站" .
                         [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                        ("Github -- 全球最大男性交友平台"         .
                         [simple-query "github.com" "github.com/search?ref=simplesearch&q=" ""])
                        ("MDN -- 前端或网络相关开发资源"  .
                         [simple-query "mozilla.org" "developer.mozilla.org/en-US/search?q=" ""])
                        ("Rust Docs -- Rust 帮助页面" .
                         [simple-query "rust-lang.org"  "doc.rust-lang.org/std/?search=" ""])
                        ("Rust Crate -- Rust crates 包搜索" .
                         [simple-query "crates.io"  "crates.io/search?q=" ""])
                        ("Golang Pkg -- Golang 包搜索" .
                         [simple-query "pkg.go.dev"  "pkg.go.dev/search?q=net" ""])
                        ("Regex101 -- 在线正则表达式测试和调试" . "regex101.com")
                        ("Code Search -- 代码搜索和具有代码图上下文的人工智能助手" .
                         [simple-query "sourcegraph.com" "sourcegraph.com/search?q=context:global+" "&patternType=literal"])

                        ;; AI
                        ("Huggingface Model -- Huggingface 模型搜索" .
                         [simple-query "huggingface.co"  "huggingface.co/models?sort=trending&search=" ""])
                        ("Huggingface Dataset -- Huggingface 数据集搜索" .
                         [simple-query "huggingface.co"  "huggingface.co/dataset?sort=trending&search=" ""])
                        ("Ollama Modle -- Ollama 模型搜索" .
                         [simple-query "ollama.com"  "ollama.com/library?q=" ""])

                        ;; Wiki
                        ("Wikipedia -- 维基百科" .
                         [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                        ("Annas Archive -- 安娜的档案（人类历史上最大的完全开放的图书馆）" .
                         [simple-query "zh.annas-archive.org" "zh.annas-archive.org/search?q=" ""])

                        ;; Video


                        ;; Writing
                        ("Ludwig Guru -- 您的英语写作平台" .
                         [simple-query "ludwig.guru" "ludwig.guru/s/" ""]))))

(use-package webpaste
  :commands webpaste-paste-buffer-or-region
  :general
  (thomas-leader-define
    "rp" '(:ignore t :wk "webpaste")
    "rpb" 'webpaste-paste-buffer
    "rpr" 'webpaste-paste-region
    "rpp" 'webpaste-paste-buffer-or-region)
  :config
  (setq webpaste-open-in-browser t
        webpaste-paste-confirmation t ; 在执行粘贴之前需要确认
        webpaste-add-to-killring t ; 成功粘贴后将返回的 url 自动保存到 killring中。
        webpaste-provider-priority '("paste.mozilla.org" "dpaste.org" "gist.github.com")))

(provide 'init-browser)
;;; init-browser.el ends here
