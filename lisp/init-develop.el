;;; init-dev.el --- Programming development -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar +project-ignore-project-list '("~" "/home/thomas" "/")
  "忽略指定目录作为项目目录.")


;;
(use-package project
  :straight (:type built-in)
  :init
  (setq project-list-file (concat thomas-data-dir "projects"))
  :general
  (thomas-leader
    "p!" 'project-shell-command ; 在项目根目录下执行命令
    "p&" 'project-async-shell-command
    "pa" 'project-remember-projects-under ; 添加新项目
    "pc" 'project-compile ; 执行编译命令
    "pd" 'project-find-dir ; 在 dired 中打开选定的文件夹
    "pD" 'project-dired ; 在项目根目录打开 dired
    "pf" 'project-find-file ; 在项目中搜索文件
    "pF" 'project-or-external-find-file
    "pp" 'project-switch-project ; 切换项目
    "pv" 'project-vc-dir
    "px" 'project-execute-extended-command
    "pr" 'project-query-replace-regexp
    "pk" 'project-kill-buffers ; kill 所有项目 buffer
    "sp" 'project-find-regexp)
  :config
  (cl-defmethod project-root ((project (head local)))
    "Return root directory of current PROJECT."
    (cdr project))

  (defun thomas/project-try-flag (dir)
    "Determine if DIR is a non-Git project."
    (unless (member (expand-file-name "" dir)
                    +project-ignore-project-list)
      (catch 'ret
        (let ((pr-flags '((".project")
                          ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
                          ("Makefile" "README.org" "README.md"))))
          (dolist (current-level pr-flags)
            (dolist (f current-level)
              (when-let ((root (locate-dominating-file dir f)))
                (throw 'ret (cons 'local root)))))))))

  (setq project-find-functions '(project-try-vc thomas/project-try-flag)))

(use-package compile
  ;; Compilation Mode
  ;; TODO understand
  :straight (:type built-in)
  :hook (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-always-kill t ; kill compilation process before starting another
        compilation-ask-about-save nil  ; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t))

(use-package comint
  ;; ComintMode 用于制作 shell 或 repl 之类的缓冲区，在其中与外部进程交互。
  ;; TODO understand
  :straight (:type built-in)
  :config
  (setq comint-prompt-read-only t
        ;; double the default
        comint-buffer-maximum-size 2048)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

;; The unified debugger
(use-package gud
  :straight (:type built-in)
  :hook (gud-mode . gud-tooltip-mode)
  :config
  (setq gud-highlight-current-line t))

;; GDB specific config
(use-package gdb-mi
  :straight (:type built-in)
  :commands gdb
  :config
  (setq gdb-show-main t
        gdb-display-io-nopopup t
        gdb-show-changed-values t
        gdb-delete-out-of-scope t
        gdb-use-colon-colon-notation t
        gdb-debuginfod-enable-setting nil
        gdb-restore-window-configuration-after-quit t))

;; Insert SPDX license header
(use-package spdx
  :hook (prog-mode . spdx-tempo-setup)
  :config
  (setq spdx-ignore-deprecated t))

;; Quickrun codes, including cpp. awesome!
(use-package quickrun
  :config
  (setq quickrun-focus-p nil
        quickrun-input-file-extension ".qr"))

;; xref
(use-package xref
  :straight (:type built-in)
  :hook ((xref-after-return xref-after-jump) . recenter)
  :config
  ;; Emacs 28+
  ;;
  ;; `project-find-regexp' can be faster when setting `xref-search-program' to
  ;;  `ripgrep'.
  (setq xref-search-program (cond ((executable-find "rg") 'ripgrep)
                                  ((executable-find "ugrep") 'ugrep)
                                  (t 'grep))
        xref-history-storage 'xref-window-local-history
        xref-show-xrefs-function #'xref-show-definitions-completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read))

;; Jump to definition, used as a fallback of lsp-find-definition
(use-package dumb-jump
  :config
  (setq dumb-jump-quiet t
        dumb-jump-aggressive t
        dumb-jump-selector 'completing-read))

;; Hiding structured data
;; zm hide-all
;; zr show-all
;; za toggle-fold
;; zo show-block
;; zc hide-block
(use-package hideshow
  :straight (:type built-in)
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  (defconst hideshow-folded-face '((t (:inherit 'font-lock-comment-face :box t))))
  (defface hideshow-border-face
    '((((background light))
       :background "rosy brown" :extend t)
      (t
       :background "sandy brown" :extend t))
    "Face used for hideshow fringe."
    :group 'hideshow)

  (define-fringe-bitmap 'hideshow-folded-fringe
    (vector #b00000000
            #b00000000
            #b00000000
            #b11000011
            #b11100111
            #b01111110
            #b00111100
            #b00011000))

  (defun hideshow-folded-overlay-fn (ov)
    "Display a folded region indicator with the number of folded lines."
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((nlines (count-lines (overlay-start ov) (overlay-end ov)))
             (info (format " (%d)..." nlines)))
        ;; fringe indicator
        (overlay-put ov 'before-string (propertize " "
                                                   'display '(left-fringe hideshow-folded-fringe
                                                                          hideshow-border-face)))
        ;; folding indicator
        (overlay-put ov 'display (propertize info 'face hideshow-folded-face)))))
  (setq hs-set-up-overlay #'hideshow-folded-overlay-fn))

;; XML
(use-package nxml-mode
  :straight (:type built-in)
  :mode (("\\.xml\\'" . nxml-mode)
         ("\\.rss\\'" . nxml-mode))
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))

;; Config files mode
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; Syntax highlighting for systemd files
(use-package conf-mode
  :straight (:type built-in)
  :mode ((rx "."
             (or "automount" "busname" "link" "mount" "netdev" "network"
                 "path" "service" "slice" "socket" "swap" "target" "timer")
             string-end) . conf-toml-mode))


(use-package lsp-bridge
  :straight '(lsp-bridge  :fetcher github :repo "manateelazycat/lsp-bridge"
                          :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                          :build (:not compile))
  :hook (prog-mode . lsp-bridge-semantic-tokens-mode)
  :init
  (global-lsp-bridge-mode)

  (defvar +lsp-bridge-data-dir
    (expand-file-name "lsp-bridge" thomas-data-dir)
    "Lsp-Bridge 相关配置目录.")
  :general
  (thomas-leader
    "ca" 'lsp-bridge-code-action ; 弹出代码修复菜单
    "cj" 'lsp-bridge-find-def ; 跳转到定义位置
    "cJ" 'lsp-bridge-find-def-other-window ; 在其他窗口跳转到定义位置
    "cr" 'lsp-bridge-rename ; 重命名
    "ci" 'lsp-bridge-find-impl ; 跳转到接口实现位置
    "cI" 'lsp-bridge-find-impl-other-window ; 在其他窗口跳转到接口实现位置
    "ct" 'lsp-bridge-find-type-def ; 跳转到类型定义位置
    "cT" 'lsp-bridge-find-type-def-other-window ; 在其他窗口跳转到类型定义位置
    "cD" 'lsp-bridge-find-references ; 查看代码引用
    "ck" 'lsp-bridge-show-documentation
    "cx" 'lsp-bridge-diagnostic-list ; 列出所有诊断信息
    )
  :config
  (setq lsp-bridge-python-lsp-server "pyright"
        lsp-bridge-c-lsp-server "ccls")
  ;; 定期(以秒为单位)给远程服务器发送心跳包
  (setq lsp-bridge-remote-heartbeat-interval 10)
  (setq lsp-bridge-org-babel-lang-list '("c" "c++" "python" "java" "go" "rust" "scala"))
  ;; 跳转定义和引用的 fallback function
  (setq lsp-bridge-find-def-fallback-function #'xref-find-definitions)
  (setq lsp-bridge-find-ref-fallback-function #'xref-find-references)
  ;; 设置自定义 langserver 配置目录
  (setq lsp-bridge-user-langserver-dir (expand-file-name "langserver" +lsp-bridge-data-dir)
        lsp-bridge-user-multiserver-dir (expand-file-name "multiserver" +lsp-bridge-data-dir))
  (defface lsp-bridge-semantic-tokens-variable-face
    '((t (:inherit font-lock-variable-name-face)))
    "Face used for variable name."
    :group 'lsp-bridge-semantic-tokens)

  (defface lsp-bridge-semantic-tokens-global-scope-face
    '((t :weight extra-bold))
    "Face used for globalScope token."
    :group 'lsp-bridge-semantic-tokens)

  (setq-default lsp-bridge-semantic-tokens-type-faces
                [("variable" . lsp-bridge-semantic-tokens-variable-face)])

  (setq-default lsp-bridge-semantic-tokens-type-modifier-faces
                [("globalScope" . lsp-bridge-semantic-tokens-global-scope-face)])

  (setq-default lsp-bridge-semantic-tokens-ignore-modifier-limit-types [])
  ;; 在 Org Babel 里使用 LSP 补全
  (setq lsp-bridge-enable-org-babel t))

(provide 'init-develop)

;;; init-develop.el ends here
