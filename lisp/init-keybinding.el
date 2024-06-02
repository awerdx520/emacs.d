;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(require 'core-basic)

(thomas-leader-define
 ;; Top
 ":" 'execute-extended-command
 ";" 'pp-eval-expression
 "`" 'switch-to-buffer
 "'" 'vertico-repeat
 "." 'consult-find
 "RET" 'consult-bookmark

 ;;
 "a" '(embark-act :wk "Actions")

 ;; Buffer
 "b" '(:ignore t :wk "buffer")
 ;;    "b-" 'thomas/toggle-narrow-buffer
 "b[" 'previous-buffer
 "b]" 'next-buffer
 "bc" 'clone-indirect-buffer
 "bC" 'clone-indirect-buffer-other-window
 "bd" 'kill-current-buffer
 "bi" 'ibuffer
 "bk" 'kill-current-buffer
 "bl" 'evil-switch-to-windows-last-buffer
 "bm" 'bookmark-set
 "bM" 'bookmark-delete
 "bn" 'next-buffer
 "bN" 'evil-buffer-new
 "bp" 'previous-buffer
 "br" 'revert-buffer
 "bs" 'save-buffer
 "bS" 'evil-write-all
 "by" '+default/yank-buffer-contents
 "bz" 'bury-buffer
 "bb" 'consult-buffer
 "bB" 'consult-buffer-other-window

 ;; Code
 "c" '(:ignore t :wk "code")
 "cc" 'compile
 "cC" 'recompile
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
 ;; "cx" 'consult-flycheck
 ;; File
 "f" '(:ignore t :wk "file")
 "ff" 'find-file
 "fe" '+default/find-file-in-emacs
 "fr" 'consult-recent-file

 ;; Git
 "g" '(:ignore t :wk "git")
 "g." 'magit-file-dispatch
 "g/" 'magit-dispatch
 "gb" 'magit-branch-checkout
 "gC" 'magit-clone
 "gD" 'magit-file-delete
 "gg" 'magit-status
 "gG" 'magit-status-here
 "gL" 'magit-log-buffer-file
 "gS" 'magit-stage-buffer-file
 "gU" 'magit-unstage-buffer-file

 ;; Help
 "h" '(:ignore t :wk "help")
 "hbf" 'which-key-show-full-keymap
 "hbi" 'which-key-show-minor-mode-keymap
 "hbk" 'which-key-show-keymap
 "hbm" 'which-key-show-major-mode
 "hbt" 'which-key-show-top-level
 "h RET" 'info-emacs-manual
 "h'" 'describe-char
 "h." 'display-local-help
 "h?" 'help-for-help
 "ha" 'apropos
 "ht" 'consult-theme
 "hA" 'apropos-documentation
 "hbb" 'describe-bindings
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
 "h C-w" 'describe-no-warranty

 ;; Insert
 "i" '(:ignore t :wk "insert")
 "ie" 'emoji-search
 "ir" 'consult-register
 "is" 'consult-yasnippet

 ;; Open
 "o" '(:ignore t :wk "open")
 "o-" 'dired-jump
 "oA" 'org-agenda
 "of" 'make-frame
 "oF" 'select-frame-by-name
 "oo" 'browse-url-at-point
 "ot" 'term
 "obi" 'browse-url
 "obd" 'browse-url-of-dired-file
 "obs" 'browse-url-of-file

 ;; Project
 "p" '(:ignore t :wk "project")
 "pl" 'devdocs-lookup
 "pt" 'magit-todos-list
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
 "pb" 'consult-project-buffer ;; 切换到项目中已经打开的 Buffer
 "pg" 'consult-git-grep

 ;; Quit
 "q" '(:ignore t :wk "quit/session")
 "qK" 'save-buffers-kill-emacs
 "qq" 'save-buffers-kill-terminal

 ;; Remote
 "r" '(:ignore t :wk "remote")
 "rpb" 'webpaste-paste-buffer
 "rpr" 'webpaste-paste-region
 "rpp" 'webpaste-paste-buffer-or-region
 ;; Search
 "s" '(:ignore t :wk "search")
 "se" '+default/search-emacs
 "sb" '+default/search-buffer
 "ss" '+default/search-buffer
 "sB" '+default/search-all-buffer
 "sp" 'project-find-regexp
 "sf" 'consult-locate
 "si" 'consult-imenu
 "sI" 'consult-imenu-multi
 "sm" 'consult-bookmark
 "sr" 'consult-mark
 "so" 'webjump

 ;; Window
 "w" '(:ignore t :wk "window")
 "w=" 'balance-windows
 "wf" 'ffap-other-window
 "wT" 'tear-off-window
 "w C-o" 'delete-other-windows
 "w:" 'evil-ex
 "w+" 'evil-window-increase-height
 "w-" 'evil-window-decrease-height
 "w<" 'evil-window-decrease-width
 "w>" 'evil-window-decrease-width
 "w_" 'evil-window-set-height
 "wb" 'evil-window-bottom-right
 "wc" 'evil-window-delete
 "wd" 'evil-window-delete
 "wh" 'evil-window-left
 "wj" 'evil-window-down
 "wk" 'evil-window-up
 "wl" 'evil-window-right

 ;;"wm" 'maxiz
 "wn" 'evil-window-new
 "wp" 'evil-window-mru
 "wq" 'evil-quit
 "wr" 'evil-window-rotate-downwards
 "wR" 'evil-window-rotate-upwards
 "ws" 'evil-window-split
 "wt" 'evil-window-top-left
 "wv" 'evil-window-vsplit
 "ww" 'evil-window-next
 "wW" 'evil-window-prev
 "wx" 'evil-window-exchange
 "w|" 'evil-window-set-width)

(general-def :states 'normal
  ;;    "] t" '+evil/next-frame
  ;;   "[ t" '+evil/previous-frame
  "] f" '+evil/next-file
  "[ f" '+evil/previous-file)

(provide 'init-keybinding)
;;; init-keybinding.el ends here
