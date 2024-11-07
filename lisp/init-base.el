;;; init-base.el 基础配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: 五月 07, 2024
;; Modified: 五月 07, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-base
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; Requisites
(require 'init-const)
(require 'init-funcs)

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000) ;64kb

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Emacs 垃圾收集优化
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init (setq gcmh-idle-delay 'auto
              gcmh-auto-idle-delay-factor 10
              gcmh-high-cons-threshold #x1000000)) ; 16MB


;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")

;; 解决复制中文出现乱码
(if IS-WSL
    (set-selection-coding-system 'gbk)
  (set-selection-coding-system 'utf-8))

;; 保存访问过文件的光标位置.
(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-ignore-files-regexp
        "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|elpa\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$"
        save-place-file (expand-file-name "saveplace" thomas-cache-dir)))

;; 保存最近的编辑过的那些文件
(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300
        recentf-save-file (expand-file-name "recentf" thomas-cache-dir)
        ;; The most sensible time to clean up your recent files list is when you quit
        ;; Emacs (unless this is a long-running daemon session).
        recentf-auto-cleanup (if (daemonp) 300 'mode)
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; 保存 session 相关配置，如：kill-ring 等变量
(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :init
  (setq savehist-autosave-interval 300
        savehist-save-minibuffer-history t
        savehist-file (expand-file-name "savehist" thomas-cache-dir)

        ;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
        ;; while we're in the minibuffer.
        enable-recursive-minibuffers t
        history-length 1000
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          ;; persist searches
          search-ring regexp-search-ring)))

;; 自动重载被修改过的文件.
(use-package autorevert
  :straight (:type built-in)
  :diminish auto-revert-mode
  :hook (find-file . global-auto-revert-mode))

(use-package simple
  :straight (:type built-in)
  :init
  (setq column-number-mode t
        line-number-mode nil
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default

  ;; 在保存 buffer 之前显示末尾空白，然后保存之后删除末尾空白
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  :hook ((after-init . size-indication-mode)
         (text-mode .  visual-line-mode)
         ((prog-mode conf-mode markdown-mode) . #'enable-trailing-whitespace)))

;; Misc
;; 简化 yes-or-no 输入
(setq use-short-answers t
      ;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
      y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil) ; Permanently indent with spaces, never with TABs


(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      initial-major-mode 'text-mode
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)

;; Environment
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Treesit
(use-package treesit-auto
  :config
  (setq treesit-font-lock-level 4
        treesit-auto-install 'prompt)

  ;; 自动注册 `treesit-auto-langs' 中所有的拓展到 auto-mode-alist 中
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

(use-package general
  :init
  (with-eval-after-load 'evil
    (general-add-hook 'after-init-hook
                      (lambda (&rest _)
                        (when-let ((messages-buffer (get-buffer "*Messages*")))
                          (with-current-buffer messages-buffer
                            (evil-normalize-keymaps))))
                      nil
                      nil
                      t))
  ;; leader key define
  (general-create-definer thomas-leader-define
    :states '(normal visual motion evilified)
    :prefix thomas-leader-key
    :non-normal-key thomas-localleader-key
    :keymaps 'override)
  :config
  (general-def :states '(normal visual)
    "]f" '+thomas/find-next-file
    "[f" '+thomas/find-previous-file)

  (thomas-leader-define
    ;; Top
    ":" 'execute-extended-command
    ";" 'pp-eval-expression
    "`" 'switch-to-buffer

    ;; Buffer
    "b" '(:ignore t :wk "buffer")
    "b[" 'previous-buffer
    "b]" 'next-buffer
    "bc" 'clone-indirect-buffer
    "bC" 'clone-indirect-buffer-other-window
    "bd" 'kill-current-buffer
    "bi" 'ibuffer
    "bk" 'kill-current-buffer
    "bm" 'bookmark-set
    "bM" 'bookmark-delete
    "bn" 'next-buffer
    "bN" 'evil-buffer-new
    "bp" 'previous-buffer
    "br" 'revert-buffer
    "bs" 'save-buffer
    "bS" 'evil-write-all
    "bz" 'bury-buffer
    "bx" '+thomas/switch-to-scratch-buffer

    ;; Code
    "c" '(:ignore t :wk "code")
    "cc" 'compile
    "cC" 'recompile

    ;; do
    "d" '(:ignore t :wk "do")

    ;; File
    "f" '(:ignore t :wk "file")
    "ff" 'find-file
    "fe" '+thomas/find-file-in-emacsd

    ;; Git
    "g" '(:ignore t :wk "git")

    ;; Insert
    "i" '(:ignore t :wk "insert")
    "ie" 'emoji-search

    ;; Open
    "o" '(:ignore t :wk "open")
    "o-" 'dired-jump
    "oA" 'org-agenda
    "of" 'make-frame
    "oF" 'select-frame-by-name

    ;; Project
    "p" '(:ignore t :wk "project")

    ;; Quit
    "q" '(:ignore t :wk "quit/session")
    "qf" 'delete-frame
    "qF" 'delete-other-frames
    "qK" 'save-buffers-kill-emacs
    "qq" 'save-buffers-kill-terminal

    ;; Remote
    "r" '(:ignore t :wk "remote")

    ;; Search
    "s" '(:ignore t :wk "search")
    "sb" '+thomas/search-buffer
    "se" '+thomas/search-emacsd

    ;; Window
    "w" '(:ignore t :wk "window")
    "w=" 'balance-windows
    "wf" 'ffap-other-window
    "wT" 'tear-off-window
    "w C-o" 'delete-other-windows))

(use-package which-key
  :diminish which-key-mode "Ⓚ"
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)

  (put 'which-key-replacement-alist 'initial-value which-key-replacement-alist)
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)
  (defun +which-key-set-line-spaceing-fn ()
    "Set `which-key' line spacing"
    (setq line-spacing 3))
  (add-hook 'which-key-init-buffer-hook #'+which-key-set-line-spaceing-fn)
  ;;
  (which-key-add-key-based-replacements thomas-leader-key "<leader>")
  (which-key-add-key-based-replacements thomas-localleader-key "<localleader>"))

(use-package bookmark
  :custom
  (bookmark-default-file (concat thomas-data-dir "bookmarks"))
  :config
  (with-no-warnings
    ;; Display icons for bookmarks
    (defun +bookmark-bmenu--revert ()
      "Re-populate `tabulated-list-entries'."
      (let (entries)
        (dolist (full-record (bookmark-maybe-sort-alist))
          (let* ((name       (bookmark-name-from-full-record full-record))
                 (annotation (bookmark-get-annotation full-record))
                 (location   (bookmark-location full-record))
                 (file       (file-name-nondirectory location))
                 (type       (let ((fmt "%-8.8s"))
                               (cond ((null location)
                                      (propertize (format fmt "NOFILE") 'face 'warning))
                                     ((file-remote-p location)
                                      (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                     ((not (file-exists-p location))
                                      (propertize (format fmt "NOTFOUND") 'face 'error))
                                     ((file-directory-p location)
                                      (propertize (format fmt "DIRED") 'face 'warning))
                                     (t (propertize (format fmt "FILE") 'face 'success)))))
                 (icon       (if (icons-displayable-p)
                                 (cond
                                  ((file-remote-p location)
                                   (nerd-icons-codicon "nf-cod-radio_tower"))
                                  ((file-directory-p location)
                                   (nerd-icons-icon-for-dir location))
                                  ((not (string-empty-p file))
                                   (nerd-icons-icon-for-file file)))
                               "")))
            (push (list
                   full-record
                   `[,(if (and annotation (not (string-equal annotation "")))
                          "*" "")
                     ,icon
                     ,(if (display-mouse-p)
                          (propertize name
                                      'font-lock-face 'bookmark-menu-bookmark
                                      'mouse-face 'highlight
                                      'follow-link t
                                      'help-echo "mouse-2: go to this bookmark in other window")
                        name)
                     ,type
                     ,@(if bookmark-bmenu-toggle-filenames
                           (list (propertize location 'face 'completions-annotations)))])
                  entries)))
        (tabulated-list-init-header)
        (setq tabulated-list-entries entries))
      (tabulated-list-print t))
    (advice-add #'bookmark-bmenu--revert :override #'+bookmark-bmenu--revert)

    (defun +bookmark-bmenu-list ()
      "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
      (interactive)
      (bookmark-maybe-load-default-file)
      (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
        (if (called-interactively-p 'interactive)
            (pop-to-buffer buf)
          (set-buffer buf)))
      (bookmark-bmenu-mode)
      (bookmark-bmenu--revert))
    (advice-add #'bookmark-bmenu-list :override #'+bookmark-bmenu-list)

    (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (setq tabulated-list-format
            `[("" 1) ;; Space to add "*" for bookmark with annotation
              ("" ,(if (icons-displayable-p) 2 0)) ;; Icons
              ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
              ("Type" 9)
              ,@(if bookmark-bmenu-toggle-filenames
                    '(("File" 0 bookmark-bmenu--file-predicate)))])
      (setq tabulated-list-padding bookmark-bmenu-marks-width)
      (setq tabulated-list-sort-key '("Bookmark" . nil))
      (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
      (setq revert-buffer-function #'bookmark-bmenu--revert)
      (tabulated-list-init-header))))

(provide 'init-base)
;;; init-base.el ends here
