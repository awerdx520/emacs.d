;;; init-editor.el 编辑相关配置 -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 tangxin
;;
;; Author: tangxin <1164074502@qq.com>
;; Maintainer: tangxin <1164074502@qq.com>
;; Created: April 21, 2024
;; Modified: April 21, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thomas/init-editor
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;; This package lets you enable minor modes based on file name and contents.
;; To find the right modes, it checks filenames against patterns in `auto-minor-mode-alist'
;; and file contents against `auto-minor-mode-magic-alist'.
;; These work like the built-in Emacs variables `auto-mode-alist' and `magic-mode-alist'.
;; Unlike major modes, all matching minor modes are enabled, not only the first match.
(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode)))

;;; Edit
;; 智能添加 parens 符号
(use-package smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  :hook (after-init . smartparens-global-mode)
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  ;; smartparens recognizes `slime-mrepl-mode', but not `sly-mrepl-mode', so...
  (add-to-list 'sp-lisp-modes 'sly-mrepl-mode)
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil)
    ;; Smartparens conditional binds a key to C-g when sp overlays are active
    ;; (even if they're invisible). This disruptively changes the behavior of
    ;; C-g in insert mode, requiring two presses of the key to exit insert mode.
    ;; I don't see the point of this keybind, so...
    (setq sp-pair-overlay-keymap (make-sparse-keymap)))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for some modes), we reduce it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 25)
  ;; No pair has any business being longer than 4 characters; if they must, set
  ;; it buffer-locally. It's less work for smartparens.
  (setq sp-max-pair-length 4)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (defun thomas-init-smartparens-in-eval-expression-h ()
    "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
    (when smartparens-global-mode (smartparens-mode +1)))
  (add-hook 'eval-expression-minibuffer-setup-hook #'thomas-init-smartparens-in-eval-expression-h)

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "'" nil :actions nil)
  (sp-local-pair '(minibuffer-mode minibuffer-inactive-mode) "`" nil :actions nil))

(use-package transient
  :init
  (setq transient-levels-file (concat thomas-data-dir "transient/levels")
        transient-values-file (concat thomas-data-dir "transient/values")
        transient-history-file (concat thomas-data-dir "transient/history"))
  :config
  (transient-define-prefix scroll-other-window-menu ()
    "Scroll other window."
    :transient-suffix     'transient--do-stay
    [["Line"
      ("j" "next line" scroll-other-window-line)
      ("k" "previous line" scroll-other-window-down-line)]
     ["Page"
      ("C-f" "next page" scroll-other-window)
      ("C-b" "previous page" scroll-other-window-down)]])

  (defun scroll-other-window-line ()
    "Scroll up of one line in other window."
    (interactive)
    (scroll-other-window 1))

  (defun scroll-other-window-down-line ()
    "Scroll down of one line in other window."
    (interactive)
    (scroll-other-window-down 1))

  (transient-define-prefix background-opacity-menu ()
    "Set frame background opacity."
    [:description
     background-opacity-get-alpha-str
     ("+" "increase" background-opacity-inc-alpha :transient t)
     ("-" "decrease" background-opacity-dec-alpha :transient t)
     ("=" "set to ?" background-opacity-set-alpha)])

  (defun background-opacity-inc-alpha (&optional n)
    (interactive)
    (let* ((alpha (background-opacity-get-alpha))
           (next-alpha (cl-incf alpha (or n 1))))
      (set-frame-parameter nil 'alpha-background next-alpha)))

  (defun background-opacity-dec-alpha ()
    (interactive)
    (background-opacity-inc-alpha -1))

  (defun background-opacity-set-alpha (alpha)
    (interactive "nSet to: ")
    (set-frame-parameter nil 'alpha-background alpha))

  (defun background-opacity-get-alpha ()
    (pcase (frame-parameter nil 'alpha-background)
      ((pred (not numberp)) 100)
      (`,alpha alpha)))

  (defun background-opacity-get-alpha-str ()
    (format "Alpha %s%%" (background-opacity-get-alpha))))

;; 配置输入法
(use-package rime
  :init
  (setq default-input-method "rime"
        rime-user-data-dir (concat thomas-cache-dir "rime/"))
  :config
  (setq rime-show-candidate 'posframe ; 使用 posframe 展示 candidate
        rime-posframe-properties (list :internal-border-width 5
                                       :left-fringe 3)
        ;; 关闭编码显示
        rime-show-preedit nil
        ;; 对任意无关函数自动上屏
        rime-commit1-forall t)

  ;; 使用 `rime-commit1-forall' 设置，不能使得在输入时按 ESC 时首项也自动上屏，这里给出针对性的配置方法。
  ;; 机制：在 emacs-rime 的输入状态下，ESC 被绑定到了 rime--escape 函数上，此函数显然不是“与 emacs-rime 无关的函数”。
  ;; 实例：以 evil 插件为例，若要使得 ESC 能自动上屏并切换到 evil 的 normal state，可以先自定义函数，并将此函数绑定到 ESC 上。
  ;;（注：至少对于 evil 插件而言， C-[ 和 ESC 被 emacs 当作同一按键，所以不需要对 C-[ 另行绑键。）
  (defun +rime-commit1-and-evil-normal ()
    "Commit the 1st item if exists, then go to evil normal state."
    (interactive)
    (rime-commit1)
    (evil-normal-state))

  ;; TODO 当前自动上屏功能不用
  (general-def rime-active-mode-map "<Esc>" '+rime-commit1-and-evil-normal)

  ;; 由于 `Emacs' 不能捕获 `shift' 修饰键，不能像 `windows' 下那样使用 `shift' 键来进入 `inline_mode',
  ;; 但是可以通过将 `shift' 键为重新映射到新的键上，如：shift -> C-\ 。
  ;; 但是这个和切换输入法项目没有任何优势，所以决定不开启 `inline_mode' 。
  ;; 使用 `toggle-input-method' 命令切换输入的方式来解决输入包含空格的长英文模式。
  ;; https://github.com/DogLooksGood/emacs-rime/issues/8
  (setq rime-disable-predicates '(rime-predicate-evil-mode-p ; 在英文字符串之后（必须为以字母开头的英文字符串）
                                  rime-predicate-ace-window-p ; 激活 ace-window-mode

                                  ;; 字符
                                  rime-predicate-after-alphabet-char-p ; 在英文字符串之后（必须为以字母开头的英文字符串）
                                  rime-predicate-punctuation-after-ascii-p ; 当要在任意英文字符之后输入符号时
                                  rime-predicate-current-uppercase-letter-p ; 将要输入的为大写字母时
                                  rime-predicate-space-after-cc-p ; 在中文字符且有空格之后

                                  ;; 代码
                                  rime-predicate-prog-in-code-p))) ; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
;; 在中英文字符之间添加空格。
(use-package pangu-spacing
  :hook ((text-mode . pangu-spacing-mode)
         ;; Always insert `real' space in org-mode.
         (org-mode . (lambda ()
                       (set (make-local-variable
                             'pangu-spacing-real-insert-separtor) t)))))

;; avy 拼音跳转支持
(use-package ace-pinyin
  :after avy
  :init (setq ace-pinyin-use-avy t)
  :custom-face (aw-leading-char-face ((t (:foreground "white" :background "#2a6041")
                                         :weight bold :height 2.5 :box (:line-width 10 :color "#2a6041"))))
  :config (ace-pinyin-global-mode t))

(use-package evil-pinyin
  :after evil
  :config
  (setq-default evil-pinyin-with-search-rule 'always)
  (global-evil-pinyin-mode 1))


;; 拼写检查
;; Interactive spell checker
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
  (setq ispell-skip-region-alist '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
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

(use-package persistent-scratch
  :init
  (setq persistent-scratch-save-file
        (expand-file-name ".persistent-scratch" thomas-cache-dir))
  :config
  (persistent-scratch-setup-default))


(provide 'init-editor)
;;; init-editor.el ends here
