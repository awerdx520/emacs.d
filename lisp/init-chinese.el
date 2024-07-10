;; -*- lexical-binding: t -*-

(use-package rime
  :init
  (setq default-input-method "rime"
        rime-user-data-dir (concat thomas-cache-dir "rime/"))
  :general
  (:keymaps 'rime-active-mode-map
            "M-j" 'rime-inline-ascii)
  (:keymaps 'rime-mode-map
            "M-j" 'rime-force-enable
            "C-~" 'rime-send-keybinding)
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
  (defun rime-commit1-and-evil-normal ()
    "Commit the 1st item if exists, then go to evil normal state."
    (interactive)
    (rime-commit1)
    (evil-normal-state))

  ;; TODO 当前自动上屏功能不用
  (general-def rime-active-mode-map "<Esc>" 'rime-commit1-and-evil-normal)

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


(use-package pangu-spacing
  :hook ((text-mode . pangu-spacing-mode)
         ;; Always insert `real' space in org-mode.
         (org-mode . (lambda ()
                       (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))))

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

(provide 'init-chinese)
