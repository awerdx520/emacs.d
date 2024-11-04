;;; init-evil.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;
;;

;;; Code:
;;
;;; Undo
(use-package undo-fu
  :hook (after-init . undo-fu-mode)
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

  (define-minor-mode undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-_")     #'undo-fu-only-undo)
              (define-key map (kbd "M-_")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t))


(use-package undo-fu-session
  :hook (undo-fu-mode . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory (expand-file-name "undo-fu-session" thomas-cache-dir))
  :config
  (setq undo-fu-session-incompatible-files
        '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))

  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))

  (defun undo-fu-make-hashed-session-file-name-a (file)
    "HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
        filenames to prevent file paths that are too long, so we force
        `undo-fu-session--make-file-name' to use it instead of its own
        home-grown overly-long-filename generator.
        TODO PR this upstream; should be a universal issue"
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file)
              (undo-fu-session--file-name-ext)))
    (advice-add  'undo-fu-session--make-file-name
                 :override #'undo-fu-make-hashed-session-file-name-a)))

(use-package vundo
  :when (> emacs-major-version 27)
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t))

(use-package avy
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil))

(use-package evil
  :demand t
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-want-fine-undo t
        evil-want-C-g-bindings t
        evil-want-C-d-scroll t
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-abbrev-expand-on-insert-exit nil)

  (setq evil-disable-insert-state-bindings t
        evil-undo-system 'undo-fu) ; 由于 undo 更新频率很低，并且与 evil 存在兼容性问题

  ;; Switch to the new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-complete-emacs-commands nil
        evil-ex-interactive-search-highlight 'selected-window)

  (setq evil-respect-visual-line-mode t ; when `visual-line-mode' enabled, exchange j/k with gj/gk
        evil-symbol-word-search t)

  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-motion-state-map
         ("f" . evil-avy-goto-char-in-line)

         :map evil-insert-state-map
         ("C-e" . move-end-of-line)
         ("M-j" . yas-expand)
         :map evil-visual-state-map
         ("C-e" . move-end-of-line)

         :map evil-normal-state-map
         ("s" . evil-avy-goto-char-timer)
         ;; ("Y" . (kbd "y$"))
         ("C-e" . move-end-of-line)

         :map evil-ex-completion-map
         ("C-a" . move-beginning-of-line)
         ("C-b" . backward-char)
         ("C-f" . forward-char)
         ("M-n" . next-complete-history-element)
         ("M-p" . previous-complete-history-element))
  :general
  (thomas-leader-define
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
    "w|" 'evil-window-set-width )
  :config
  ;; Specify major mode uses EMACS original state.
  (dolist (p '((minibuffer-inactive-mode . emacs)
               (calendar-mode . emacs)
               (term-mode . emacs)
               (anaconda-nav-mode . emacs)
               (log-edit-mode . emacs)
               (vc-log-edit-mode . emacs)
               (magit-log-edit-mode . emacs)
               (vterm-mode . emacs)
               (lsp-bridge-ref-mode . emacs)
               (profiler-report-mode . emacs)))
    (evil-set-initial-state (car p) (cdr p))))

;; 使用 esc 键退出
(use-package evil-escape
  :diminish evil-escape-mode
  :hook (evil-mode . evil-escape-mode)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  :general
  (general-def :states '(insert replace visual operator) :keymaps 'global
    "\C-g" #'evil-escape)
  :config
  (defun +evil-inhibit-escape-in-minibuffer-fn ()
    (and (minibufferp)
         (or (not (bound-and-true-p evil-collection-setup-minibuffer))
             (evil-normal-state-p))))
  ;; `evil-escape' in the minibuffer is more disruptive than helpful. That is,
  ;; unless we have `evil-collection-setup-minibuffer' enabled, in which case we
  ;; want the same behavior in insert mode as we do in normal buffers.
  (add-hook 'evil-escape-inhibit-functions #'+evil-inhibit-escape-in-minibuffer-fn))

;; evil 默认键集合
(use-package evil-collection
  :after evil
  :demand t
  :bind (([remap evil-show-marks] . evil-collection-consult-mark)
         ([remap evil-show-jumps] . evil-collection-consult-jump-list))
  :init
  (setq evil-collection-setup-debugger-keys nil
        evil-collection-want-find-usages-bindings t
        evil-collection-calendar-want-org-bindings t)
  :config
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
  (evil-collection-init)

  (cl-loop for (mode . state) in
           '((org-agenda-mode . normal)
             (Custom-mode . emacs)
             (eshell-mode . emacs)
             (makey-key-mode . motion))
           do (evil-set-initial-state mode state)))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-nerd-commenter
  :bind ([remap comment-dwim] . evilnc-comment-or-uncomment-lines)
  :general
  (:states '(normal visual)
           "gcc" 'evilnc-comment-or-uncomment-lines
           "gcp" 'evilnc-comment-or-uncomment-paragraphs))

;; s: 2 char forward; S: 2 char backward
;; f: 1 char forward; F: 1 char backward
;; ;and, repeat search
(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :hook ((evil-mode . evil-snipe-mode)
         (evil-mode . evil-snipe-override-mode))
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (add-to-list 'evil-snipe-disabled-modes 'Info-mode nil #'eq))

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

;; proted numbert.vim,
;; 用于增加或减少当前点下的数据
(use-package evil-numbers
  :general
  (:states '(normal visual)
           "g=" 'evil-numbers/inc-at-pt
           "g-" 'evil-numbers/dec-at-pt))

;; 用于 Evil 的简易文本交换操作程序。 这是 Tom McDonald 的 vim-exchange 移植版本。
(use-package evil-exchange
  :config
  (evil-exchange-install))

;; 使用 *, # 进行前向后向搜索 visual 的值
(use-package evil-visualstar
  :hook (after-init . global-evil-visualstar-mode))

;; Evil 中用于分隔参数的Motion和文本对象。
(use-package evil-args
  :general
  ;; 替换 treesit 中定义的函数参数 textobject
  (:keymaps 'evil-inner-text-objects-map "a" 'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map "a" 'evil-outer-arg)
  ;; Motion
  (:states '(normal motion)
           "L" 'evil-forward-arg
           "H" 'evil-backward-arg)
  (:states '(normal) "K" 'evil-jump-out-args))

;;(use-package evil-mc)

;; https://github.com/nvim-treesitter/nvim-treesitter-textobjects#built-in-textobjects
(use-package evil-textobj-tree-sitter
  :after (evil treesit)
  :init
  (defun +tree-sitter-goto-textobj (group &optional previous end query)
    "Thin wrapper that returns the symbol of a named function, used in keybindings."
    (let ((sym (intern (format "+goto%s%s-%s" (if previous "-previous" "") (if end "-end" "") group))))
      (fset sym (lambda ()
                  (interactive)
                  (evil-textobj-tree-sitter-goto-textobj group previous end query)))
      sym))
  :general
  ;; Mapping textobjects
  (:keymaps 'evil-inner-text-objects-map
            ;; use evil-args 替代
            ;; "a" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.inner"))
            ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
            "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
            "F" (evil-textobj-tree-sitter-get-textobj "call.inner")
            "C" (evil-textobj-tree-sitter-get-textobj "class.inner")
            "v" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
            "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))

  (:keymaps 'evil-outer-text-objects-map
            ;; 用 evil-args 替代
            ;; "a" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer"))
            ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
            "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
            "F" (evil-textobj-tree-sitter-get-textobj "call.outer")
            "C" (evil-textobj-tree-sitter-get-textobj "class.outer")
            "c" (evil-textobj-tree-sitter-get-textobj "comment.outer")
            "v" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
            "l" (evil-textobj-tree-sitter-get-textobj "loop.outer")
            ;; Not Working
            "m" (evil-textobj-tree-sitter-get-textobj "import"
                  '((python-mode . [(import_statement) @import])
                    (rust-mode . [(use_declaration) @import]))))
  ;; Goto
  (:states '(normal) ; Previous
           "[ga" (+tree-sitter-goto-textobj "parameter.outer" t)
           "[gf" (+tree-sitter-goto-textobj "function.outer" t)
           "[gF" (+tree-sitter-goto-textobj "call.outer" t)
           "[gC" (+tree-sitter-goto-textobj "class.outer" t)
           "[gc" (+tree-sitter-goto-textobj "comment.outer" t)
           "[gv" (+tree-sitter-goto-textobj "conditional.outer" t)
           "[gl" (+tree-sitter-goto-textobj "loop.outer" t)
           )

  (:states '(normal) ; Next
           "]ga" (+tree-sitter-goto-textobj "parameter.outer")
           "]gf" (+tree-sitter-goto-textobj "function.outer")
           "]gF" (+tree-sitter-goto-textobj "call.outer")
           "]gC" (+tree-sitter-goto-textobj "class.outer")
           "]gc" (+tree-sitter-goto-textobj "comment.outer")
           "]gv" (+tree-sitter-goto-textobj "conditional.outer")
           "]gl" (+tree-sitter-goto-textobj "loop.outer")))

(with-eval-after-load 'which-key
  (setq which-key-allow-multiple-replacements t)
  (push '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1"))
        which-key-replacement-alist))




(provide 'init-evil)
;; init-evil ends here
