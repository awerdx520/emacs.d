;;; init-evil.el --- elisp -*- lexical-binding: t -*-
;;; Commentary:
;;
;;

;;; Code:

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

  ;; 为不同模式定义不同光标颜色，在 terminal 模式下不能使用
  (setq evil-emacs-state-cursor '("red" box)
        evil-normal-state-cursor '("green" box)
        evil-visual-state-cursor '("orange" box)
        evil-insert-state-cursor '("red" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))
  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-motion-state-map
         ("f" . evil-avy-goto-char-in-line)

         :map evil-insert-state-map
         ("C-e" . move-end-of-line)
         ;;("M-j" . yas-expand)
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

(provide 'init-evil)
;; init-evil ends here
