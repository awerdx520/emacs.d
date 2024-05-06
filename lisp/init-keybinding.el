;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t
        evil-want-Y-yank-to-eol t)
  ;; 由于 undo 更新频率很低，并且与 evil 存在兼容性问题
  (setq evil-undo-system 'undo-fu)
  :hook (after-init . evil-mode)
  ;; Don't quit Emacs on `:q'.
  ;;
  ;; Rebind `f'/`s' to mimic `evil-snipe'.
  :bind (([remap evil-quit] . kill-this-buffer)
         :map evil-motion-state-map
         ("f" . evil-avy-goto-char-in-line)

         :map evil-normal-state-map
         ("s" . evil-avy-goto-char-timer))
  :config
  ;; Switch to the new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-complete-emacs-commands nil
        evil-ex-interactive-search-highlight 'selected-window)

  ;; when `visual-line-mode' enabled, exchange j/k with gj/gk
  (setq evil-respect-visual-line-mode t
        evil-want-integration t
        evil-want-keybinding nil
        evil-want-fine-undo t
        evil-want-C-g-bindings t
        evil-want-C-u-scroll t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-symbol-word-search t)
  :general
  (thomas-leader
    "w+" 'evil-window-increase-height
    "w-" 'evil-window-decrease-height
    "w:" 'evil-ex
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
    "C-d" 'evil-scroll-down
    "C-u" 'evil-scroll-up
    ;;
    ;;    "] t" '+evil/next-frame
    ;;   "[ t" '+evil/previous-frame
    "] f" '+evil/next-file
    "[ f" '+evil/previous-file))

;; evil 默认键集合
(use-package evil-collection
  :after evil
  :bind (([remap evil-show-marks] . evil-collection-consult-mark)
         ([remap evil-show-jumps] . evil-collection-consult-jump-list))
  :init
  (setq evil-collection-setup-debugger-keys nil
        evil-collection-want-find-usages-bindings t
        evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init))

;; 使用 esc 键退出
(use-package evil-escape
  :after evil
  :commands evil-escape
  :hook (after-init . evil-escape-mode)
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

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

(provide 'init-keybinding)
;;; init-evil.el ends here
