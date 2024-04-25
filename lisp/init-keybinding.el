;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-Y-yank-to-eol t)
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
   "w|" 'evil-window-set-width))


(use-package evil-collection
  :after evil
  :hook (after-init . evil-collection-init)
  :bind (([remap evil-show-marks] . evil-collection-consult-mark)
         ([remap evil-show-jumps] . evil-collection-consult-jump-list))
  :config
  (setq evil-collection-setup-debugger-keys nil
        evil-collection-calendar-want-org-bindings t
        evil-collection-unimpaired-want-repeat-mode-integration t))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))


(with-eval-after-load 'view
  (general-def :keymaps 'view-mode-map
    :states 'normal
    [escape] 'View-quit-all))

(with-eval-after-load 'man
  (general-def :keymaps 'Man-mode-map
    :states 'normal
    "q" 'kill-current-buffer))

(provide 'init-keybinding)
;;; init-evil.el ends here
